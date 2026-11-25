package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"math/big"
	"sort"

	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/ledger/byron"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

const (
	// byronMaxLovelace is maxLovelaceVal, 45e15.
	byronMaxLovelace = 45_000_000_000_000_000
	// byronNano is the scale of a Byron fee coefficient (Nano, E9).
	byronNano = 1_000_000_000
)

func byronRejection(rule, name string, err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: rule, Name: name, Detail: err.Error()}}}
}

// byronFeePolicy decodes TxSizeLinear a b into dingo's parameter shape. The
// reference holds a as Lovelace and b as a Rational, but its CBOR form is
// Nano, so a must be an integer in [0, maxLovelaceVal] and b a non-negative
// multiple of 1e-9; anything else is UNSUPPORTED on both nodes.
func byronFeePolicy(a, b *v1.Rational) (*eras.ByronProtocolParameters, error) {
	ra, err := ratArg("tx_fee_a", a)
	if err != nil {
		return nil, err
	}
	rb, err := ratArg("tx_fee_b", b)
	if err != nil {
		return nil, err
	}
	if !ra.IsInt() || ra.Sign() < 0 || ra.Num().Cmp(big.NewInt(byronMaxLovelace)) > 0 {
		return nil, dispatch.Unsupported(errors.New("tx_fee_a is not a Lovelace integer"))
	}
	nano := new(big.Rat).Mul(rb, big.NewRat(byronNano, 1))
	if rb.Sign() < 0 || !nano.IsInt() {
		return nil, dispatch.Unsupported(errors.New("tx_fee_b is not a non-negative Nano"))
	}
	return &eras.ByronProtocolParameters{
		TxFeeSummand:        ra.Num().Uint64(),
		TxFeeMultiplierNano: new(big.Int).Set(nano.Num()),
	}, nil
}

// byronMinFee maps to ByronProtocolParameters.MinFee (calculateTxSizeLinear).
// A fee outside the Lovelace domain is the rejection "LovelaceOverflow", the
// reference's addLovelace failure.
func byronMinFee(_ context.Context, req *v1.ByronMinFeeRequest) (*v1.ByronMinFeeResponse, error) {
	params, err := byronFeePolicy(req.GetTxFeeA(), req.GetTxFeeB())
	if err != nil {
		return nil, err
	}
	fee, err := params.MinFee(req.GetSize())
	if err != nil {
		return &v1.ByronMinFeeResponse{Rejection: byronRejection("byron-fee", "LovelaceOverflow", err)}, nil
	}
	return &v1.ByronMinFeeResponse{Fee: fee.Uint64()}, nil
}

// byronParams builds dingo's Byron parameters from the request. Only the
// fields a function reads are meaningful; the update-system thresholds are
// filled by the functions that need them.
func byronParams(pp *v1.ByronProtocolParameters) (*eras.ByronProtocolParameters, error) {
	params, err := byronFeePolicy(pp.GetTxFeeA(), pp.GetTxFeeB())
	if err != nil {
		return nil, err
	}
	params.MaxTxSize = new(big.Int).SetUint64(pp.GetMaxTxSize())
	params.MaxBlockSize = new(big.Int).SetUint64(pp.GetMaxBlockSize())
	params.MaxHeaderSize = new(big.Int).SetUint64(pp.GetMaxHeaderSize())
	return params, nil
}

// byronLedgerState is the narrow ledger view the Byron transaction rules read:
// the UTxO and the protocol magic. The embedded interface is nil, so any other
// capability would panic rather than answer silently.
type byronLedgerState struct {
	lcommon.LedgerState
	magic uint32
	utxo  map[string]lcommon.Utxo
}

func byronUtxoKey(id lcommon.Blake2b256, index uint32) string {
	return fmt.Sprintf("%x#%d", id[:], index)
}

func (s *byronLedgerState) UtxoById(in lcommon.TransactionInput) (lcommon.Utxo, error) {
	u, ok := s.utxo[byronUtxoKey(in.Id(), in.Index())]
	if !ok {
		return lcommon.Utxo{}, errors.New("utxo not found")
	}
	return u, nil
}

func (s *byronLedgerState) ByronProtocolMagic() (uint32, error) { return s.magic, nil }

func newByronLedgerState(magic uint32, entries []*v1.UtxoEntry) (*byronLedgerState, error) {
	ls := &byronLedgerState{magic: magic, utxo: make(map[string]lcommon.Utxo, len(entries))}
	for i, e := range entries {
		if len(e.GetTxin().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: tx_id is not 32 bytes", i))
		}
		out, err := byron.NewByronTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: %w", i, err))
		}
		in := shelley.ShelleyTransactionInput{OutputIndex: e.GetTxin().GetIndex()}
		copy(in.TxId[:], e.GetTxin().GetTxId())
		ls.utxo[byronUtxoKey(in.TxId, in.OutputIndex)] = lcommon.Utxo{Id: in, Output: out}
	}
	return ls, nil
}

// byronFailureName maps a dingo Byron error onto the reference constructor
// that reports the same condition: TxValidationError for the rules of
// validateTx and validateTxAux, UTxOError for an unresolved input, and
// DeserialiseFailure for what the reference refuses while decoding (an empty
// input or output list, an input index above Word16).
func byronFailureName(err error) string {
	var (
		inEmpty   eras.InputSetEmptyByronError
		inIndex   eras.InputIndexByronError
		outEmpty  eras.OutputSetEmptyByronError
		outZero   eras.OutputNotPositiveByronError
		outNeg    eras.OutputNegativeByronError
		tooLarge  eras.TxTooLargeByronError
		attrs     eras.UnknownAttributesByronError
		addrAttrs eras.UnknownAddressAttributesByronError
		lovelace  eras.LovelaceBoundByronError
		conserved eras.ValueNotConservedByronError
		feeLow    eras.FeeTooLowByronError
		wrongKey  eras.WitnessWrongKeyByronError
		badInputs eras.BadInputsByronError
		magic     eras.NetworkMagicMismatchByronError
		validErr  *lcommon.ValidationError
	)
	switch {
	case errors.As(err, &inEmpty), errors.As(err, &inIndex), errors.As(err, &outEmpty),
		errors.As(err, &outZero), errors.As(err, &outNeg):
		return "DeserialiseFailure"
	case errors.As(err, &tooLarge):
		return "TxValidationTxTooLarge"
	case errors.As(err, &attrs):
		return "TxValidationUnknownAttributes"
	case errors.As(err, &addrAttrs):
		return "TxValidationUnknownAddressAttributes"
	case errors.As(err, &lovelace), errors.As(err, &conserved):
		return "TxValidationLovelaceError"
	case errors.As(err, &feeLow):
		return "TxValidationFeeTooSmall"
	case errors.As(err, &wrongKey):
		return "TxValidationWitnessWrongKey"
	case errors.As(err, &badInputs):
		return "UTxOMissingInput"
	case errors.As(err, &magic):
		return "TxValidationNetworkMagicMismatch"
	case errors.As(err, &validErr):
		if validErr.Message == "invalid byron transaction witness" {
			return "DeserialiseFailure"
		}
		return "TxValidationWitnessWrongSignature"
	}
	return "DingoError"
}

// byronFailureRank is the order in which the reference reaches each failure:
// the UTxO lookup of the inputs' addresses, the witnesses, validateTxAux (size,
// then balances, then the fee comparison), and last validateTx. It is used
// only to choose which of dingo's joined failures to report.
var byronFailureRank = map[string]int{
	"DeserialiseFailure":                   0,
	"UTxOMissingInput":                     1,
	"TxValidationWitnessWrongSignature":    2,
	"TxValidationWitnessWrongKey":          3,
	"TxValidationTxTooLarge":               4,
	"TxValidationLovelaceError":            5,
	"TxValidationFeeTooSmall":              6,
	"TxValidationUnknownAttributes":        7,
	"TxValidationUnknownAddressAttributes": 8,
	"TxValidationNetworkMagicMismatch":     9,
}

// byronFailures reports the one failure the reference would reach first. The
// reference validates fail-fast (MonadError) while dingo joins every failing
// rule, so a transaction failing several rules would otherwise report names
// the reference never produces. A failure outside the table ranks last, so it
// is reported only when nothing else failed.
func byronFailures(rule string, err error) *v1.Rejection {
	var leaves []error
	var walk func(error)
	walk = func(e error) {
		if j, ok := e.(interface{ Unwrap() []error }); ok {
			for _, c := range j.Unwrap() {
				walk(c)
			}
			return
		}
		leaves = append(leaves, e)
	}
	walk(err)
	rank := func(name string) int {
		if r, ok := byronFailureRank[name]; ok {
			return r
		}
		return len(byronFailureRank)
	}
	best, bestName := leaves[0], byronFailureName(leaves[0])
	for _, l := range leaves[1:] {
		if n := byronFailureName(l); rank(n) < rank(bestName) {
			best, bestName = l, n
		}
	}
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: rule, Name: bestName, Detail: best.Error()}}}
}

// byronTxValidate maps to eras.ValidateTxByron over the request UTxO, then
// reports the UTxO delta of a valid transaction and its implicit fee.
func byronTxValidate(_ context.Context, req *v1.ByronTxValidateRequest) (*v1.ByronTxValidateResponse, error) {
	params, err := byronParams(req.GetPparams())
	if err != nil {
		return nil, err
	}
	ls, err := newByronLedgerState(req.GetProtocolMagic(), req.GetUtxo())
	if err != nil {
		return nil, err
	}
	tx, err := byron.NewByronTransactionFromCbor(req.GetTx())
	if err != nil {
		return &v1.ByronTxValidateResponse{Rejection: byronRejection("Byron.Tx", "DeserialiseFailure", err)}, nil
	}
	if err := eras.ValidateTxByron(tx, 0, ls, params); err != nil {
		return &v1.ByronTxValidateResponse{Rejection: byronFailures("Byron.Tx", err)}, nil
	}
	resp := &v1.ByronTxValidateResponse{}
	var consumed, produced uint64
	for _, in := range tx.Inputs() {
		id := in.Id()
		resp.UtxoRemoved = append(resp.UtxoRemoved, &v1.TxIn{TxId: id[:], Index: in.Index()})
		consumed += ls.utxo[byronUtxoKey(id, in.Index())].Output.Amount().Uint64()
	}
	txid := tx.Hash()
	for i, out := range tx.Outputs() {
		resp.UtxoAdded = append(resp.UtxoAdded, &v1.UtxoEntry{
			Txin:  &v1.TxIn{TxId: txid[:], Index: uint32(i)}, //nolint:gosec // bounded by the decoded output count
			Txout: out.Cbor(),
		})
		produced += out.Amount().Uint64()
	}
	resp.Fee = consumed - produced
	byronSortTxIns(resp.UtxoRemoved)
	byronSortEntries(resp.UtxoAdded)
	return resp, nil
}

func byronTxInLess(a, b *v1.TxIn) bool {
	if c := bytes.Compare(a.GetTxId(), b.GetTxId()); c != 0 {
		return c < 0
	}
	return a.GetIndex() < b.GetIndex()
}

func byronSortTxIns(in []*v1.TxIn) {
	sort.Slice(in, func(i, j int) bool { return byronTxInLess(in[i], in[j]) })
}

func byronSortEntries(es []*v1.UtxoEntry) {
	sort.Slice(es, func(i, j int) bool { return byronTxInLess(es[i].GetTxin(), es[j].GetTxin()) })
}

func registerByron(r *dispatch.Registry) {
	dispatch.Register(r, byronMinFee)
	dispatch.Register(r, byronTxValidate)
}
