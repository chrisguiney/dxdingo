package main

import (
	"context"
	"errors"
	"fmt"
	"math"
	"math/big"

	"github.com/blinklabs-io/dingo/ledger/eras"
	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Fee functions. Eras are Shelley through Conway: Byron has no such rules and
// Dijkstra is not modelled on either node, so both reply UNSUPPORTED.
//
// A result that does not fit a Word64 is UNSUPPORTED on both nodes. dingo
// saturates at the largest uint64 where the reference computes in unbounded
// integers, so a saturated result is treated as not representable.

var feeTxTypes = map[v1.Era]uint{
	v1.Era_ERA_SHELLEY: ledger.TxTypeShelley,
	v1.Era_ERA_ALLEGRA: ledger.TxTypeAllegra,
	v1.Era_ERA_MARY:    ledger.TxTypeMary,
	v1.Era_ERA_ALONZO:  ledger.TxTypeAlonzo,
	v1.Era_ERA_BABBAGE: ledger.TxTypeBabbage,
	v1.Era_ERA_CONWAY:  ledger.TxTypeConway,
}

// feeEra checks the era is within [lo, hi].
func feeEra(fn string, era, lo, hi v1.Era) error {
	if _, ok := feeTxTypes[era]; !ok || era < lo || era > hi {
		return dispatch.Unsupported(fmt.Errorf("%s: era %s is outside %s..%s", fn, era, lo, hi))
	}
	return nil
}

// deserialiseRejection is the Haskell DecoderError as a rejection.
func deserialiseRejection(rule string, err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: rule, Name: "DeserialiseFailure", Detail: err.Error()}}}
}

func decodeFeeTx(era v1.Era, raw []byte) (lcommon.Transaction, *v1.Rejection) {
	tx, err := ledger.NewTransactionFromCbor(feeTxTypes[era], raw)
	if err != nil {
		return nil, deserialiseRejection("tx", err)
	}
	return tx, nil
}

// feeParam returns a required optional protocol parameter.
func feeParam(name string, v *uint64) (uint64, error) {
	if v == nil {
		return 0, dispatch.BadRequest(fmt.Errorf("pparams.%s is required", name))
	}
	return *v, nil
}

// nonNegativeRat decodes a rational the reference holds as NonNegativeInterval.
func nonNegativeRat(name string, r *v1.Rational) (*big.Rat, error) {
	x, err := ratArg(name, r)
	if err != nil {
		return nil, err
	}
	if err := nonNegativeInterval(name, x); err != nil {
		return nil, err
	}
	return x, nil
}

// maxRefScriptSize bounds the reference-script size: the tiered fee loops once
// per 25600 bytes over a growing rational, so an absurd size would never
// finish on either node. Both reply UNSUPPORTED above it.
const maxRefScriptSize = 1 << 28

// saturated reports a dingo result that hit the uint64 ceiling.
func saturated(fn string, v uint64) error {
	if v == math.MaxUint64 {
		return dispatch.Unsupported(fmt.Errorf("%s: result does not fit a Word64", fn))
	}
	return nil
}

// feeTxSize maps to dingo eras.TxSizeForFee, the size dingo's fee and size
// rules use.
func feeTxSize(_ context.Context, req *v1.FeeTxSizeRequest) (*v1.FeeTxSizeResponse, error) {
	if err := feeEra("fee_tx_size", req.GetEra(), v1.Era_ERA_SHELLEY, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	tx, rej := decodeFeeTx(req.GetEra(), req.GetTx())
	if rej != nil {
		return &v1.FeeTxSizeResponse{Rejection: rej}, nil
	}
	return &v1.FeeTxSizeResponse{Size: eras.TxSizeForFee(tx)}, nil
}

// feeMinFee maps to dingo eras.MinFeeTx, the minimum fee ValidateTxFee and
// ValidateTxFeeConway compare against. The reference-script size is an input.
// Prices apply from Alonzo and the reference-script fee only in Conway; other
// eras ignore them, as getMinFeeTx does.
func feeMinFee(_ context.Context, req *v1.FeeMinFeeRequest) (*v1.FeeMinFeeResponse, error) {
	era := req.GetEra()
	if err := feeEra("fee_min_fee", era, v1.Era_ERA_SHELLEY, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	a, err := feeParam("min_fee_a", pp.MinFeeA)
	if err != nil {
		return nil, err
	}
	b, err := feeParam("min_fee_b", pp.MinFeeB)
	if err != nil {
		return nil, err
	}
	var priceMem, priceSteps, refCost *big.Rat
	if era >= v1.Era_ERA_ALONZO {
		if priceMem, err = nonNegativeRat("price_mem", pp.GetPriceMem()); err != nil {
			return nil, err
		}
		if priceSteps, err = nonNegativeRat("price_steps", pp.GetPriceSteps()); err != nil {
			return nil, err
		}
	}
	refSize := uint64(0)
	if era == v1.Era_ERA_CONWAY {
		if refCost, err = nonNegativeRat("min_fee_ref_script_cost_per_byte", pp.GetMinFeeRefScriptCostPerByte()); err != nil {
			return nil, err
		}
		refSize = req.GetRefScriptSize()
		if refSize > maxRefScriptSize {
			return nil, dispatch.Unsupported(errors.New("ref_script_size is above the modelled bound"))
		}
	}
	tx, rej := decodeFeeTx(era, req.GetTx())
	if rej != nil {
		return &v1.FeeMinFeeResponse{Rejection: rej}, nil
	}
	fee, err := eras.MinFeeTx(tx, uint(a), uint(b), priceMem, priceSteps, refSize, refCost) //nolint:gosec // uint is 64-bit
	if err != nil {
		return &v1.FeeMinFeeResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	if err := saturated("fee_min_fee", fee); err != nil {
		return nil, err
	}
	return &v1.FeeMinFeeResponse{MinFee: fee}, nil
}

// resolvedState answers UtxoById from the request's entries; dingo's
// reference-script size reads nothing else from the ledger state.
type resolvedState struct {
	lcommon.LedgerState
	utxos lcommon.ResolvedUtxos
}

func (s resolvedState) UtxoById(in lcommon.TransactionInput) (lcommon.Utxo, error) {
	return s.utxos.UtxoById(in)
}

func decodeUtxoEntries(entries []*v1.UtxoEntry) (lcommon.ResolvedUtxos, error) {
	utxos := make(lcommon.ResolvedUtxos, 0, len(entries))
	for i, e := range entries {
		var id shelley.ShelleyTransactionInput
		if len(e.GetTxin().GetTxId()) != lcommon.Blake2b256Size {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: tx_id is not 32 bytes", i))
		}
		copy(id.TxId[:], e.GetTxin().GetTxId())
		id.OutputIndex = e.GetTxin().GetIndex()
		out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
		if err != nil {
			return nil, dispatch.BadRequest(fmt.Errorf("utxo[%d]: %w", i, err))
		}
		utxos = append(utxos, lcommon.Utxo{Id: id, Output: out})
	}
	return utxos, nil
}

// feeRefScriptSize maps to dingo eras.ReferencedScriptSize, which resolves the
// spending and reference inputs and counts an input in both sets once.
func feeRefScriptSize(_ context.Context, req *v1.FeeRefScriptSizeRequest) (*v1.FeeRefScriptSizeResponse, error) {
	if err := feeEra("fee_ref_script_size", req.GetEra(), v1.Era_ERA_BABBAGE, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	utxos, err := decodeUtxoEntries(req.GetUtxo())
	if err != nil {
		return nil, err
	}
	tx, rej := decodeFeeTx(req.GetEra(), req.GetTx())
	if rej != nil {
		return &v1.FeeRefScriptSizeResponse{Rejection: rej}, nil
	}
	size, err := eras.ReferencedScriptSize(tx, resolvedState{utxos: utxos})
	if err != nil {
		return &v1.FeeRefScriptSizeResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	return &v1.FeeRefScriptSizeResponse{Size: size}, nil
}

// feeRefScriptFee maps to dingo eras.CalculateConwayRefScriptFee. The tier
// stride (25600) and multiplier (6/5) are Conway constants.
func feeRefScriptFee(_ context.Context, req *v1.FeeRefScriptFeeRequest) (*v1.FeeRefScriptFeeResponse, error) {
	if err := feeEra("fee_ref_script_fee", req.GetEra(), v1.Era_ERA_CONWAY, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	cost, err := nonNegativeRat("cost_per_byte", req.GetCostPerByte())
	if err != nil {
		return nil, err
	}
	if req.GetSize() > maxRefScriptSize {
		return nil, dispatch.Unsupported(errors.New("size is above the modelled bound"))
	}
	fee := eras.CalculateConwayRefScriptFee(req.GetSize(), cost)
	if err := saturated("fee_ref_script_fee", fee); err != nil {
		return nil, err
	}
	return &v1.FeeRefScriptFeeResponse{Fee: fee}, nil
}

// feeScriptFee maps to gouroboros common.ExecutionUnitsFee, the pricing step
// of CalculateExecutionUnitsFee. gouroboros holds execution units as int64;
// a larger value is reported as a dingo error, where the reference still
// answers.
func feeScriptFee(_ context.Context, req *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
	mem, err := nonNegativeRat("price_mem", req.GetPriceMem())
	if err != nil {
		return nil, err
	}
	steps, err := nonNegativeRat("price_steps", req.GetPriceSteps())
	if err != nil {
		return nil, err
	}
	if req.GetExMem() > math.MaxInt64 || req.GetExSteps() > math.MaxInt64 {
		return &v1.FeeScriptFeeResponse{Rejection: dingoRejection("fee", errors.New("execution units exceed int64"))}, nil
	}
	fee, err := lcommon.ExecutionUnitsFee(
		lcommon.ExUnitPrice{MemPrice: &cbor.Rat{Rat: mem}, StepPrice: &cbor.Rat{Rat: steps}},
		[]lcommon.ExUnits{{Memory: int64(req.GetExMem()), Steps: int64(req.GetExSteps())}}, //nolint:gosec // bounded above
	)
	if err != nil {
		return &v1.FeeScriptFeeResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	return &v1.FeeScriptFeeResponse{Fee: fee}, nil
}

// feeMinUtxo maps to the era's gouroboros MinCoinTxOut. Shelley, Allegra and
// Mary take the flat minUTxOValue; Alonzo prices UTxO words; Babbage and
// Conway price the serialised output.
func feeMinUtxo(_ context.Context, req *v1.FeeMinUtxoRequest) (*v1.FeeMinUtxoResponse, error) {
	era := req.GetEra()
	if err := feeEra("fee_min_utxo", era, v1.Era_ERA_SHELLEY, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	var param uint64
	var err error
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA, v1.Era_ERA_MARY:
		param, err = feeParam("min_utxo_value", pp.MinUtxoValue)
	case v1.Era_ERA_ALONZO:
		param, err = feeParam("coins_per_utxo_word", pp.CoinsPerUtxoWord)
	default:
		param, err = feeParam("coins_per_utxo_byte", pp.CoinsPerUtxoByte)
	}
	if err != nil {
		return nil, err
	}
	out, err := ledger.NewTransactionOutputFromCbor(req.GetTxout())
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("txout: %w", err))
	}
	var min uint64
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA:
		min, err = shelley.MinCoinTxOut(nil, &shelley.ShelleyProtocolParameters{MinUtxoValue: uint(param)}) //nolint:gosec // uint is 64-bit
	case v1.Era_ERA_MARY:
		min, err = mary.MinCoinTxOut(nil, &mary.MaryProtocolParameters{MinUtxoValue: uint(param)}) //nolint:gosec // uint is 64-bit
	case v1.Era_ERA_ALONZO:
		min, err = alonzo.MinCoinTxOut(out, &alonzo.AlonzoProtocolParameters{AdaPerUtxoByte: param})
	case v1.Era_ERA_BABBAGE:
		min, err = babbage.MinCoinTxOut(out, &babbage.BabbageProtocolParameters{AdaPerUtxoByte: param})
	default:
		min, err = conway.MinCoinTxOut(out, &conway.ConwayProtocolParameters{AdaPerUtxoByte: param})
	}
	if err != nil {
		return &v1.FeeMinUtxoResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	return &v1.FeeMinUtxoResponse{MinCoin: min}, nil
}

// feeScriptIntegrityHash maps to gouroboros common.ScriptDataHash, the hash
// the Alonzo through Conway script-data-hash rules compare against. When no
// redeemers, datums or languages are given nothing is hashed. Datums without
// redeemers hash the era's empty redeemers: an array through Babbage and a map
// in Conway, as those rules encode them.
func feeScriptIntegrityHash(_ context.Context, req *v1.FeeScriptIntegrityHashRequest) (*v1.FeeScriptIntegrityHashResponse, error) {
	era := req.GetEra()
	if err := feeEra("fee_script_integrity_hash", era, v1.Era_ERA_ALONZO, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	models := map[uint][]int64{}
	versions := map[uint]struct{}{}
	for _, l := range req.GetLanguages() {
		var key uint
		var model []int64
		switch l {
		case v1.PlutusVersion_PLUTUS_VERSION_V1:
			key, model = 0, pp.GetCostModelPlutusV1()
		case v1.PlutusVersion_PLUTUS_VERSION_V2:
			key, model = 1, pp.GetCostModelPlutusV2()
		case v1.PlutusVersion_PLUTUS_VERSION_V3:
			key, model = 2, pp.GetCostModelPlutusV3()
		default:
			return nil, dispatch.Unsupported(fmt.Errorf("language %s is not modelled", l))
		}
		versions[key] = struct{}{}
		if model != nil {
			models[key] = model
		}
	}
	redeemers, datums := req.GetRedeemers(), req.GetDatums()
	if len(redeemers) == 0 && len(datums) == 0 && len(versions) == 0 {
		return &v1.FeeScriptIntegrityHashResponse{}, nil
	}
	if len(redeemers) == 0 {
		redeemers = []byte{0x80}
		if era == v1.Era_ERA_CONWAY {
			redeemers = []byte{0xa0}
		}
	}
	h, err := lcommon.ScriptDataHash(redeemers, datums, versions, models)
	if err != nil {
		return &v1.FeeScriptIntegrityHashResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	return &v1.FeeScriptIntegrityHashResponse{Hash: h.Bytes()}, nil
}

func registerFee(r *dispatch.Registry) {
	dispatch.Register(r, feeMinFee)
	dispatch.Register(r, feeRefScriptSize)
	dispatch.Register(r, feeRefScriptFee)
	dispatch.Register(r, feeTxSize)
	dispatch.Register(r, feeScriptFee)
	dispatch.Register(r, feeMinUtxo)
	dispatch.Register(r, feeScriptIntegrityHash)
	dispatch.Register(r, feeValueBalance)
}

// balanceState is the ledger state ComputeValueBalance reads: UTxO lookup, the
// registered stake credentials with their deposits, and registered pools.
type balanceState struct {
	lcommon.LedgerState
	utxos    lcommon.ResolvedUtxos
	accounts map[balanceCred]uint64
	pools    map[lcommon.PoolKeyHash]*uint64
	epoch    uint64
	slot     uint64
}

type balanceCred struct {
	kind uint
	hash lcommon.Blake2b224
}

func (s *balanceState) UtxoById(in lcommon.TransactionInput) (lcommon.Utxo, error) {
	return s.utxos.UtxoById(in)
}

func (s *balanceState) IsStakeCredentialRegistered(c lcommon.Credential) bool {
	_, ok := s.accounts[balanceCred{c.CredType, c.Credential}]
	return ok
}

func (s *balanceState) StakeCredentialDeposit(c lcommon.Credential) (*uint64, error) {
	d, ok := s.accounts[balanceCred{c.CredType, c.Credential}]
	if !ok {
		return nil, nil
	}
	return &d, nil
}

// PoolCurrentState reports a registered pool with its pending retirement
// epoch, which is always after the context epoch.
func (s *balanceState) PoolCurrentState(p lcommon.PoolKeyHash) (*lcommon.PoolRegistrationCertificate, *uint64, error) {
	retiring, ok := s.pools[p]
	if !ok {
		return nil, nil, nil
	}
	return &lcommon.PoolRegistrationCertificate{Operator: p}, retiring, nil
}

func (s *balanceState) EpochForSlot(slot uint64) (uint64, error) {
	if slot != s.slot {
		return 0, fmt.Errorf("epoch of slot %d is not part of the request", slot)
	}
	return s.epoch, nil
}

func credentialKey(c *v1.Credential) (balanceCred, error) {
	var k balanceCred
	switch c.GetKind() {
	case v1.CredentialKind_CREDENTIAL_KIND_KEY:
		k.kind = lcommon.CredentialTypeAddrKeyHash
	case v1.CredentialKind_CREDENTIAL_KIND_SCRIPT:
		k.kind = lcommon.CredentialTypeScriptHash
	default:
		return k, dispatch.BadRequest(errors.New("credential kind is unspecified"))
	}
	if len(c.GetHash()) != lcommon.Blake2b224Size {
		return k, dispatch.BadRequest(errors.New("credential hash is not 28 bytes"))
	}
	copy(k.hash[:], c.GetHash())
	return k, nil
}

func assetList(in []conway.ValueBalanceAsset) ([]*v1.Asset, error) {
	out := make([]*v1.Asset, 0, len(in))
	for _, a := range in {
		if !a.Quantity.IsInt64() {
			return nil, dispatch.Unsupported(errors.New("fee_value_balance: asset quantity does not fit an Int64"))
		}
		out = append(out, &v1.Asset{PolicyId: a.Policy.Bytes(), AssetName: a.Name, Quantity: a.Quantity.Int64()})
	}
	return out, nil
}

// feeValueBalance maps to gouroboros conway.ComputeValueBalance, the sums the
// Conway value-conservation rule compares. Conway only.
//
// Deposit mismatches and unregistered credentials make dingo fail where the
// reference still returns amounts; they are reported as dingo errors.
func feeValueBalance(_ context.Context, req *v1.FeeValueBalanceRequest) (*v1.FeeValueBalanceResponse, error) {
	if err := feeEra("fee_value_balance", req.GetContext().GetEra(), v1.Era_ERA_CONWAY, v1.Era_ERA_CONWAY); err != nil {
		return nil, err
	}
	pp := req.GetPparams()
	keyDeposit, err := feeParam("key_deposit", pp.KeyDeposit)
	if err != nil {
		return nil, err
	}
	poolDeposit, err := feeParam("pool_deposit", pp.PoolDeposit)
	if err != nil {
		return nil, err
	}
	drepDeposit, err := feeParam("drep_deposit", pp.DrepDeposit)
	if err != nil {
		return nil, err
	}
	govDeposit, err := feeParam("gov_action_deposit", pp.GovActionDeposit)
	if err != nil {
		return nil, err
	}
	st := req.GetState()
	utxos, err := decodeUtxoEntries(st.GetUtxo())
	if err != nil {
		return nil, err
	}
	ls := &balanceState{
		utxos:    utxos,
		accounts: map[balanceCred]uint64{},
		pools:    map[lcommon.PoolKeyHash]*uint64{},
		epoch:    req.GetContext().GetEpoch(),
		slot:     req.GetContext().GetSlot(),
	}
	for _, a := range st.GetAccounts() {
		k, err := credentialKey(a.GetCredential())
		if err != nil {
			return nil, err
		}
		ls.accounts[k] = a.GetDeposit()
	}
	for _, p := range st.GetPools() {
		if p.GetStage() != v1.PoolStage_POOL_STAGE_CURRENT {
			continue
		}
		var id lcommon.PoolKeyHash
		if len(p.GetPoolId()) != lcommon.Blake2b224Size {
			return nil, dispatch.BadRequest(errors.New("pool_id is not 28 bytes"))
		}
		copy(id[:], p.GetPoolId())
		var retiring *uint64
		if p.RetiringEpoch != nil {
			r := p.GetRetiringEpoch()
			retiring = &r
		}
		ls.pools[id] = retiring
	}
	tx, rej := decodeFeeTx(v1.Era_ERA_CONWAY, req.GetTx())
	if rej != nil {
		return &v1.FeeValueBalanceResponse{Rejection: rej}, nil
	}
	params := &conway.ConwayProtocolParameters{
		KeyDeposit:       uint(keyDeposit),  //nolint:gosec // uint is 64-bit
		PoolDeposit:      uint(poolDeposit), //nolint:gosec // uint is 64-bit
		DRepDeposit:      drepDeposit,
		GovActionDeposit: govDeposit,
	}
	b, err := conway.ComputeValueBalance(tx, req.GetContext().GetSlot(), ls, params)
	if err != nil {
		return &v1.FeeValueBalanceResponse{Rejection: dingoRejection("fee", err)}, nil
	}
	for _, v := range []*big.Int{b.ConsumedCoin, b.ProducedCoin, b.Deposits, b.Refunds} {
		if !v.IsUint64() {
			return nil, dispatch.Unsupported(errors.New("fee_value_balance: coin total does not fit a Word64"))
		}
	}
	resp := &v1.FeeValueBalanceResponse{
		ConsumedCoin: b.ConsumedCoin.Uint64(),
		ProducedCoin: b.ProducedCoin.Uint64(),
		Deposits:     b.Deposits.Uint64(),
		Refunds:      b.Refunds.Uint64(),
	}
	if resp.ConsumedAssets, err = assetList(b.ConsumedAssets); err != nil {
		return nil, err
	}
	if resp.ProducedAssets, err = assetList(b.ProducedAssets); err != nil {
		return nil, err
	}
	return resp, nil
}
