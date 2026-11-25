package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"sort"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/allegra"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/conway"
	"github.com/blinklabs-io/gouroboros/ledger/mary"
	"github.com/blinklabs-io/gouroboros/ledger/shelley"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Codec functions: gouroboros decoders. A decode failure is
// Rejection{codec, DeserialiseFailure}; only accept versus reject is
// compared, never the failure text.

func registerCodec(r *dispatch.Registry) {
	dispatch.Register(r, codecTx)
	dispatch.Register(r, codecTxOutput)
	dispatch.Register(r, codecCertificate)
	dispatch.Register(r, codecGovernance)
	dispatch.Register(r, codecPlutusData)
	dispatch.Register(r, codecBlockBody)
	dispatch.Register(r, codecBlockHeader)
	dispatch.Register(r, codecByronBlock)
	dispatch.Register(r, codecPParamsUpdate)
}

// decodeFailure is the rejection a failed decode reports on both nodes.
func decodeFailure(err error) *v1.Rejection {
	return &v1.Rejection{Failures: []*v1.Failure{{Rule: "codec", Name: "DeserialiseFailure", Detail: err.Error()}}}
}

// codecEra describes a post-Byron era: the gouroboros transaction and block
// types, and the protocol versions its decoders serve (the reference's
// eraProtVerLow and eraProtVerHigh).
type codecEra struct {
	txType, blockType uint
	low, high         uint32
}

var codecEras = map[v1.Era]codecEra{
	v1.Era_ERA_SHELLEY: {ledger.TxTypeShelley, ledger.BlockTypeShelley, 2, 2},
	v1.Era_ERA_ALLEGRA: {ledger.TxTypeAllegra, ledger.BlockTypeAllegra, 3, 3},
	v1.Era_ERA_MARY:    {ledger.TxTypeMary, ledger.BlockTypeMary, 4, 4},
	v1.Era_ERA_ALONZO:  {ledger.TxTypeAlonzo, ledger.BlockTypeAlonzo, 5, 6},
	v1.Era_ERA_BABBAGE: {ledger.TxTypeBabbage, ledger.BlockTypeBabbage, 7, 8},
	v1.Era_ERA_CONWAY:  {ledger.TxTypeConway, ledger.BlockTypeConway, 9, 11},
}

// codecEraArg resolves the era and checks the protocol version lies in the
// era's range. An unset version means the era's own. Byron and Dijkstra are
// UNSUPPORTED.
func codecEraArg(era v1.Era, pv *v1.ProtocolVersion) (codecEra, error) {
	e, ok := codecEras[era]
	if !ok {
		return e, dispatch.Unsupported(fmt.Errorf("no codec for %v", era))
	}
	if major := pv.GetMajor(); major != 0 && (major < e.low || major > e.high) {
		return e, dispatch.Unsupported(fmt.Errorf("protocol version %d outside %v", major, era))
	}
	return e, nil
}

func codecTx(_ context.Context, req *v1.CodecTxRequest) (*v1.CodecTxResponse, error) {
	e, err := codecEraArg(req.GetEra(), req.GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	tx, err := ledger.NewTransactionFromCbor(e.txType, req.GetCbor())
	if err != nil {
		return &v1.CodecTxResponse{Rejection: decodeFailure(err)}, nil
	}
	size, err := common.TxSize(tx)
	if err != nil {
		return &v1.CodecTxResponse{Rejection: dingoRejection("codec", err)}, nil
	}
	id := tx.Hash()
	return &v1.CodecTxResponse{
		Cbor:    tx.Cbor(),
		TxId:    id[:],
		Size:    uint64(size),
		IsValid: tx.IsValid(),
	}, nil
}

func decodeTxOutput(era v1.Era, b []byte) (common.TransactionOutput, error) {
	switch era {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA:
		return ledger.NewShelleyTransactionOutputFromCbor(b)
	case v1.Era_ERA_MARY:
		return ledger.NewMaryTransactionOutputFromCbor(b)
	case v1.Era_ERA_ALONZO:
		return ledger.NewAlonzoTransactionOutputFromCbor(b)
	default:
		return ledger.NewBabbageTransactionOutputFromCbor(b)
	}
}

// codecTxOutput reports the address, coin, assets (sorted by policy and
// name), datum hash, inline datum and the script reference's hash.
func codecTxOutput(_ context.Context, req *v1.CodecTxOutputRequest) (*v1.CodecTxOutputResponse, error) {
	if _, err := codecEraArg(req.GetEra(), req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	out, err := decodeTxOutput(req.GetEra(), req.GetCbor())
	if err != nil {
		return &v1.CodecTxOutputResponse{Rejection: decodeFailure(err)}, nil
	}
	addr := out.Address()
	addrBytes, err := addr.Bytes()
	if err != nil {
		return &v1.CodecTxOutputResponse{Rejection: dingoRejection("codec", err)}, nil
	}
	resp := &v1.CodecTxOutputResponse{Cbor: out.Cbor(), Address: addrBytes}
	if amt := out.Amount(); amt != nil && amt.IsUint64() {
		resp.Coin = amt.Uint64()
	}
	if ma := out.Assets(); ma != nil {
		for _, pol := range ma.Policies() {
			for _, name := range ma.Assets(pol) {
				q := ma.Asset(pol, name)
				if !q.IsInt64() {
					return &v1.CodecTxOutputResponse{Rejection: dingoRejection("codec", errors.New("asset quantity exceeds int64"))}, nil
				}
				resp.Assets = append(resp.Assets, &v1.Asset{PolicyId: pol[:], AssetName: name, Quantity: q.Int64()})
			}
		}
		sort.Slice(resp.Assets, func(i, j int) bool {
			a, b := resp.Assets[i], resp.Assets[j]
			if c := bytes.Compare(a.PolicyId, b.PolicyId); c != 0 {
				return c < 0
			}
			return bytes.Compare(a.AssetName, b.AssetName) < 0
		})
	}
	// DatumHash() also reports the hash of an inline datum; the response
	// carries one or the other.
	if d := out.Datum(); d != nil && d.Data != nil {
		resp.InlineDatum = d.Cbor()
	} else if h := out.DatumHash(); h != nil {
		resp.DatumHash = h[:]
	}
	if s := out.ScriptRef(); s != nil {
		h := s.Hash()
		resp.ScriptRef = h[:]
	}
	return resp, nil
}

// certKinds maps the CDDL certificate tag to the reference's TxCert pattern
// name. Tags 0 and 1 are the deposit-less registration forms.
var certKinds = map[uint]string{
	0: "RegTxCert", 1: "UnRegTxCert", 2: "DelegStakeTxCert", 3: "RegPoolTxCert",
	4: "RetirePoolTxCert", 5: "GenesisDelegTxCert", 6: "MirTxCert",
	7: "RegDepositTxCert", 8: "UnRegDepositTxCert", 9: "DelegTxCert",
	10: "DelegTxCert", 11: "RegDepositDelegTxCert", 12: "RegDepositDelegTxCert",
	13: "RegDepositDelegTxCert", 14: "AuthCommitteeHotKeyTxCert",
	15: "ResignCommitteeColdTxCert", 16: "RegDRepTxCert", 17: "UnRegDRepTxCert",
	18: "UpdateDRepTxCert",
}

func codecCertificate(_ context.Context, req *v1.CodecCertificateRequest) (*v1.CodecCertificateResponse, error) {
	if _, err := codecEraArg(req.GetEra(), req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	var w common.CertificateWrapper
	if _, err := cbor.DecodeExact(req.GetCbor(), &w); err != nil {
		return &v1.CodecCertificateResponse{Rejection: decodeFailure(err)}, nil
	}
	if req.GetEra() != v1.Era_ERA_CONWAY {
		if err := common.ValidatePreConwayCertificateTypes([]common.CertificateWrapper{w}); err != nil {
			return &v1.CodecCertificateResponse{Rejection: decodeFailure(err)}, nil
		}
	}
	return &v1.CodecCertificateResponse{Cbor: w.Certificate.Cbor(), CertKind: certKinds[w.Type]}, nil
}

func codecGovernance(_ context.Context, req *v1.CodecGovernanceRequest) (*v1.CodecGovernanceResponse, error) {
	if req.GetEra() != v1.Era_ERA_CONWAY {
		return nil, dispatch.Unsupported(fmt.Errorf("governance items exist from Conway, not %v", req.GetEra()))
	}
	if _, err := codecEraArg(req.GetEra(), req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	switch req.GetKind() {
	case v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL:
		var p conway.ConwayProposalProcedure
		if _, err := cbor.DecodeExact(req.GetCbor(), &p); err != nil {
			return &v1.CodecGovernanceResponse{Rejection: decodeFailure(err)}, nil
		}
		enc, err := cbor.Encode(&p)
		if err != nil {
			return &v1.CodecGovernanceResponse{Rejection: dingoRejection("codec", err)}, nil
		}
		t := v1.GovActionType_GOV_ACTION_TYPE_UNSPECIFIED
		switch p.PPGovAction.Action.(type) {
		case *conway.ConwayParameterChangeGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_PARAMETER_CHANGE
		case *common.HardForkInitiationGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_HARD_FORK_INITIATION
		case *common.TreasuryWithdrawalGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_TREASURY_WITHDRAWALS
		case *common.NoConfidenceGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_NO_CONFIDENCE
		case *common.UpdateCommitteeGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_UPDATE_COMMITTEE
		case *common.NewConstitutionGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_NEW_CONSTITUTION
		case *common.InfoGovAction:
			t = v1.GovActionType_GOV_ACTION_TYPE_INFO
		}
		return &v1.CodecGovernanceResponse{Cbor: enc, ActionType: t}, nil
	case v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES:
		var vp common.VotingProcedures
		if _, err := cbor.DecodeExact(req.GetCbor(), &vp); err != nil {
			return &v1.CodecGovernanceResponse{Rejection: decodeFailure(err)}, nil
		}
		enc, err := cbor.Encode(&vp)
		if err != nil {
			return &v1.CodecGovernanceResponse{Rejection: dingoRejection("codec", err)}, nil
		}
		return &v1.CodecGovernanceResponse{Cbor: enc}, nil
	}
	return nil, dispatch.BadRequest(errors.New("governance kind unset"))
}

// codecPlutusData decodes as gouroboros decodes a datum (common.Datum, the
// path transaction and output datums take). A datum keeps its wire bytes, so
// the re-encoding is the input.
func codecPlutusData(_ context.Context, req *v1.CodecPlutusDataRequest) (*v1.CodecPlutusDataResponse, error) {
	var d common.Datum
	if err := d.UnmarshalCBOR(req.GetCbor()); err != nil {
		return &v1.CodecPlutusDataResponse{Rejection: decodeFailure(err)}, nil
	}
	enc, err := d.MarshalCBOR()
	if err != nil {
		return &v1.CodecPlutusDataResponse{Rejection: dingoRejection("codec", err)}, nil
	}
	return &v1.CodecPlutusDataResponse{Cbor: enc}, nil
}

// decodeBlock decodes a whole era block. Body-hash checking is skipped: the
// reference decoder does not check it, the BBODY rule does.
func decodeBlock(blockType uint, b []byte) (common.Block, error) {
	return ledger.NewBlockFromCbor(blockType, b, common.VerifyConfig{SkipBodyHashValidation: true})
}

// codecBlockBody takes a whole era block. It reports the transaction count
// and the body hash the header commits to.
func codecBlockBody(_ context.Context, req *v1.CodecBlockBodyRequest) (*v1.CodecBlockBodyResponse, error) {
	e, err := codecEraArg(req.GetEra(), req.GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	blk, err := decodeBlock(e.blockType, req.GetCbor())
	if err != nil {
		return &v1.CodecBlockBodyResponse{Rejection: decodeFailure(err)}, nil
	}
	h := blk.BlockBodyHash()
	return &v1.CodecBlockBodyResponse{
		Cbor:     blk.Cbor(),
		TxCount:  uint64(len(blk.Transactions())),
		BodyHash: h[:],
	}, nil
}

func codecBlockHeader(_ context.Context, req *v1.CodecBlockHeaderRequest) (*v1.CodecBlockHeaderResponse, error) {
	e, err := codecEraArg(req.GetEra(), req.GetProtocolVersion())
	if err != nil {
		return nil, err
	}
	hdr, err := ledger.NewBlockHeaderFromCbor(e.blockType, req.GetCbor())
	if err != nil {
		return &v1.CodecBlockHeaderResponse{Rejection: decodeFailure(err)}, nil
	}
	hash, prev, bodyHash, issuer := hdr.Hash(), hdr.PrevHash(), hdr.BlockBodyHash(), hdr.IssuerVkey()
	resp := &v1.CodecBlockHeaderResponse{
		Cbor:       hdr.Cbor(),
		HeaderHash: hash[:],
		Slot:       hdr.SlotNumber(),
		BlockNo:    hdr.BlockNumber(),
		IssuerVkey: issuer[:],
		BodySize:   hdr.BlockBodySize(),
		BodyHash:   bodyHash[:],
	}
	// The genesis successor's previous hash is absent; gouroboros reports
	// it as zero bytes.
	if prev != (common.Blake2b256{}) {
		resp.PrevHash = prev[:]
	}
	switch h := hdr.(type) {
	case *shelley.ShelleyBlockHeader:
		fillTpraosHeader(resp, h)
	case *allegra.AllegraBlockHeader:
		fillTpraosHeader(resp, &h.ShelleyBlockHeader)
	case *mary.MaryBlockHeader:
		fillTpraosHeader(resp, &h.ShelleyBlockHeader)
	case *alonzo.AlonzoBlockHeader:
		fillTpraosHeader(resp, &h.ShelleyBlockHeader)
	case *babbage.BabbageBlockHeader:
		fillPraosHeader(resp, h)
	case *conway.ConwayBlockHeader:
		fillPraosHeader(resp, &h.BabbageBlockHeader)
	}
	return resp, nil
}

func fillTpraosHeader(r *v1.CodecBlockHeaderResponse, h *shelley.ShelleyBlockHeader) {
	b := h.Body
	r.VrfVkey = b.VrfKey
	r.OpcertCounter = b.OpCertSequenceNumber
	r.KesPeriod = b.OpCertKesPeriod
	r.ProtocolVersion = &v1.ProtocolVersion{Major: uint32(b.ProtoMajorVersion), Minor: uint32(b.ProtoMinorVersion)}
}

func fillPraosHeader(r *v1.CodecBlockHeaderResponse, h *babbage.BabbageBlockHeader) {
	b := h.Body
	r.VrfVkey = b.VrfKey
	r.OpcertCounter = b.OpCert.SequenceNumber
	r.KesPeriod = b.OpCert.KesPeriod
	r.ProtocolVersion = &v1.ProtocolVersion{Major: uint32(b.ProtoVersion.Major), Minor: uint32(b.ProtoVersion.Minor)}
}

// codecByronBlock takes the ABlockOrBoundary wire form [tag, block], tag 0
// for an epoch boundary block and 1 for a main block.
func codecByronBlock(_ context.Context, req *v1.CodecByronBlockRequest) (*v1.CodecByronBlockResponse, error) {
	var w struct {
		cbor.StructAsArray
		Type uint
		Raw  cbor.RawMessage
	}
	if _, err := cbor.DecodeExact(req.GetCbor(), &w); err != nil {
		return &v1.CodecByronBlockResponse{Rejection: decodeFailure(err)}, nil
	}
	if w.Type != ledger.BlockTypeByronEbb && w.Type != ledger.BlockTypeByronMain {
		return &v1.CodecByronBlockResponse{Rejection: decodeFailure(fmt.Errorf("byron block tag %d", w.Type))}, nil
	}
	blk, err := decodeBlock(w.Type, w.Raw)
	if err != nil {
		return &v1.CodecByronBlockResponse{Rejection: decodeFailure(err)}, nil
	}
	hash := blk.Hash()
	resp := &v1.CodecByronBlockResponse{
		Cbor:       req.GetCbor(),
		IsBoundary: w.Type == ledger.BlockTypeByronEbb,
		HeaderHash: hash[:],
	}
	switch b := blk.(type) {
	case *ledger.ByronMainBlock:
		resp.Epoch = b.BlockHeader.ConsensusData.SlotId.Epoch
		resp.Slot = b.BlockHeader.ConsensusData.SlotId.Slot
	case *ledger.ByronEpochBoundaryBlock:
		resp.Epoch = b.BlockHeader.ConsensusData.Epoch
	}
	return resp, nil
}

// pparamsOut accumulates the fields an update sets. The optional proto
// fields stay unset for parameters the update leaves out.
type pparamsOut struct{ p *v1.ProtocolParameters }

func u64(x *uint) *uint64 {
	if x == nil {
		return nil
	}
	v := uint64(*x)
	return &v
}

func u32(x *uint) *uint32 {
	if x == nil {
		return nil
	}
	v := uint32(*x)
	return &v
}

func ratOf(r *cbor.Rat) *v1.Rational {
	if r == nil || r.Rat == nil {
		return nil
	}
	return convert.RatProto(r.Rat)
}

func (o pparamsOut) base(
	minA, minB, bodySize, txSize, headerSize, keyDep, poolDep, maxEpoch, nOpt *uint,
	a0, rho, tau *cbor.Rat, minPoolCost *uint64,
) {
	o.p.MinFeeA, o.p.MinFeeB = u64(minA), u64(minB)
	o.p.MaxBlockBodySize, o.p.MaxTxSize, o.p.MaxBlockHeaderSize = u32(bodySize), u32(txSize), u32(headerSize)
	o.p.KeyDeposit, o.p.PoolDeposit, o.p.MaxEpoch, o.p.NOpt = u64(keyDep), u64(poolDep), u64(maxEpoch), u32(nOpt)
	o.p.A0, o.p.Rho, o.p.Tau = ratOf(a0), ratOf(rho), ratOf(tau)
	o.p.MinPoolCost = minPoolCost
}

func (o pparamsOut) shelleyExtras(d *cbor.Rat, ee *common.Nonce, pv *common.ProtocolParametersProtocolVersion) {
	o.p.D = ratOf(d)
	if ee != nil {
		if ee.Type == 0 {
			o.p.ExtraEntropy = &v1.Nonce{}
		} else {
			o.p.ExtraEntropy = &v1.Nonce{Hash: ee.Value[:]}
		}
	}
	o.protocolVersion(pv)
}

func (o pparamsOut) protocolVersion(pv *common.ProtocolParametersProtocolVersion) {
	if pv != nil {
		o.p.ProtocolVersion = &v1.ProtocolVersion{Major: uint32(pv.Major), Minor: uint32(pv.Minor)}
	}
}

func (o pparamsOut) plutus(
	models map[uint][]int64, prices *common.ExUnitPrice, txEx, blockEx *common.ExUnits,
	valueSize, collateralPct, collateralInputs *uint,
) {
	o.p.CostModelPlutusV1 = models[0]
	o.p.CostModelPlutusV2 = models[1]
	o.p.CostModelPlutusV3 = models[2]
	o.p.CostModelPlutusV4 = models[3]
	if prices != nil {
		o.p.PriceMem, o.p.PriceSteps = ratOf(prices.MemPrice), ratOf(prices.StepPrice)
	}
	if txEx != nil {
		m, s := uint64(txEx.Memory), uint64(txEx.Steps)
		o.p.MaxTxExMem, o.p.MaxTxExSteps = &m, &s
	}
	if blockEx != nil {
		m, s := uint64(blockEx.Memory), uint64(blockEx.Steps)
		o.p.MaxBlockExMem, o.p.MaxBlockExSteps = &m, &s
	}
	o.p.MaxValueSize, o.p.CollateralPercentage, o.p.MaxCollateralInputs = u32(valueSize), u32(collateralPct), u32(collateralInputs)
}

// codecPParamsUpdate decodes an era's protocol-parameter update and reports
// only the parameters it sets.
func codecPParamsUpdate(_ context.Context, req *v1.CodecPParamsUpdateRequest) (*v1.CodecPParamsUpdateResponse, error) {
	if _, err := codecEraArg(req.GetEra(), req.GetProtocolVersion()); err != nil {
		return nil, err
	}
	p := &v1.ProtocolParameters{}
	o := pparamsOut{p}
	var enc []byte
	var err error
	switch req.GetEra() {
	case v1.Era_ERA_SHELLEY, v1.Era_ERA_ALLEGRA:
		var u shelley.ShelleyProtocolParameterUpdate
		if _, err = cbor.DecodeExact(req.GetCbor(), &u); err == nil {
			o.base(u.MinFeeA, u.MinFeeB, u.MaxBlockBodySize, u.MaxTxSize, u.MaxBlockHeaderSize, u.KeyDeposit, u.PoolDeposit, u.MaxEpoch, u.NOpt, u.A0, u.Rho, u.Tau, u.MinPoolCost)
			o.shelleyExtras(u.Decentralization, u.ExtraEntropy, u.ProtocolVersion)
			p.MinUtxoValue = u64(u.MinUtxoValue)
			enc = u.Cbor()
		}
	case v1.Era_ERA_MARY:
		var u mary.MaryProtocolParameterUpdate
		if _, err = cbor.DecodeExact(req.GetCbor(), &u); err == nil {
			o.base(u.MinFeeA, u.MinFeeB, u.MaxBlockBodySize, u.MaxTxSize, u.MaxBlockHeaderSize, u.KeyDeposit, u.PoolDeposit, u.MaxEpoch, u.NOpt, u.A0, u.Rho, u.Tau, u.MinPoolCost)
			o.shelleyExtras(u.Decentralization, u.ExtraEntropy, u.ProtocolVersion)
			p.MinUtxoValue = u64(u.MinUtxoValue)
			enc = u.Cbor()
		}
	case v1.Era_ERA_ALONZO:
		var u alonzo.AlonzoProtocolParameterUpdate
		if _, err = cbor.DecodeExact(req.GetCbor(), &u); err == nil {
			o.base(u.MinFeeA, u.MinFeeB, u.MaxBlockBodySize, u.MaxTxSize, u.MaxBlockHeaderSize, u.KeyDeposit, u.PoolDeposit, u.MaxEpoch, u.NOpt, u.A0, u.Rho, u.Tau, u.MinPoolCost)
			o.shelleyExtras(u.Decentralization, u.ExtraEntropy, u.ProtocolVersion)
			p.CoinsPerUtxoWord = u.AdaPerUtxoByte
			o.plutus(u.CostModels, u.ExecutionCosts, u.MaxTxExUnits, u.MaxBlockExUnits, u.MaxValueSize, u.CollateralPercentage, u.MaxCollateralInputs)
			enc = u.Cbor()
		}
	case v1.Era_ERA_BABBAGE:
		var u babbage.BabbageProtocolParameterUpdate
		if _, err = cbor.DecodeExact(req.GetCbor(), &u); err == nil {
			o.base(u.MinFeeA, u.MinFeeB, u.MaxBlockBodySize, u.MaxTxSize, u.MaxBlockHeaderSize, u.KeyDeposit, u.PoolDeposit, u.MaxEpoch, u.NOpt, u.A0, u.Rho, u.Tau, u.MinPoolCost)
			o.protocolVersion(u.ProtocolVersion)
			p.CoinsPerUtxoByte = u.AdaPerUtxoByte
			o.plutus(u.CostModels, u.ExecutionCosts, u.MaxTxExUnits, u.MaxBlockExUnits, u.MaxValueSize, u.CollateralPercentage, u.MaxCollateralInputs)
			enc = u.Cbor()
		}
	case v1.Era_ERA_CONWAY:
		var u conway.ConwayProtocolParameterUpdate
		if _, err = cbor.DecodeExact(req.GetCbor(), &u); err == nil {
			o.base(u.MinFeeA, u.MinFeeB, u.MaxBlockBodySize, u.MaxTxSize, u.MaxBlockHeaderSize, u.KeyDeposit, u.PoolDeposit, u.MaxEpoch, u.NOpt, u.A0, u.Rho, u.Tau, u.MinPoolCost)
			o.protocolVersion(u.ProtocolVersion)
			p.CoinsPerUtxoByte = u.AdaPerUtxoByte
			o.plutus(u.CostModels, u.ExecutionCosts, u.MaxTxExUnits, u.MaxBlockExUnits, u.MaxValueSize, u.CollateralPercentage, u.MaxCollateralInputs)
			if t := u.PoolVotingThresholds; t != nil {
				p.PoolVotingThresholds = &v1.PoolVotingThresholds{
					MotionNoConfidence: ratOf(&t.MotionNoConfidence), CommitteeNormal: ratOf(&t.CommitteeNormal),
					CommitteeNoConfidence: ratOf(&t.CommitteeNoConfidence), HardForkInitiation: ratOf(&t.HardForkInitiation),
					PpSecurityGroup: ratOf(&t.PpSecurityGroup),
				}
			}
			if t := u.DRepVotingThresholds; t != nil {
				p.DrepVotingThresholds = &v1.DRepVotingThresholds{
					MotionNoConfidence: ratOf(&t.MotionNoConfidence), CommitteeNormal: ratOf(&t.CommitteeNormal),
					CommitteeNoConfidence: ratOf(&t.CommitteeNoConfidence), UpdateToConstitution: ratOf(&t.UpdateToConstitution),
					HardForkInitiation: ratOf(&t.HardForkInitiation), PpNetworkGroup: ratOf(&t.PpNetworkGroup),
					PpEconomicGroup: ratOf(&t.PpEconomicGroup), PpTechnicalGroup: ratOf(&t.PpTechnicalGroup),
					PpGovGroup: ratOf(&t.PpGovGroup), TreasuryWithdrawal: ratOf(&t.TreasuryWithdrawal),
				}
			}
			p.CommitteeMinSize = u32(u.MinCommitteeSize)
			p.CommitteeMaxTermLength, p.GovActionLifetime, p.GovActionDeposit = u.CommitteeTermLimit, u.GovActionValidityPeriod, u.GovActionDeposit
			p.DrepDeposit, p.DrepActivity = u.DRepDeposit, u.DRepInactivityPeriod
			p.MinFeeRefScriptCostPerByte = ratOf(u.MinFeeRefScriptCostPerByte)
			enc = u.Cbor()
		}
	}
	if err != nil {
		return &v1.CodecPParamsUpdateResponse{Rejection: decodeFailure(err)}, nil
	}
	return &v1.CodecPParamsUpdateResponse{Cbor: enc, Update: p}, nil
}
