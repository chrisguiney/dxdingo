// Package rulefixture builds rule-function requests from real transactions.
//
// The block files in testdata are copies of gouroboros internal/testdata
// (mainnet blocks). A real transaction's witnesses are signed for input
// addresses the block does not contain, so the UTxO tables are synthesised:
// every input becomes an enterprise address of one of the transaction's own
// vkey witnesses, holding enough lovelace to balance the transaction. The
// transaction is then valid under the rules that need no certificate or
// script state, and tests mutate the request to make it fail one rule at a time.
package rulefixture

import (
	"embed"
	"encoding/hex"
	"fmt"
	"math/big"
	"strings"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/alonzo"
	"github.com/blinklabs-io/gouroboros/ledger/babbage"
	"github.com/blinklabs-io/gouroboros/ledger/common"
	"github.com/blinklabs-io/gouroboros/ledger/mary"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

//go:embed testdata/*.hex
var blocks embed.FS

func rat(n, d string) *v1.Rational { return &v1.Rational{Numerator: n, Denominator: d} }

func u64(x uint64) *uint64 { return &x }
func u32(x uint32) *uint32 { return &x }

// Env returns a mainnet-shaped context, globals and parameters for the era.
func Env(era v1.Era) (*v1.LedgerContext, *v1.Globals, *v1.ProtocolParameters) {
	pv := map[v1.Era]uint32{v1.Era_ERA_ALONZO: 6, v1.Era_ERA_BABBAGE: 8, v1.Era_ERA_CONWAY: 10}[era]
	ctx := &v1.LedgerContext{Era: era, ProtocolVersion: &v1.ProtocolVersion{Major: pv}, Slot: 100_000_000, Epoch: 600, NetworkId: 1}
	g := &v1.Globals{
		NetworkMagic: 764824073, EpochLength: 432000, SlotLength: rat("1", "1"), SecurityParam: 2160,
		ActiveSlotCoeff: rat("1", "20"), SlotsPerKesPeriod: 129600, MaxKesEvolutions: 62,
		MaxLovelaceSupply: 45_000_000_000_000_000, SystemStartUnix: 1506203091, Quorum: 5,
	}
	pp := &v1.ProtocolParameters{
		MinFeeA: u64(44), MinFeeB: u64(155381), MaxBlockBodySize: u32(90112), MaxTxSize: u32(16384),
		MaxBlockHeaderSize: u32(1100), KeyDeposit: u64(2_000_000), PoolDeposit: u64(500_000_000),
		MaxEpoch: u64(18), NOpt: u32(500), A0: rat("3", "10"), Rho: rat("3", "1000"), Tau: rat("2", "10"),
		ProtocolVersion: &v1.ProtocolVersion{Major: pv}, MinPoolCost: u64(170_000_000),
		PriceMem: rat("577", "10000"), PriceSteps: rat("721", "10000000"),
		MaxTxExMem: u64(14_000_000), MaxTxExSteps: u64(10_000_000_000),
		MaxBlockExMem: u64(62_000_000), MaxBlockExSteps: u64(20_000_000_000),
		MaxValueSize: u32(5000), CollateralPercentage: u32(150), MaxCollateralInputs: u32(3),
	}
	switch era {
	case v1.Era_ERA_ALONZO:
		pp.D = rat("0", "1")
		pp.CoinsPerUtxoWord = u64(34482)
	case v1.Era_ERA_BABBAGE:
		pp.CoinsPerUtxoByte = u64(4310)
	case v1.Era_ERA_CONWAY:
		pp.CoinsPerUtxoByte = u64(4310)
		pp.PoolVotingThresholds = &v1.PoolVotingThresholds{
			MotionNoConfidence: rat("51", "100"), CommitteeNormal: rat("51", "100"), CommitteeNoConfidence: rat("51", "100"),
			HardForkInitiation: rat("51", "100"), PpSecurityGroup: rat("51", "100"),
		}
		d := rat("2", "3")
		pp.DrepVotingThresholds = &v1.DRepVotingThresholds{
			MotionNoConfidence: d, CommitteeNormal: d, CommitteeNoConfidence: d, UpdateToConstitution: d,
			HardForkInitiation: d, PpNetworkGroup: d, PpEconomicGroup: d, PpTechnicalGroup: d, PpGovGroup: d, TreasuryWithdrawal: d,
		}
		pp.CommitteeMinSize = u32(7)
		pp.CommitteeMaxTermLength = u64(146)
		pp.GovActionLifetime = u64(6)
		pp.GovActionDeposit = u64(100_000_000_000)
		pp.DrepDeposit = u64(500_000_000)
		pp.DrepActivity = u64(20)
		pp.MinFeeRefScriptCostPerByte = rat("15", "1")
	}
	return ctx, g, pp
}

// BlockTxs returns the era's transaction CBORs from the embedded block.
func BlockTxs(era v1.Era) ([][]byte, error) {
	name, typ := "", uint(0)
	switch era {
	case v1.Era_ERA_ALONZO:
		name, typ = "alonzo_block.hex", ledger.BlockTypeAlonzo
	case v1.Era_ERA_BABBAGE:
		name, typ = "babbage_block.hex", ledger.BlockTypeBabbage
	case v1.Era_ERA_CONWAY:
		name, typ = "conway_block.hex", ledger.BlockTypeConway
	default:
		return nil, fmt.Errorf("no block for %v", era)
	}
	raw, err := blocks.ReadFile("testdata/" + name)
	if err != nil {
		return nil, err
	}
	data, err := hex.DecodeString(strings.TrimSpace(string(raw)))
	if err != nil {
		return nil, err
	}
	blk, err := ledger.NewBlockFromCbor(typ, data)
	if err != nil {
		return nil, err
	}
	var out [][]byte
	for _, tx := range blk.Transactions() {
		out = append(out, tx.Cbor())
	}
	return out, nil
}

// Plain reports whether the transaction needs nothing beyond key witnesses:
// no scripts, redeemers, minting, certificates, withdrawals or collateral.
// Those rules read ledger state the synthesised tables do not carry.
func Plain(era v1.Era, txCbor []byte) bool {
	tx, err := ledger.NewTransactionFromCbor(TxType(era), txCbor)
	if err != nil {
		return false
	}
	w := tx.Witnesses()
	return len(tx.Collateral()) == 0 && len(tx.Certificates()) == 0 && len(tx.Withdrawals()) == 0 &&
		len(tx.ReferenceInputs()) == 0 && len(w.NativeScripts()) == 0 && len(w.PlutusV1Scripts()) == 0 &&
		len(w.PlutusV2Scripts()) == 0 && len(w.PlutusV3Scripts()) == 0 && len(w.PlutusData()) == 0 &&
		len(w.Bootstrap()) == 0 && tx.AssetMint() == nil && tx.ScriptDataHash() == nil
}

// TxType maps an era to its gouroboros transaction type.
func TxType(era v1.Era) uint {
	switch era {
	case v1.Era_ERA_ALONZO:
		return ledger.TxTypeAlonzo
	case v1.Era_ERA_BABBAGE:
		return ledger.TxTypeBabbage
	}
	return ledger.TxTypeConway
}

// State synthesises the UTxO table for tx so its inputs hold exactly
// outputs + fee lovelace (and the outputs' assets) between them.
func State(era v1.Era, networkID uint8, txCbor []byte) (*v1.LedgerState, error) {
	tx, err := ledger.NewTransactionFromCbor(TxType(era), txCbor)
	if err != nil {
		return nil, err
	}
	var keys []common.Blake2b224
	for _, w := range tx.Witnesses().Vkey() {
		keys = append(keys, common.Blake2b224Hash(w.Vkey))
	}
	if len(keys) == 0 {
		return nil, fmt.Errorf("transaction has no vkey witnesses")
	}
	total := new(big.Int).Set(tx.Fee())
	merged := map[common.Blake2b224]map[cbor.ByteString]*big.Int{}
	for _, o := range tx.Outputs() {
		total.Add(total, o.Amount())
		if a := o.Assets(); a != nil {
			for _, p := range a.Policies() {
				for _, n := range a.Assets(p) {
					if merged[p] == nil {
						merged[p] = map[cbor.ByteString]*big.Int{}
					}
					k := cbor.NewByteString(n)
					if merged[p][k] == nil {
						merged[p][k] = new(big.Int)
					}
					merged[p][k].Add(merged[p][k], a.Asset(p, n))
				}
			}
		}
	}
	inputs := tx.Inputs()
	st := &v1.LedgerState{Pots: &v1.Pots{Treasury: 1_000_000_000_000}}
	remaining := total
	for i, in := range inputs {
		amt := big.NewInt(5_000_000)
		if i == len(inputs)-1 || remaining.Cmp(amt) <= 0 {
			amt = new(big.Int).Set(remaining)
		}
		remaining = new(big.Int).Sub(remaining, amt)
		var ma *common.MultiAsset[common.MultiAssetTypeOutput]
		if i == 0 && len(merged) > 0 {
			m := common.NewMultiAsset[*big.Int](merged)
			ma = &m
		}
		addr, err := common.NewAddressFromParts(common.AddressTypeKeyNone, networkID, keys[i%len(keys)].Bytes(), nil)
		if err != nil {
			return nil, err
		}
		out, err := encodeOutput(era, addr, amt.Uint64(), ma)
		if err != nil {
			return nil, err
		}
		st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: in.Id().Bytes(), Index: in.Index()}, Txout: out})
	}
	// Collateral and reference inputs are plain ada outputs; the collateral is
	// large enough for the collateral percentage the tx would need.
	for _, in := range tx.Collateral() {
		out, err := encodeOutput(era, addrFor(keys[0], networkID), 50_000_000, nil)
		if err != nil {
			return nil, err
		}
		st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: in.Id().Bytes(), Index: in.Index()}, Txout: out})
	}
	for _, in := range tx.ReferenceInputs() {
		out, err := encodeOutput(era, addrFor(keys[0], networkID), 2_000_000, nil)
		if err != nil {
			return nil, err
		}
		st.Utxo = append(st.Utxo, &v1.UtxoEntry{Txin: &v1.TxIn{TxId: in.Id().Bytes(), Index: in.Index()}, Txout: out})
	}
	return st, nil
}

func addrFor(key common.Blake2b224, networkID uint8) common.Address {
	a, err := common.NewAddressFromParts(common.AddressTypeKeyNone, networkID, key.Bytes(), nil)
	if err != nil {
		panic(err)
	}
	return a
}

// SlotFor returns a slot inside the transaction's validity interval.
func SlotFor(era v1.Era, txCbor []byte) (uint64, error) {
	tx, err := ledger.NewTransactionFromCbor(TxType(era), txCbor)
	if err != nil {
		return 0, err
	}
	if ttl := tx.TTL(); ttl > 0 {
		return ttl - 1, nil
	}
	return tx.ValidityIntervalStart() + 1, nil
}

func encodeOutput(era v1.Era, addr common.Address, coin uint64, ma *common.MultiAsset[common.MultiAssetTypeOutput]) ([]byte, error) {
	val := mary.MaryTransactionOutputValue{Amount: coin, Assets: ma}
	if era == v1.Era_ERA_ALONZO {
		return cbor.Encode(&alonzo.AlonzoTransactionOutput{OutputAddress: addr, OutputAmount: val})
	}
	return cbor.Encode(&babbage.BabbageTransactionOutput{OutputAddress: addr, OutputAmount: val})
}

// Rebalance returns the entry with its lovelace changed by delta.
func Rebalance(era v1.Era, e *v1.UtxoEntry, delta int64) *v1.UtxoEntry {
	out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
	if err != nil {
		panic(err)
	}
	coin := new(big.Int).Add(out.Amount(), big.NewInt(delta)).Uint64()
	var ma *common.MultiAsset[common.MultiAssetTypeOutput]
	ma = out.Assets()
	b, err := encodeOutput(era, out.Address(), coin, ma)
	if err != nil {
		panic(err)
	}
	return &v1.UtxoEntry{Txin: e.GetTxin(), Txout: b}
}

// Rekey returns the entry with its address moved to a key nobody signed with.
func Rekey(era v1.Era, e *v1.UtxoEntry) *v1.UtxoEntry {
	out, err := ledger.NewTransactionOutputFromCbor(e.GetTxout())
	if err != nil {
		panic(err)
	}
	var other common.Blake2b224
	for i := range other {
		other[i] = 0xee
	}
	b, err := encodeOutput(era, addrFor(other, 1), out.Amount().Uint64(), out.Assets())
	if err != nil {
		panic(err)
	}
	return &v1.UtxoEntry{Txin: e.GetTxin(), Txout: b}
}
