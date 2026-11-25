package corpusgen

import (
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// The codec corpus is cut from real chain bytes: the mainnet blocks the
// gouroboros tests embed, one per era. Transactions, outputs, certificates,
// headers and whole blocks are extracted with gouroboros itself, so every
// positive case is a real on-chain encoding. Malformed variants (truncated,
// trailing byte, wrong era) are derived from them.

func TestGenerateCodecCorpus(t *testing.T) {
	if os.Getenv("DXDINGO_GEN_CORPUS") == "" {
		t.Skip("set DXDINGO_GEN_CORPUS=1 to regenerate corpus/")
	}
	var ok, div []entry
	for _, e := range codecCases(t) {
		if why, bad := codecDivergences[e.name]; bad {
			e.comment = why
			div = append(div, e)
		} else {
			ok = append(ok, e)
		}
	}
	writeCorpus(t, "", ok)
	writeCorpus(t, "divergences", div)
}

// codecDivergences are confirmed mismatches, kept out of corpus/. The
// reference re-serialises an output or transaction it decoded; gouroboros
// returns the wire bytes it was given. Each has direct tests on both sides
// (dxnode-go TestDivergenceCodec*, Test.Codec "divergence: ..." checks).
var codecDivergences = map[string]string{
	"codec_tx_output_babbage_1_1":          "real mainnet output: the reference re-encodes the multi-asset map sorted by bytes in an indefinite-length map; gouroboros keeps the wire order (TestDivergenceCodecOutputKeepsWireBytes)",
	"codec_tx_output_conway_1_1":           "real mainnet output [address, [coin, {}]]: the reference drops the empty multi-asset, gouroboros keeps it (TestDivergenceCodecOutputKeepsWireBytes)",
	"codec_tx_output_conway_datum_hash":    "Babbage map output with a datum hash: the reference re-encodes the legacy array [address, coin, datum_hash] (TestDivergenceCodecOutputKeepsWireBytes)",
	"codec_tx_wrong_era_shelley_as_conway": "a three-element transaction decodes as Conway on both nodes; the reference re-encodes it with IsValid, gouroboros keeps three elements (TestDivergenceCodecThreeElementTx)",
}

type codecEra struct {
	name      string
	era       v1.Era
	blockType uint
	txType    uint
	file      string
}

var codecEras = []codecEra{
	{"shelley", v1.Era_ERA_SHELLEY, ledger.BlockTypeShelley, ledger.TxTypeShelley, "shelley_block.hex"},
	{"allegra", v1.Era_ERA_ALLEGRA, ledger.BlockTypeAllegra, ledger.TxTypeAllegra, "allegra_block.hex"},
	{"mary", v1.Era_ERA_MARY, ledger.BlockTypeMary, ledger.TxTypeMary, "mary_block.hex"},
	{"alonzo", v1.Era_ERA_ALONZO, ledger.BlockTypeAlonzo, ledger.TxTypeAlonzo, "alonzo_block.hex"},
	{"babbage", v1.Era_ERA_BABBAGE, ledger.BlockTypeBabbage, ledger.TxTypeBabbage, "babbage_block.hex"},
	{"conway", v1.Era_ERA_CONWAY, ledger.BlockTypeConway, ledger.TxTypeConway, "conway_block.hex"},
}

func gouroborosTestdata(t *testing.T) string {
	t.Helper()
	out, err := exec.Command("go", "list", "-m", "-f", "{{.Dir}}", "github.com/blinklabs-io/gouroboros").Output()
	if err != nil {
		t.Fatal(err)
	}
	return filepath.Join(strings.TrimSpace(string(out)), "internal", "testdata")
}

func readHex(t *testing.T, dir, name string) []byte {
	t.Helper()
	s, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		t.Fatal(err)
	}
	b, err := hex.DecodeString(strings.TrimSpace(string(s)))
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func mustCbor(t *testing.T, v any) []byte {
	t.Helper()
	b, err := cbor.Encode(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// malformed returns the truncated and trailing-byte variants of a valid
// encoding.
func malformed(b []byte) (truncated, trailing []byte) {
	return b[:len(b)-1], append(append([]byte{}, b...), 0x00)
}

// wireTx returns the transaction as it is submitted: gouroboros rebuilds a
// block's transactions without the Alonzo-family IsValid flag, which the
// submission encoding carries as the third of four elements.
func wireTx(t *testing.T, x common.Transaction) []byte {
	t.Helper()
	b := x.Cbor()
	if x.Type() < ledger.TxTypeAlonzo {
		return b
	}
	var parts []cbor.RawMessage
	if _, err := cbor.Decode(b, &parts); err != nil {
		t.Fatal(err)
	}
	if len(parts) != 3 {
		return b
	}
	flag := byte(0xf5)
	if !x.IsValid() {
		flag = 0xf4
	}
	out := []byte{0x84}
	out = append(out, parts[0]...)
	out = append(out, parts[1]...)
	out = append(out, flag)
	out = append(out, parts[2]...)
	return out
}

func codecCases(t *testing.T) []entry {
	dir := gouroborosTestdata(t)
	var out []entry
	add := func(name, why string, call *v1.Call) { out = append(out, entry{name, why, call}) }

	tx := func(e v1.Era, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecTx{CodecTx: &v1.CodecTxRequest{Era: e, Cbor: b}}}
	}
	txOut := func(e v1.Era, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecTxOutput{CodecTxOutput: &v1.CodecTxOutputRequest{Era: e, Cbor: b}}}
	}
	cert := func(e v1.Era, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecCertificate{CodecCertificate: &v1.CodecCertificateRequest{Era: e, Cbor: b}}}
	}
	header := func(e v1.Era, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecBlockHeader{CodecBlockHeader: &v1.CodecBlockHeaderRequest{Era: e, Cbor: b}}}
	}
	body := func(e v1.Era, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecBlockBody{CodecBlockBody: &v1.CodecBlockBodyRequest{Era: e, Cbor: b}}}
	}

	certsSeen := map[string]bool{}
	for _, e := range codecEras {
		raw := readHex(t, dir, e.file)
		blk, err := ledger.NewBlockFromCbor(e.blockType, raw, common.VerifyConfig{SkipBodyHashValidation: true})
		if err != nil {
			t.Fatal(err)
		}
		// Whole block: the body codec, and the header inside it.
		add("codec_block_body_"+e.name, "real mainnet "+e.name+" block", body(e.era, raw))
		hdr := blk.Header().Cbor()
		add("codec_block_header_"+e.name, "real mainnet "+e.name+" header", header(e.era, hdr))
		tr, tl := malformed(hdr)
		add("codec_block_header_"+e.name+"_truncated", "header cut by one byte is rejected", header(e.era, tr))
		add("codec_block_header_"+e.name+"_trailing", "header with a trailing byte is rejected", header(e.era, tl))
		if e.name == "shelley" || e.name == "conway" {
			tr, tl := malformed(raw)
			add("codec_block_body_"+e.name+"_truncated", "block cut by one byte is rejected", body(e.era, tr))
			add("codec_block_body_"+e.name+"_trailing", "block with a trailing byte is rejected", body(e.era, tl))
		}

		txs := blk.Transactions()
		for i, x := range txs {
			if i >= 2 {
				break
			}
			add(fmt.Sprintf("codec_tx_%s_%d", e.name, i), "real mainnet "+e.name+" transaction", tx(e.era, wireTx(t, x)))
			outs := x.Produced()
			for j, u := range outs {
				if j >= 2 {
					break
				}
				add(fmt.Sprintf("codec_tx_output_%s_%d_%d", e.name, i, j), "real mainnet "+e.name+" output", txOut(e.era, u.Output.Cbor()))
			}
			for k, c := range x.Certificates() {
				kind := fmt.Sprintf("%s_%d", e.name, c.Type())
				if k >= 2 || certsSeen[kind] {
					continue
				}
				certsSeen[kind] = true
				add("codec_certificate_"+kind, fmt.Sprintf("real mainnet %s certificate, type %d", e.name, c.Type()), cert(e.era, c.Cbor()))
			}
		}
		if len(txs) > 0 {
			x := wireTx(t, txs[0])
			tr, tl := malformed(x)
			add("codec_tx_"+e.name+"_truncated", "transaction cut by one byte is rejected", tx(e.era, tr))
			add("codec_tx_"+e.name+"_trailing", "transaction with a trailing byte is rejected", tx(e.era, tl))
		}
	}

	// Wrong era: a Conway transaction through the Shelley decoder, and a
	// Shelley transaction through the Conway decoder.
	conwayBlk := readHex(t, dir, "conway_block.hex")
	cb, err := ledger.NewConwayBlockFromCbor(conwayBlk, common.VerifyConfig{SkipBodyHashValidation: true})
	if err != nil {
		t.Fatal(err)
	}
	if txs := cb.Transactions(); len(txs) > 0 {
		add("codec_tx_wrong_era_conway_as_shelley", "a Conway transaction is not a Shelley one", tx(v1.Era_ERA_SHELLEY, wireTx(t, txs[0])))
		add("codec_tx_protocol_version_outside_era", "protocol version 5 is outside Conway: UNSUPPORTED on both nodes", &v1.Call{Request: &v1.Call_CodecTx{CodecTx: &v1.CodecTxRequest{
			Era: v1.Era_ERA_CONWAY, ProtocolVersion: &v1.ProtocolVersion{Major: 5}, Cbor: wireTx(t, txs[0]),
		}}})
	}
	oneShot := readHex(t, dir, "mainnet_tx_oneshot_mint.hex")
	add("codec_tx_conway_plutus_v3_mint", "mainnet Plutus V3 mint: redeemers and datums keep their wire encoding", tx(v1.Era_ERA_CONWAY, oneShot))
	sb, err := ledger.NewShelleyBlockFromCbor(readHex(t, dir, "shelley_block.hex"), common.VerifyConfig{SkipBodyHashValidation: true})
	if err != nil {
		t.Fatal(err)
	}
	if txs := sb.Transactions(); len(txs) > 0 {
		add("codec_tx_wrong_era_shelley_as_conway", "a Shelley transaction is not a Conway one", tx(v1.Era_ERA_CONWAY, wireTx(t, txs[0])))
	}

	// Hand-built certificates cover the constructors no sampled block holds.
	h28, h28b, h32 := rep(0x11, 28), rep(0x22, 28), rep(0x33, 32)
	key := func(h []byte) []any { return []any{0, h} }
	hand := func(name, why string, e v1.Era, v any) { add("codec_certificate_"+name, why, cert(e, mustCbor(t, v))) }
	hand("shelley_stake_registration", "tag 0: RegTxCert", v1.Era_ERA_SHELLEY, []any{0, key(h28)})
	hand("shelley_stake_deregistration", "tag 1: UnRegTxCert", v1.Era_ERA_SHELLEY, []any{1, key(h28)})
	hand("shelley_stake_delegation", "tag 2: DelegStakeTxCert", v1.Era_ERA_SHELLEY, []any{2, key(h28), h28b})
	hand("shelley_pool_retirement", "tag 4: RetirePoolTxCert", v1.Era_ERA_SHELLEY, []any{4, h28, 300})
	hand("shelley_genesis_delegation", "tag 5: GenesisDelegTxCert", v1.Era_ERA_SHELLEY, []any{5, h28, h28b, h32})
	hand("conway_registration_deposit", "tag 7: RegDepositTxCert", v1.Era_ERA_CONWAY, []any{7, key(h28), 2000000})
	hand("conway_unregistration_deposit", "tag 8: UnRegDepositTxCert", v1.Era_ERA_CONWAY, []any{8, key(h28), 2000000})
	hand("conway_vote_delegation", "tag 9: DelegTxCert to always-abstain", v1.Era_ERA_CONWAY, []any{9, key(h28), []any{2}})
	hand("conway_stake_vote_delegation", "tag 10: DelegTxCert to a pool and a DRep", v1.Era_ERA_CONWAY, []any{10, key(h28), h28b, []any{0, h28b}})
	hand("conway_stake_registration_delegation", "tag 11: RegDepositDelegTxCert", v1.Era_ERA_CONWAY, []any{11, key(h28), h28b, 2000000})
	hand("conway_committee_hot", "tag 14: AuthCommitteeHotKeyTxCert", v1.Era_ERA_CONWAY, []any{14, key(h28), key(h28b)})
	hand("conway_committee_resign", "tag 15: ResignCommitteeColdTxCert, no anchor", v1.Era_ERA_CONWAY, []any{15, key(h28), nil})
	hand("conway_drep_registration", "tag 16: RegDRepTxCert", v1.Era_ERA_CONWAY, []any{16, key(h28), 500000000, nil})
	hand("conway_drep_update", "tag 18: UpdateDRepTxCert with an anchor", v1.Era_ERA_CONWAY, []any{18, key(h28), []any{"https://example.org/a", h32}})
	hand("babbage_conway_tag", "tag 7 does not exist before Conway: rejected in Babbage", v1.Era_ERA_BABBAGE, []any{7, key(h28), 2000000})
	hand("conway_unknown_tag", "tag 19 is no certificate", v1.Era_ERA_CONWAY, []any{19, key(h28)})

	// Hand-built outputs: a Babbage output with multi-assets, an inline datum
	// and a reference script.
	addr := append([]byte{0x61}, h28...)
	datum := mustCbor(t, cbor.Tag{Number: 24, Content: []byte{0xd8, 0x79, 0x80}})
	script := mustCbor(t, cbor.Tag{Number: 24, Content: mustCbor(t, []any{0, []any{0, h28}})})
	add("codec_tx_output_babbage_inline_datum_script_ref", "map output with assets, inline datum and reference script", txOut(v1.Era_ERA_BABBAGE, babbageOutput(t, addr, h28, datum, script)))
	add("codec_tx_output_conway_datum_hash", "map output carrying a datum hash", txOut(v1.Era_ERA_CONWAY, mustCbor(t, map[uint]any{
		0: addr, 1: 1500000, 2: []any{0, h32},
	})))
	add("codec_tx_output_shelley_array", "legacy [address, coin] output", txOut(v1.Era_ERA_SHELLEY, mustCbor(t, []any{addr, 1000000})))
	add("codec_tx_output_not_an_output", "an integer is no output", txOut(v1.Era_ERA_CONWAY, []byte{0x01}))

	// Hand-built governance items.
	anchor := []any{"https://example.org/p", h32}
	reward := append([]byte{0xe0}, h28...)
	prop := func(name, why string, action any) {
		add("codec_governance_proposal_"+name, why, &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{
			Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: mustCbor(t, []any{1000000000, reward, action, anchor}),
		}}})
	}
	prop("info", "Info action: the simplest proposal", []any{6})
	prop("hard_fork", "HardForkInitiation to protocol 11.0, no parent", []any{1, nil, []any{11, 0}})
	prop("treasury", "TreasuryWithdrawals of one account", cbor.RawMessage(append(append([]byte{0x83, 0x02, 0xa1}, mustCbor(t, reward)...), append(mustCbor(t, 5000000), 0xf6)...)))
	prop("no_confidence", "NoConfidence, no parent", []any{3, nil})
	prop("new_constitution", "NewConstitution with no guardrail script", []any{5, nil, []any{anchor, nil}})
	add("codec_governance_proposal_truncated", "proposal missing its anchor", &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: mustCbor(t, []any{1000000000, reward, []any{6}}),
	}}})
	add("codec_governance_proposal_pre_conway", "governance items do not exist in Babbage: UNSUPPORTED", &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_BABBAGE, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, Cbor: mustCbor(t, []any{1000000000, reward, []any{6}, anchor}),
	}}})
	voter := []any{2, h28}
	actionID := []any{h32, 0}
	votes := append([]byte{0xa1}, mustCbor(t, voter)...)
	votes = append(votes, 0xa1)
	votes = append(votes, mustCbor(t, actionID)...)
	votes = append(votes, mustCbor(t, []any{1, nil})...)
	add("codec_governance_voting_procedures", "one DRep voting yes on one action", &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: votes,
	}}})
	add("codec_governance_voting_procedures_trailing", "trailing byte is rejected", &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{
		Era: v1.Era_ERA_CONWAY, Kind: v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, Cbor: append(append([]byte{}, votes...), 0),
	}}})

	// Plutus data.
	pd := func(name, why string, b []byte) {
		add("codec_plutus_data_"+name, why, &v1.Call{Request: &v1.Call_CodecPlutusData{CodecPlutusData: &v1.CodecPlutusDataRequest{Cbor: b}}})
	}
	pd("constr_empty", "Constr 0 []", []byte{0xd8, 0x79, 0x80})
	pd("integer", "a small integer", []byte{0x1a, 0x00, 0x0f, 0x42, 0x40})
	pd("big_integer", "bignum above 64 bits keeps its tag-2 form", append([]byte{0xc2, 0x49}, rep(0x01, 9)...))
	pd("bytes", "a short byte string", []byte{0x44, 1, 2, 3, 4})
	pd("bytes_long", "a 65-byte string: the reference encoder chunks above 64, the wire form decides", append([]byte{0x58, 65}, rep(0xab, 65)...))
	pd("bytes_chunked", "an indefinite byte string of two chunks", []byte{0x5f, 0x41, 0x01, 0x41, 0x02, 0xff})
	pd("list_indefinite", "indefinite list: the wire form decides the re-encoding", []byte{0x9f, 0x01, 0x02, 0xff})
	pd("map", "a definite map", []byte{0xa1, 0x01, 0x02})
	pd("constr_general", "tag 102 general constructor form", []byte{0xd8, 0x66, 0x82, 0x09, 0x80})
	pd("empty", "no bytes", nil)
	pd("break_only", "a lone break is no data", []byte{0xff})
	pd("trailing", "trailing byte after a valid datum", []byte{0xd8, 0x79, 0x80, 0x00})
	pd("text_string", "text is no Plutus data", []byte{0x61, 0x61})
	pd("negative_bignum", "bignum tag 3", append([]byte{0xc3, 0x49}, rep(0x01, 9)...))

	// Byron: ABlockOrBoundary wire form [tag, block], from the mainnet block.
	byron := readHex(t, dir, "byron_block.hex")
	wrapped := append([]byte{0x82, 0x01}, byron...)
	byronCall := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecByronBlock{CodecByronBlock: &v1.CodecByronBlockRequest{ProtocolMagic: 764824073, Cbor: b}}}
	}
	add("codec_byron_block_main", "real mainnet Byron main block", byronCall(wrapped))
	tr, tl := malformed(wrapped)
	add("codec_byron_block_truncated", "Byron block cut by one byte is rejected", byronCall(tr))
	add("codec_byron_block_trailing", "Byron block with a trailing byte is rejected", byronCall(tl))
	add("codec_byron_block_bad_tag", "tag 2 is neither boundary nor main", byronCall(append([]byte{0x82, 0x02}, byron...)))
	return out
}

func babbageOutput(t *testing.T, addr, policy, datum, script []byte) []byte {
	t.Helper()
	// {0: addr, 1: [coin, {policy: {name: qty}}], 2: [1, datum], 3: script}
	// is assembled by hand so the map keys stay in canonical order.
	var b []byte
	b = append(b, 0xa4)
	b = append(b, 0x00)
	b = append(b, mustCbor(t, addr)...)
	b = append(b, 0x01)
	b = append(b, 0x82)
	b = append(b, mustCbor(t, 2000000)...)
	b = append(b, 0xa1)
	b = append(b, mustCbor(t, policy)...)
	b = append(b, 0xa2)
	b = append(b, mustCbor(t, []byte("tokA"))...)
	b = append(b, 0x05)
	b = append(b, mustCbor(t, []byte("tokB"))...)
	b = append(b, 0x07)
	b = append(b, 0x02)
	b = append(b, 0x82, 0x01)
	b = append(b, datum...)
	b = append(b, 0x03)
	b = append(b, script...)
	return b
}
