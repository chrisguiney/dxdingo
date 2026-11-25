package corpusgen

import (
	"fmt"
	"math/big"
	"testing"

	"github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// dijkstraCodecCases adds the Dijkstra era to the codec functions. No chain
// holds a Dijkstra block, so the positive cases are the real Conway
// transactions and outputs of the mainnet block in the Dijkstra shape (a
// transaction is [body, witnesses, auxiliary data]) and hand-built items.
func dijkstraCodecCases(t *testing.T, out []entry) []entry {
	const era = v1.Era_ERA_DIJKSTRA
	add := func(name, why string, call *v1.Call) { out = append(out, entry{name, why, call}) }
	tx := func(b []byte, major uint32) *v1.Call {
		r := &v1.CodecTxRequest{Era: era, Cbor: b}
		if major != 0 {
			r.ProtocolVersion = &v1.ProtocolVersion{Major: major}
		}
		return &v1.Call{Request: &v1.Call_CodecTx{CodecTx: r}}
	}
	txOut := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecTxOutput{CodecTxOutput: &v1.CodecTxOutputRequest{Era: era, Cbor: b}}}
	}
	cert := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecCertificate{CodecCertificate: &v1.CodecCertificateRequest{Era: era, Cbor: b}}}
	}
	gov := func(kind v1.GovernanceItemKind, b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecGovernance{CodecGovernance: &v1.CodecGovernanceRequest{Era: era, Kind: kind, Cbor: b}}}
	}
	header := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecBlockHeader{CodecBlockHeader: &v1.CodecBlockHeaderRequest{Era: era, Cbor: b}}}
	}
	body := func(b []byte) *v1.Call {
		return &v1.Call{Request: &v1.Call_CodecBlockBody{CodecBlockBody: &v1.CodecBlockBodyRequest{Era: era, Cbor: b}}}
	}
	update := func(name, why string, b []byte) {
		add("codec_pparams_update_dijkstra_"+name, why, &v1.Call{Request: &v1.Call_CodecPparamsUpdate{CodecPparamsUpdate: &v1.CodecPParamsUpdateRequest{Era: era, Cbor: b}}})
	}

	// Transactions: the first two of the real Conway block, re-shaped.
	dir := gouroborosTestdata(t)
	cb, err := ledger.NewConwayBlockFromCbor(readHex(t, dir, "conway_block.hex"), common.VerifyConfig{SkipBodyHashValidation: true})
	if err != nil {
		t.Fatal(err)
	}
	for i, x := range cb.Transactions() {
		if i >= 2 {
			break
		}
		add(fmt.Sprintf("codec_tx_dijkstra_conway_%d", i), "a real mainnet Conway transaction as [body, witnesses, auxiliary data]", tx(withoutIsValid(t, x.Cbor()), 0))
		for j, u := range x.Produced() {
			if j >= 2 {
				break
			}
			add(fmt.Sprintf("codec_tx_output_dijkstra_conway_%d_%d", i, j), "a real mainnet Conway output", txOut(u.Output.Cbor()))
		}
		for k, c := range x.Certificates() {
			if k >= 1 {
				break
			}
			add(fmt.Sprintf("codec_certificate_dijkstra_conway_%d_%d", i, k), fmt.Sprintf("a real mainnet Conway certificate of type %d", c.Type()), cert(c.Cbor()))
		}
	}
	minimal := mustCbor(t, []any{cbor.RawMessage{0xa3, 0x00, 0x80, 0x01, 0x80, 0x02, 0x00}, cbor.RawMessage{0xa0}, nil})
	add("codec_tx_dijkstra_minimal", "the empty body {0: [], 1: [], 2: 0}", tx(minimal, 0))
	add("codec_tx_dijkstra_version_12", "protocol version 12 is the era", tx(minimal, 12))
	add("codec_tx_dijkstra_version_11", "protocol version 11 is Conway's: UNSUPPORTED on both nodes", tx(minimal, 11))
	add("codec_tx_dijkstra_version_13", "protocol version 13 is past the era: UNSUPPORTED on both nodes", tx(minimal, 13))
	tr, tl := malformed(minimal)
	add("codec_tx_dijkstra_truncated", "cut by one byte is rejected", tx(tr, 0))
	add("codec_tx_dijkstra_trailing", "a trailing byte is rejected", tx(tl, 0))
	add("codec_tx_dijkstra_mempool_valid", "the mempool form with is_valid true: [body, witnesses, true, auxiliary data]", tx(mustCbor(t, []any{cbor.RawMessage{0xa3, 0x00, 0x80, 0x01, 0x80, 0x02, 0x00}, cbor.RawMessage{0xa0}, true, nil}), 0))
	add("codec_tx_dijkstra_mempool_invalid", "the mempool form with is_valid false", tx(mustCbor(t, []any{cbor.RawMessage{0xa3, 0x00, 0x80, 0x01, 0x80, 0x02, 0x00}, cbor.RawMessage{0xa0}, false, nil}), 0))
	add("codec_tx_dijkstra_not_an_array", "an integer is no transaction", tx([]byte{0x01}, 0))

	// Outputs.
	h28, h28b, h32 := rep(0x11, 28), rep(0x22, 28), rep(0x33, 32)
	addr := append([]byte{0x61}, h28...)
	datum := mustCbor(t, cbor.Tag{Number: 24, Content: []byte{0xd8, 0x79, 0x80}})
	script := mustCbor(t, cbor.Tag{Number: 24, Content: mustCbor(t, []any{0, []any{0, h28}})})
	add("codec_tx_output_dijkstra_inline_datum_script_ref", "map output with assets, inline datum and reference script", txOut(babbageOutput(t, addr, h28, datum, script)))
	add("codec_tx_output_dijkstra_datum_hash", "map output carrying a datum hash", txOut(mustCbor(t, map[uint]any{0: addr, 1: 1500000, 2: []any{0, h32}})))
	add("codec_tx_output_dijkstra_legacy_array", "legacy [address, coin] output", txOut(mustCbor(t, []any{addr, 1000000})))
	add("codec_tx_output_dijkstra_not_an_output", "an integer is no output", txOut([]byte{0x01}))

	// Certificates: the Dijkstra CDDL drops the deposit-less registrations,
	// genesis delegation and MIR.
	key := func(h []byte) []any { return []any{0, h} }
	hand := func(name, why string, v any) { add("codec_certificate_dijkstra_"+name, why, cert(mustCbor(t, v))) }
	hand("stake_registration", "tag 0, the deposit-less registration, is expunged from Dijkstra", []any{0, key(h28)})
	hand("stake_deregistration", "tag 1, the deposit-less deregistration, is expunged from Dijkstra", []any{1, key(h28)})
	hand("stake_delegation", "tag 2: delegation to a pool", []any{2, key(h28), h28b})
	hand("pool_retirement", "tag 4: RetirePoolTxCert", []any{4, h28, 300})
	hand("genesis_delegation", "tag 5 is expunged from Dijkstra", []any{5, h28, h28b, h32})
	hand("registration_deposit", "tag 7: RegDepositTxCert", []any{7, key(h28), 2000000})
	hand("unregistration_deposit", "tag 8: UnRegDepositTxCert", []any{8, key(h28), 2000000})
	hand("vote_delegation", "tag 9: DelegTxCert to always-abstain", []any{9, key(h28), []any{2}})
	hand("stake_vote_delegation", "tag 10: a pool and a DRep", []any{10, key(h28), h28b, []any{0, h28b}})
	hand("registration_delegation", "tag 11: RegDepositDelegTxCert", []any{11, key(h28), h28b, 2000000})
	hand("committee_hot", "tag 14: AuthCommitteeHotKeyTxCert", []any{14, key(h28), key(h28b)})
	hand("committee_resign", "tag 15: ResignCommitteeColdTxCert, no anchor", []any{15, key(h28), nil})
	hand("drep_registration", "tag 16: RegDRepTxCert", []any{16, key(h28), 500000000, nil})
	hand("drep_update", "tag 18: UpdateDRepTxCert with an anchor", []any{18, key(h28), []any{"https://example.org/a", h32}})
	hand("unknown_tag", "tag 19 is no certificate", []any{19, key(h28)})

	// Governance.
	anchor := []any{"https://example.org/p", h32}
	reward := append([]byte{0xe0}, h28...)
	prop := func(name, why string, action any) {
		add("codec_governance_dijkstra_proposal_"+name, why, gov(v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, mustCbor(t, []any{1000000000, reward, action, anchor})))
	}
	prop("info", "Info action: the simplest proposal", []any{6})
	prop("hard_fork", "HardForkInitiation to protocol 13.0, no parent", []any{1, nil, []any{13, 0}})
	prop("no_confidence", "NoConfidence, no parent", []any{3, nil})
	prop("new_constitution", "NewConstitution with no guardrail script", []any{5, nil, []any{anchor, nil}})
	add("codec_governance_dijkstra_proposal_truncated", "proposal missing its anchor", gov(v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_PROPOSAL, mustCbor(t, []any{1000000000, reward, []any{6}})))
	votes := append([]byte{0xa1}, mustCbor(t, []any{2, h28})...)
	votes = append(votes, 0xa1)
	votes = append(votes, mustCbor(t, []any{h32, 0})...)
	votes = append(votes, mustCbor(t, []any{1, nil})...)
	add("codec_governance_dijkstra_voting_procedures", "one DRep voting yes on one action", gov(v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, votes))
	add("codec_governance_dijkstra_voting_procedures_trailing", "trailing byte is rejected", gov(v1.GovernanceItemKind_GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES, append(append([]byte{}, votes...), 0)))

	// Header and block body, from the consensus golden Dijkstra block.
	hdr := readHex(t, "../../cmd/dxnode-go/testdata", "dijkstra_header.hex")
	add("codec_block_header_dijkstra", "the header of the consensus golden Dijkstra block", header(hdr))
	tr, tl = malformed(hdr)
	add("codec_block_header_dijkstra_truncated", "header cut by one byte is rejected", header(tr))
	add("codec_block_header_dijkstra_trailing", "header with a trailing byte is rejected", header(tl))
	blk := func(tail ...byte) []byte { return append(append([]byte{0x82}, hdr...), tail...) }
	add("codec_block_body_dijkstra_three_components", "a body of [transactions, leios, peras]: gouroboros accepts it, the reference needs four components", body(blk(0x83, 0x80, 0xf6, 0xf6)))
	add("codec_block_body_dijkstra_four_components", "a body of [invalid transactions, transactions, leios, peras]: the reference accepts it, gouroboros needs three components", body(blk(0x84, 0xf6, 0x80, 0xf6, 0xf6)))
	add("codec_block_body_dijkstra_golden_layout", "the older golden layout (bodies, witnesses, auxiliary data, invalid transactions): both nodes reject it", body(append(append([]byte{0x86}, hdr...), 0x80, 0x80, 0xa0, 0x80)))

	// Protocol-parameter updates.
	rr := func(n, d int64) cbor.Rat { return cbor.Rat{Rat: big.NewRat(n, d)} }
	m := func(v map[uint]any) []byte { return mustCbor(t, v) }
	update("max_tx_size", "a one-key update", m(map[uint]any{3: 16384}))
	update("governance", "committee and governance parameters", m(map[uint]any{27: 7, 28: 146, 29: 6, 30: 100000000000, 31: 500000000, 32: 20, 33: rr(15, 1)}))
	update("plutus", "cost model, prices and execution units", m(map[uint]any{18: map[uint][]int64{1: {5, 6}}, 19: []any{rr(577, 10000), rr(721, 10000000)}, 20: []any{14000000, 10000000000}, 22: 5000}))
	update("empty", "an update that sets nothing", m(map[uint]any{}))
	update("trailing", "trailing byte is rejected", append(m(map[uint]any{3: 16384}), 0))
	return out
}

// withoutIsValid drops the is_valid flag of a four-element transaction.
func withoutIsValid(t *testing.T, b []byte) []byte {
	t.Helper()
	var parts []cbor.RawMessage
	if _, err := cbor.Decode(b, &parts); err != nil {
		t.Fatal(err)
	}
	if len(parts) != 4 {
		return b
	}
	return mustCbor(t, []any{parts[0], parts[1], parts[3]})
}
