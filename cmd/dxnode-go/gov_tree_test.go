package main

import (
	"bytes"
	"context"
	"fmt"
	"sort"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Expected sets are derived by hand from Governance/Proposals.hs
// proposalsApplyEnactment: expired ids leave with their descendants, then
// each enacted action in order leaves with the competing children of its
// parent and their descendants. Same cases as Test/Gov.hs.

func idBytes(b byte) []byte { return bytes.Repeat([]byte{b}, 32) }

// pcAction is a ParameterChange (maxBBSize) whose previous action id is
// (parent, 0), or none for parent 0.
func pcAction(parent byte) []byte {
	prefix := "8400f6"
	if parent != 0 {
		prefix = "840082" + "5820" + fmt.Sprintf("%x", idBytes(parent)) + "00"
	}
	return mustHex(prefix + "a1021a00016000f6")
}

func hfAction(parent byte) []byte {
	prefix := "8301f6"
	if parent != 0 {
		prefix = "830182" + "5820" + fmt.Sprintf("%x", idBytes(parent)) + "00"
	}
	return mustHex(prefix + "820a00")
}

func proposal(id byte, action []byte) *v1.Proposal {
	return &v1.Proposal{
		Id: &v1.GovActionId{TxId: idBytes(id)}, Action: action,
		ProposedEpoch: 1, ExpiresEpoch: 10, Deposit: 1000, ReturnAccount: cred(0x50),
	}
}

func idList(ids ...byte) []*v1.GovActionId {
	var out []*v1.GovActionId
	for _, b := range ids {
		out = append(out, &v1.GovActionId{TxId: idBytes(b)})
	}
	return out
}

func idSet(xs []*v1.GovActionId) []byte {
	var out []byte
	for _, x := range xs {
		out = append(out, x.GetTxId()[0])
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// 1 and 2 compete for the parameter root; 3 follows 1 and 4 follows 2; 5 is a
// hard fork; 6 is a treasury withdrawal; 7 is an info action.
func treeProposals() []*v1.Proposal {
	return []*v1.Proposal{
		proposal(1, pcAction(0)), proposal(2, pcAction(0)), proposal(3, pcAction(1)), proposal(4, pcAction(2)),
		proposal(5, hfAction(0)), proposal(6, mustHex("8302a0f6")), proposal(7, mustHex("8106")),
	}
}

func TestGovProposalTree(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		enacted, expired   []byte
		remaining, removed []byte
	}{
		"enacted action removes competitors and their subtrees": {[]byte{1}, nil, []byte{3, 5, 6, 7}, []byte{2, 4}},
		"expired action removes its subtree":                    {nil, []byte{1}, []byte{2, 4, 5, 6, 7}, []byte{1, 3}},
		"chain enacted in one round":                            {[]byte{1, 3}, nil, []byte{5, 6, 7}, []byte{2, 4}},
		"enactment and expiry together":                         {[]byte{1}, []byte{5}, []byte{3, 6, 7}, []byte{2, 4, 5}},
		"treasury withdrawal has no competitors":                {[]byte{6}, nil, []byte{1, 2, 3, 4, 5, 7}, nil},
		"nothing happens":                                       {nil, nil, []byte{1, 2, 3, 4, 5, 6, 7}, nil},
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			got, err := govProposalTree(context.Background(), &v1.GovProposalTreeRequest{
				Proposals: treeProposals(), Enacted: idList(tc.enacted...), Expired: idList(tc.expired...),
			})
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(idSet(got.GetRemaining()), tc.remaining) || !bytes.Equal(idSet(got.GetRemoved()), tc.removed) {
				t.Fatalf("remaining %v removed %v, want %v and %v", idSet(got.GetRemaining()), idSet(got.GetRemoved()), tc.remaining, tc.removed)
			}
		})
	}
}

func TestGovProposalTreeFromEnactedRoot(t *testing.T) {
	t.Parallel()
	// The parameter root is action 9; 1 and 2 both follow it.
	props := []*v1.Proposal{proposal(1, pcAction(9)), proposal(2, pcAction(9)), proposal(3, pcAction(1))}
	got, err := govProposalTree(context.Background(), &v1.GovProposalTreeRequest{
		Proposals: props, EnactState: &v1.EnactState{PrevPparamUpdate: &v1.GovActionId{TxId: idBytes(9)}},
		Enacted: idList(2),
	})
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(idSet(got.GetRemaining()), nil) || !bytes.Equal(idSet(got.GetRemoved()), []byte{1, 3}) {
		t.Fatalf("remaining %v removed %v", idSet(got.GetRemaining()), idSet(got.GetRemoved()))
	}
}

func TestGovProposalTreeInconsistentForestIsUnsupported(t *testing.T) {
	t.Parallel()
	// Action 1 names a parent that is neither the enacted root (none) nor a
	// proposal.
	_, err := govProposalTree(context.Background(), &v1.GovProposalTreeRequest{
		Proposals: []*v1.Proposal{proposal(1, pcAction(9))},
	})
	if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED {
		t.Fatalf("code %v, err %v", code, err)
	}
}

func TestGovProposalTreeUnknownEnactedIsBadRequest(t *testing.T) {
	t.Parallel()
	_, err := govProposalTree(context.Background(), &v1.GovProposalTreeRequest{
		Proposals: treeProposals(), Enacted: idList(42),
	})
	if code := dispatch.Code(err); code != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("code %v, err %v", code, err)
	}
}
