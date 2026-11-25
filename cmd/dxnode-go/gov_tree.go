package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"sort"

	"github.com/blinklabs-io/dingo/database/models"
	"github.com/blinklabs-io/dingo/ledger/governance"
	lcommon "github.com/blinklabs-io/gouroboros/ledger/common"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

type treeNode struct {
	id       *v1.GovActionId
	typ      lcommon.GovActionType
	proposal *models.GovernanceProposal
}

func idKey(id *v1.GovActionId) string { return fmt.Sprintf("%x#%d", id.GetTxId(), id.GetIndex()) }

// purposeOf groups the action types that share a chain of previous action ids.
func purposeOf(t lcommon.GovActionType) int {
	switch t {
	case lcommon.GovActionTypeParameterChange:
		return 1
	case lcommon.GovActionTypeHardForkInitiation:
		return 2
	case lcommon.GovActionTypeNoConfidence, lcommon.GovActionTypeUpdateCommittee:
		return 3
	case lcommon.GovActionTypeNewConstitution:
		return 4
	}
	return 0
}

// govProposalTree maps to governance.PlanOrphanRemovals. The parent of each
// proposal is the previous action id inside its action CBOR, as in the
// reference; the request's parent and action_type fields are not read.
//
// The reference builds its proposal forest only from consistent input (every
// parent is the enacted root of its purpose or an earlier proposal of that
// purpose), so an inconsistent request is UNSUPPORTED.
func govProposalTree(_ context.Context, req *v1.GovProposalTreeRequest) (*v1.GovProposalTreeResponse, error) {
	roots := map[int]*v1.GovActionId{
		1: req.GetEnactState().GetPrevPparamUpdate(), 2: req.GetEnactState().GetPrevHardFork(),
		3: req.GetEnactState().GetPrevCommittee(), 4: req.GetEnactState().GetPrevConstitution(),
	}
	byID := map[string]*treeNode{}
	var order []*treeNode
	for _, p := range req.GetProposals() {
		action, err := decodeGovAction(p.GetAction())
		if err != nil {
			return nil, err
		}
		if len(p.GetId().GetTxId()) != 32 {
			return nil, dispatch.BadRequest(errors.New("proposal id: want 32-byte tx id"))
		}
		key := idKey(p.GetId())
		if _, dup := byID[key]; dup {
			return nil, dispatch.BadRequest(fmt.Errorf("duplicate proposal %s", key))
		}
		row := &models.GovernanceProposal{
			TxHash: p.GetId().GetTxId(), ActionIndex: p.GetId().GetIndex(), ActionType: uint8(action.Type),
		}
		parent := actionParent(action.Action)
		purpose := purposeOf(action.Type)
		var parentID *v1.GovActionId
		if parent != nil {
			idx := parent.GovActionIdx
			row.ParentTxHash, row.ParentActionIdx = bytes.Clone(parent.TransactionId[:]), &idx
			parentID = &v1.GovActionId{TxId: row.ParentTxHash, Index: idx}
		}
		if purpose != 0 && !sameOptionalID(parentID, roots[purpose]) {
			earlier, ok := byID[idKey(parentID)]
			if parentID == nil || !ok || purposeOf(earlier.typ) != purpose {
				return nil, dispatch.Unsupported(fmt.Errorf("proposal %s names a parent that is not its purpose root or an earlier proposal", key))
			}
		}
		node := &treeNode{id: p.GetId(), typ: action.Type, proposal: row}
		byID[key] = node
		order = append(order, node)
	}

	pick := func(name string, ids []*v1.GovActionId) ([]*models.GovernanceProposal, error) {
		var out []*models.GovernanceProposal
		for _, id := range ids {
			n, ok := byID[idKey(id)]
			if !ok {
				return nil, dispatch.BadRequest(fmt.Errorf("%s: unknown proposal %s", name, idKey(id)))
			}
			out = append(out, n.proposal)
		}
		return out, nil
	}
	enacted, err := pick("enacted", req.GetEnacted())
	if err != nil {
		return nil, err
	}
	expired, err := pick("expired", req.GetExpired())
	if err != nil {
		return nil, err
	}
	// Dingo's active set excludes enacted proposals and includes the expiring
	// ones, whose descendants it then sweeps.
	enactedKeys := map[string]bool{}
	for _, id := range req.GetEnacted() {
		enactedKeys[idKey(id)] = true
	}
	var active []*models.GovernanceProposal
	for _, n := range order {
		if !enactedKeys[idKey(n.id)] {
			active = append(active, n.proposal)
		}
	}

	removedKeys := map[string]bool{}
	for _, r := range governance.PlanOrphanRemovals(enacted, expired, nil, active) {
		removedKeys[idKey(&v1.GovActionId{TxId: r.Proposal.TxHash, Index: r.Proposal.ActionIndex})] = true
	}
	for _, id := range req.GetExpired() {
		removedKeys[idKey(id)] = true
	}
	resp := &v1.GovProposalTreeResponse{}
	for _, n := range order {
		switch key := idKey(n.id); {
		case enactedKeys[key]:
		case removedKeys[key]:
			resp.Removed = append(resp.Removed, n.id)
		default:
			resp.Remaining = append(resp.Remaining, n.id)
		}
	}
	sortActionIDs(resp.Remaining)
	sortActionIDs(resp.Removed)
	return resp, nil
}

func sameOptionalID(a, b *v1.GovActionId) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	return sameActionID(a, b)
}

// sortActionIDs puts action ids in canonical order: tx id, then index.
func sortActionIDs(ids []*v1.GovActionId) {
	sort.Slice(ids, func(i, j int) bool {
		if c := bytes.Compare(ids[i].GetTxId(), ids[j].GetTxId()); c != 0 {
			return c < 0
		}
		return ids[i].GetIndex() < ids[j].GetIndex()
	})
}
