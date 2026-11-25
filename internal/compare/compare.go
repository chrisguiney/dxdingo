// Package compare decides whether two nodes' replies to the same call agree.
package compare

import (
	"fmt"
	"sort"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/testing/protocmp"

	"github.com/google/go-cmp/cmp"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Verdict classifies a pair of replies.
type Verdict int

const (
	// Match: both evaluated the call and agree.
	Match Verdict = iota
	// Mismatch: both evaluated the call and disagree. This is a finding.
	Mismatch
	// Skipped: at least one node does not implement or support the call.
	Skipped
	// HarnessFailure: a node could not evaluate the call (bad request,
	// adapter crash). This is a harness bug, not a finding.
	HarnessFailure
)

func (v Verdict) String() string {
	return [...]string{"match", "mismatch", "skipped", "harness-failure"}[v]
}

// Result is the outcome of comparing two replies.
type Result struct {
	Verdict  Verdict
	Function string
	// Diff is a human-readable difference for Mismatch and HarnessFailure.
	Diff string
}

var replyOneof = (&v1.Reply{}).ProtoReflect().Descriptor().Oneofs().ByName("response")

// Replies compares two replies to the same call. Ids are ignored. A
// rejection is compared as a set of (rule, name); detail is ignored.
//
// Repeated fields are compared in order. Fields documented as sets must be
// sorted canonically by each node adapter before replying.
func Replies(a, b *v1.Reply) Result {
	fn := function(a)
	if fn == "" {
		fn = function(b)
	}
	if skipped(a) || skipped(b) {
		return Result{Verdict: Skipped, Function: fn}
	}
	if a.GetError() != nil || b.GetError() != nil {
		return Result{Verdict: HarnessFailure, Function: fn,
			Diff: fmt.Sprintf("a error: %v\nb error: %v", a.GetError(), b.GetError())}
	}
	na, nb := normalize(a), normalize(b)
	if diff := cmp.Diff(na, nb, protocmp.Transform()); diff != "" {
		return Result{Verdict: Mismatch, Function: fn, Diff: diff}
	}
	return Result{Verdict: Match, Function: fn}
}

func skipped(r *v1.Reply) bool {
	switch r.GetError().GetCode() {
	case v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNIMPLEMENTED,
		v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED:
		return true
	}
	return false
}

func function(r *v1.Reply) string {
	if f := r.ProtoReflect().WhichOneof(replyOneof); f != nil {
		return string(f.Name())
	}
	return ""
}

// normalize returns a copy with the id cleared and, when rejected, only the
// sorted distinct (rule, name) pairs kept; other output fields are dropped
// because a rejected response leaves them unset.
func normalize(r *v1.Reply) *v1.Reply {
	out := proto.Clone(r).(*v1.Reply)
	out.Id = 0
	f := out.ProtoReflect().WhichOneof(replyOneof)
	if f == nil {
		return out
	}
	resp := out.ProtoReflect().Get(f).Message()
	rf := resp.Descriptor().Fields().ByName("rejection")
	if rf == nil || !resp.Has(rf) {
		return out
	}
	rej := resp.Get(rf).Message().Interface().(*v1.Rejection)
	seen := map[[2]string]bool{}
	var fs []*v1.Failure
	for _, x := range rej.GetFailures() {
		k := [2]string{x.GetRule(), x.GetName()}
		if !seen[k] {
			seen[k] = true
			fs = append(fs, &v1.Failure{Rule: k[0], Name: k[1]})
		}
	}
	sort.Slice(fs, func(i, j int) bool {
		if fs[i].Rule != fs[j].Rule {
			return fs[i].Rule < fs[j].Rule
		}
		return fs[i].Name < fs[j].Name
	})
	clean := resp.Type().New()
	clean.Set(rf, protoreflect.ValueOfMessage((&v1.Rejection{Failures: fs}).ProtoReflect()))
	out.ProtoReflect().Set(f, protoreflect.ValueOfMessage(clean))
	return out
}
