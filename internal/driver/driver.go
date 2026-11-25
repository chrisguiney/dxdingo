// Package driver sends the same calls to two nodes and compares replies.
package driver

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"google.golang.org/protobuf/encoding/prototext"
	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/compare"
)

// Node evaluates calls; node.Client implements it.
type Node interface {
	Call(context.Context, *v1.Call) (*v1.Reply, error)
}

// Finding is one disagreement, with what is needed to reproduce it.
type Finding struct {
	Call   *v1.Call
	A, B   *v1.Reply
	Result compare.Result
}

// Summary counts verdicts and keeps every mismatch and harness failure.
type Summary struct {
	Counts     map[compare.Verdict]int
	Mismatches []Finding
	Failures   []Finding
}

// Run sends each call to a then b. Call ids are reassigned sequentially.
// A transport error aborts the run.
func Run(ctx context.Context, a, b Node, calls []*v1.Call) (Summary, error) {
	sum := Summary{Counts: map[compare.Verdict]int{}}
	for i, c := range calls {
		call := proto.Clone(c).(*v1.Call)
		call.Id = uint64(i + 1)
		ra, err := a.Call(ctx, call)
		if err != nil {
			return sum, fmt.Errorf("node a: %w", err)
		}
		rb, err := b.Call(ctx, call)
		if err != nil {
			return sum, fmt.Errorf("node b: %w", err)
		}
		res := compare.Replies(ra, rb)
		sum.Counts[res.Verdict]++
		f := Finding{Call: c, A: ra, B: rb, Result: res}
		switch res.Verdict {
		case compare.Mismatch:
			sum.Mismatches = append(sum.Mismatches, f)
		case compare.HarnessFailure:
			sum.Failures = append(sum.Failures, f)
		}
	}
	return sum, nil
}

// LoadCorpus reads Calls from dir: *.txtpb as text protobuf, *.binpb as
// binary. Other files are ignored. Order is by file name.
func LoadCorpus(dir string) ([]*v1.Call, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}
	sort.Slice(entries, func(i, j int) bool { return entries[i].Name() < entries[j].Name() })
	var calls []*v1.Call
	for _, e := range entries {
		name := e.Name()
		var unmarshal func([]byte, proto.Message) error
		switch {
		case strings.HasSuffix(name, ".txtpb"):
			unmarshal = prototext.Unmarshal
		case strings.HasSuffix(name, ".binpb"):
			unmarshal = proto.Unmarshal
		default:
			continue
		}
		b, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil {
			return nil, err
		}
		var c v1.Call
		if err := unmarshal(b, &c); err != nil {
			return nil, fmt.Errorf("%s: %w", name, err)
		}
		calls = append(calls, &c)
	}
	return calls, nil
}
