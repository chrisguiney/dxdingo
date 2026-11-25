package driver_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/compare"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/driver"
)

type local struct{ r *dispatch.Registry }

func (l local) Call(ctx context.Context, c *v1.Call) (*v1.Reply, error) { return l.r.Handle(ctx, c), nil }

func feeNode(offset uint64) local {
	r := dispatch.NewRegistry()
	dispatch.Register(r, func(_ context.Context, q *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
		return &v1.FeeScriptFeeResponse{Fee: q.GetExMem() + offset}, nil
	})
	return local{r}
}

func feeCall(mem uint64) *v1.Call {
	return &v1.Call{Request: &v1.Call_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeRequest{ExMem: mem}}}
}

func TestRunCountsVerdictsAndKeepsMismatches(t *testing.T) {
	calls := []*v1.Call{feeCall(1), feeCall(2), {Request: &v1.Call_MetaInfo{MetaInfo: &v1.MetaInfoRequest{}}}}
	sum, err := driver.Run(context.Background(), feeNode(0), feeNode(0), calls)
	if err != nil {
		t.Fatal(err)
	}
	if sum.Counts[compare.Match] != 2 || sum.Counts[compare.Skipped] != 1 || len(sum.Mismatches) != 0 {
		t.Fatalf("summary = %+v", sum)
	}

	sum, err = driver.Run(context.Background(), feeNode(0), feeNode(1), calls[:1])
	if err != nil {
		t.Fatal(err)
	}
	if sum.Counts[compare.Mismatch] != 1 || len(sum.Mismatches) != 1 {
		t.Fatalf("summary = %+v", sum)
	}
	if !proto.Equal(sum.Mismatches[0].Call, calls[0]) {
		t.Fatalf("mismatch does not carry its call: %v", sum.Mismatches[0].Call)
	}
}

func TestRunAssignsDistinctCallIDs(t *testing.T) {
	var seen []uint64
	r := dispatch.NewRegistry()
	dispatch.Register(r, func(context.Context, *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
		return &v1.FeeScriptFeeResponse{}, nil
	})
	rec := recorder{local{r}, &seen}
	if _, err := driver.Run(context.Background(), rec, rec, []*v1.Call{feeCall(1), feeCall(1)}); err != nil {
		t.Fatal(err)
	}
	if len(seen) != 4 || seen[0] == seen[2] {
		t.Fatalf("ids = %v, want two distinct ids each sent to both nodes", seen)
	}
}

type recorder struct {
	local
	ids *[]uint64
}

func (r recorder) Call(ctx context.Context, c *v1.Call) (*v1.Reply, error) {
	*r.ids = append(*r.ids, c.GetId())
	return r.local.Call(ctx, c)
}

func TestLoadCorpusReadsTextAndBinaryCalls(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "a.txtpb"), []byte("fee_script_fee { ex_mem: 5 }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	b, _ := proto.Marshal(feeCall(9))
	if err := os.WriteFile(filepath.Join(dir, "b.binpb"), b, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "README"), []byte("ignored"), 0o644); err != nil {
		t.Fatal(err)
	}
	calls, err := driver.LoadCorpus(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(calls) != 2 || calls[0].GetFeeScriptFee().GetExMem() != 5 || calls[1].GetFeeScriptFee().GetExMem() != 9 {
		t.Fatalf("calls = %v", calls)
	}
}
