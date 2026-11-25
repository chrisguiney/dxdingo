package node_test

import (
	"context"
	"os"
	"testing"
	"time"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/node"
)

// The test binary doubles as a fake node when DXDINGO_FAKE_NODE is set.
func TestMain(m *testing.M) {
	switch os.Getenv("DXDINGO_FAKE_NODE") {
	case "serve":
		r := dispatch.NewRegistry()
		dispatch.Register(r, func(_ context.Context, q *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
			return &v1.FeeScriptFeeResponse{Fee: q.GetExMem() + 1}, nil
		})
		if err := r.Serve(context.Background(), os.Stdin, os.Stdout); err != nil {
			os.Exit(2)
		}
		os.Exit(0)
	case "crash":
		os.Exit(3)
	}
	os.Exit(m.Run())
}

func start(t *testing.T, mode string) *node.Client {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	t.Cleanup(cancel)
	c, err := node.Start(ctx, []string{os.Args[0]}, "DXDINGO_FAKE_NODE="+mode)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = c.Close() })
	return c
}

func TestCallRoundTripsThroughSubprocess(t *testing.T) {
	c := start(t, "serve")
	for i := uint64(1); i <= 3; i++ {
		rep, err := c.Call(context.Background(), &v1.Call{Id: i, Request: &v1.Call_FeeScriptFee{
			FeeScriptFee: &v1.FeeScriptFeeRequest{ExMem: i * 10},
		}})
		if err != nil {
			t.Fatal(err)
		}
		if rep.GetId() != i || rep.GetFeeScriptFee().GetFee() != i*10+1 {
			t.Fatalf("call %d: reply %v", i, rep)
		}
	}
	if err := c.Close(); err != nil {
		t.Fatalf("Close after clean EOF: %v", err)
	}
}

func TestCallFailsWhenNodeDies(t *testing.T) {
	c := start(t, "crash")
	_, err := c.Call(context.Background(), &v1.Call{Id: 1, Request: &v1.Call_MetaInfo{MetaInfo: &v1.MetaInfoRequest{}}})
	if err == nil {
		t.Fatal("Call to a dead node returned no error")
	}
}
