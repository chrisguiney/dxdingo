// Command dxnode-go is the Go node under test: dingo and gouroboros
// functions behind the dxdingo stdin/stdout transport.
package main

import (
	"context"
	"fmt"
	"os"
	"strings"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// revisions is set at link time: -X main.revisions=dingo@<sha>,gouroboros@<sha>.
var revisions string

func newRegistry() *dispatch.Registry {
	r := dispatch.NewRegistry()
	dispatch.Register(r, metaInfo(r))
	dispatch.Register(r, memberReward)
	dispatch.Register(r, drepDistribution)
	return r
}

func metaInfo(r *dispatch.Registry) func(context.Context, *v1.MetaInfoRequest) (*v1.MetaInfoResponse, error) {
	return func(context.Context, *v1.MetaInfoRequest) (*v1.MetaInfoResponse, error) {
		var revs []string
		if revisions != "" {
			revs = strings.Split(revisions, ",")
		}
		return &v1.MetaInfoResponse{
			Implementation: "dingo",
			Revisions:      revs,
			Implemented:    r.Implemented(),
		}, nil
	}
}

func main() {
	err := newRegistry().Serve(context.Background(), os.Stdin, os.Stdout)
	closeScratch()
	if err != nil {
		fmt.Fprintln(os.Stderr, "dxnode-go:", err)
		os.Exit(1)
	}
}
