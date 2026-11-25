// Command dxfuzz drives differential runs: it starts two nodes under test,
// sends each the same calls, and reports where their replies disagree.
//
//	dxfuzz -a bin/dxnode-go -b bin/dxnode-hs info
//	dxfuzz -a bin/dxnode-go -b bin/dxnode-hs replay corpus/
package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strings"

	"google.golang.org/protobuf/encoding/prototext"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/compare"
	"github.com/chrisguiney/dxdingo/internal/driver"
	"github.com/chrisguiney/dxdingo/internal/node"
)

func main() {
	a := flag.String("a", "bin/dxnode-go", "command for node A (space-separated)")
	b := flag.String("b", "bin/dxnode-hs", "command for node B (space-separated)")
	flag.Usage = func() {
		fmt.Fprintln(os.Stderr, "usage: dxfuzz [-a cmd] [-b cmd] info | replay <dir>")
		flag.PrintDefaults()
	}
	flag.Parse()
	if err := run(context.Background(), strings.Fields(*a), strings.Fields(*b), flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, "dxfuzz:", err)
		os.Exit(1)
	}
}

func run(ctx context.Context, a, b, args []string) error {
	if len(args) == 0 {
		flag.Usage()
		return fmt.Errorf("missing subcommand")
	}
	na, err := node.Start(ctx, a)
	if err != nil {
		return err
	}
	defer na.Close()
	nb, err := node.Start(ctx, b)
	if err != nil {
		return err
	}
	defer nb.Close()

	switch args[0] {
	case "info":
		for _, n := range []*node.Client{na, nb} {
			rep, err := n.Call(ctx, &v1.Call{Id: 1, Request: &v1.Call_MetaInfo{MetaInfo: &v1.MetaInfoRequest{}}})
			if err != nil {
				return err
			}
			fmt.Println(prototext.Format(rep))
		}
		return nil
	case "replay":
		if len(args) != 2 {
			return fmt.Errorf("replay needs a corpus directory")
		}
		calls, err := driver.LoadCorpus(args[1])
		if err != nil {
			return err
		}
		sum, err := driver.Run(ctx, na, nb, calls)
		if err != nil {
			return err
		}
		for _, f := range append(sum.Mismatches, sum.Failures...) {
			fmt.Printf("%s %s\ncall: %s\n%s\n", f.Result.Verdict, f.Result.Function, prototext.Format(f.Call), f.Result.Diff)
		}
		fmt.Printf("match=%d mismatch=%d skipped=%d harness-failure=%d\n",
			sum.Counts[compare.Match], sum.Counts[compare.Mismatch],
			sum.Counts[compare.Skipped], sum.Counts[compare.HarnessFailure])
		if len(sum.Mismatches)+len(sum.Failures) > 0 {
			return fmt.Errorf("%d mismatches, %d harness failures", len(sum.Mismatches), len(sum.Failures))
		}
		return nil
	default:
		return fmt.Errorf("unknown subcommand %q", args[0])
	}
}
