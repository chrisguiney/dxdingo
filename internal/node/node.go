// Package node runs a node under test as a subprocess and calls it over the
// framed stdin/stdout transport.
package node

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/wire"
)

// Client is one running node. Calls are serialized: the transport allows a
// single outstanding call.
type Client struct {
	mu    sync.Mutex
	cmd   *exec.Cmd
	stdin io.WriteCloser
	out   io.ReadCloser
}

// Start launches argv with extra environment entries. The node's stderr is
// passed through for diagnostics.
func Start(ctx context.Context, argv []string, env ...string) (*Client, error) {
	if len(argv) == 0 {
		return nil, errors.New("node: empty command")
	}
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Env = append(os.Environ(), env...)
	cmd.Stderr = os.Stderr
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, err
	}
	out, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("node: start %s: %w", argv[0], err)
	}
	return &Client{cmd: cmd, stdin: stdin, out: out}, nil
}

// Call sends one call and waits for its reply. A transport failure (the
// node died or replied out of turn) is an error; harness and domain
// outcomes are in the reply.
func (c *Client) Call(_ context.Context, call *v1.Call) (*v1.Reply, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	b, err := proto.Marshal(call)
	if err != nil {
		return nil, fmt.Errorf("node: marshal call: %w", err)
	}
	if err := wire.WriteFrame(c.stdin, b); err != nil {
		return nil, fmt.Errorf("node %s: %w", c.cmd.Path, err)
	}
	frame, err := wire.ReadFrame(c.out)
	if err != nil {
		return nil, fmt.Errorf("node %s: read reply: %w", c.cmd.Path, err)
	}
	var reply v1.Reply
	if err := proto.Unmarshal(frame, &reply); err != nil {
		return nil, fmt.Errorf("node %s: decode reply: %w", c.cmd.Path, err)
	}
	if reply.GetId() != call.GetId() {
		return nil, fmt.Errorf("node %s: reply id %d for call %d", c.cmd.Path, reply.GetId(), call.GetId())
	}
	return &reply, nil
}

// Close ends the node's input and waits for it to exit.
func (c *Client) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.stdin == nil {
		return nil
	}
	_ = c.stdin.Close()
	c.stdin = nil
	return c.cmd.Wait()
}
