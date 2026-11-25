// Package dispatch routes envelope Calls to typed function handlers and
// serves them over the framed stdin/stdout transport.
package dispatch

import (
	"context"
	"errors"
	"fmt"
	"io"
	"sort"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/wire"
)

type handler func(context.Context, proto.Message) (proto.Message, error)

// Registry maps request message types to handlers.
type Registry struct {
	handlers map[protoreflect.FullName]handler
}

// NewRegistry returns an empty registry; every function replies
// UNIMPLEMENTED until registered.
func NewRegistry() *Registry {
	return &Registry{handlers: map[protoreflect.FullName]handler{}}
}

// Register installs fn as the handler for its request type. Registering the
// same request type twice panics: it is a wiring bug.
func Register[Req, Resp proto.Message](r *Registry, fn func(context.Context, Req) (Resp, error)) {
	var zero Req
	name := zero.ProtoReflect().Descriptor().FullName()
	if _, dup := r.handlers[name]; dup {
		panic(fmt.Sprintf("dispatch: %s registered twice", name))
	}
	r.handlers[name] = func(ctx context.Context, m proto.Message) (proto.Message, error) {
		return fn(ctx, m.(Req))
	}
}

var (
	callOneof  = (&v1.Call{}).ProtoReflect().Descriptor().Oneofs().ByName("request")
	replyOneof = (&v1.Reply{}).ProtoReflect().Descriptor().Oneofs().ByName("response")
)

// Implemented returns the envelope field names that have handlers, sorted.
func (r *Registry) Implemented() []string {
	var out []string
	fields := callOneof.Fields()
	for i := range fields.Len() {
		if _, ok := r.handlers[fields.Get(i).Message().FullName()]; ok {
			out = append(out, string(fields.Get(i).Name()))
		}
	}
	sort.Strings(out)
	return out
}

// Handle evaluates one call. It always returns a reply carrying the call id.
func (r *Registry) Handle(ctx context.Context, call *v1.Call) *v1.Reply {
	reply := &v1.Reply{Id: call.GetId()}
	cm := call.ProtoReflect()
	field := cm.WhichOneof(callOneof)
	if field == nil {
		reply.Error = harnessError(v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST, "call has no request")
		return reply
	}
	h, ok := r.handlers[field.Message().FullName()]
	if !ok {
		reply.Error = harnessError(v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNIMPLEMENTED, string(field.Name()))
		return reply
	}
	resp, err := h(ctx, cm.Get(field).Message().Interface())
	if err != nil {
		reply.Error = classify(err)
		return reply
	}
	rf := replyOneof.Fields().ByNumber(field.Number())
	if rf == nil || rf.Message().FullName() != resp.ProtoReflect().Descriptor().FullName() {
		reply.Error = harnessError(v1.HarnessErrorCode_HARNESS_ERROR_CODE_INTERNAL,
			fmt.Sprintf("handler for %s returned %s", field.Name(), resp.ProtoReflect().Descriptor().FullName()))
		return reply
	}
	reply.ProtoReflect().Set(rf, protoreflect.ValueOfMessage(resp.ProtoReflect()))
	return reply
}

// Serve answers framed Calls from in until it ends cleanly. An undecodable
// frame gets a BAD_REQUEST reply with id 0; serving continues.
func (r *Registry) Serve(ctx context.Context, in io.Reader, out io.Writer) error {
	for {
		frame, err := wire.ReadFrame(in)
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return err
		}
		var reply *v1.Reply
		var call v1.Call
		if err := proto.Unmarshal(frame, &call); err != nil {
			reply = &v1.Reply{Error: harnessError(v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST, err.Error())}
		} else {
			reply = r.Handle(ctx, &call)
		}
		b, err := proto.Marshal(reply)
		if err != nil {
			return fmt.Errorf("dispatch: marshal reply: %w", err)
		}
		if err := wire.WriteFrame(out, b); err != nil {
			return err
		}
	}
}

type codedError struct {
	code v1.HarnessErrorCode
	err  error
}

func (e *codedError) Error() string { return e.err.Error() }
func (e *codedError) Unwrap() error { return e.err }

// BadRequest marks a handler error as a malformed request.
func BadRequest(err error) error {
	return &codedError{v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST, err}
}

// Unsupported marks inputs the implementation does not handle, such as an
// era it does not model.
func Unsupported(err error) error {
	return &codedError{v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED, err}
}

func classify(err error) *v1.HarnessError {
	var ce *codedError
	if errors.As(err, &ce) {
		return harnessError(ce.code, err.Error())
	}
	return harnessError(v1.HarnessErrorCode_HARNESS_ERROR_CODE_INTERNAL, err.Error())
}

func harnessError(code v1.HarnessErrorCode, msg string) *v1.HarnessError {
	return &v1.HarnessError{Code: code, Message: msg}
}
