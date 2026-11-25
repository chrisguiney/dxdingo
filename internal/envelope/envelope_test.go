package envelope_test

import (
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// The service, Call and Reply are maintained by hand once generated; this
// keeps the three in step: same functions, same field numbers, matching
// request and response types.
func TestEnvelopeMatchesService(t *testing.T) {
	svc := v1.File_dxdingo_v1_service_proto.Services().ByName("NodeService")
	call := (&v1.Call{}).ProtoReflect().Descriptor().Oneofs().ByName("request")
	reply := (&v1.Reply{}).ProtoReflect().Descriptor().Oneofs().ByName("response")

	methods := svc.Methods()
	if methods.Len() != call.Fields().Len() || methods.Len() != reply.Fields().Len() {
		t.Fatalf("methods=%d call fields=%d reply fields=%d",
			methods.Len(), call.Fields().Len(), reply.Fields().Len())
	}
	byInput := map[protoreflect.FullName]protoreflect.MethodDescriptor{}
	for i := range methods.Len() {
		byInput[methods.Get(i).Input().FullName()] = methods.Get(i)
	}
	for i := range call.Fields().Len() {
		cf := call.Fields().Get(i)
		m, ok := byInput[cf.Message().FullName()]
		if !ok {
			t.Errorf("Call.%s: no service method takes %s", cf.Name(), cf.Message().FullName())
			continue
		}
		rf := reply.Fields().ByNumber(cf.Number())
		if rf == nil {
			t.Errorf("Call.%s = %d: no Reply field with that number", cf.Name(), cf.Number())
			continue
		}
		if rf.Name() != cf.Name() {
			t.Errorf("field %d: Call.%s but Reply.%s", cf.Number(), cf.Name(), rf.Name())
		}
		if rf.Message().FullName() != m.Output().FullName() {
			t.Errorf("Reply.%s is %s, method %s returns %s",
				rf.Name(), rf.Message().FullName(), m.Name(), m.Output().FullName())
		}
	}
}

// Every response reserves field 1 for the rejection so callers and the
// comparator can find it without per-function code.
func TestEveryResponseHasRejectionAtField1(t *testing.T) {
	reply := (&v1.Reply{}).ProtoReflect().Descriptor().Oneofs().ByName("response")
	for i := range reply.Fields().Len() {
		msg := reply.Fields().Get(i).Message()
		if msg.FullName() == "dxdingo.v1.MetaInfoResponse" {
			continue
		}
		f := msg.Fields().ByNumber(1)
		if f == nil || f.Name() != "rejection" || f.Message() == nil ||
			f.Message().FullName() != "dxdingo.v1.Rejection" {
			t.Errorf("%s: field 1 is not `Rejection rejection`", msg.FullName())
		}
	}
}
