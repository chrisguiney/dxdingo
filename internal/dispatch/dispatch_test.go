package dispatch_test

import (
	"bytes"
	"context"
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
	"github.com/chrisguiney/dxdingo/internal/wire"
)

func feeCall(id uint64) *v1.Call {
	return &v1.Call{Id: id, Request: &v1.Call_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeRequest{ExMem: 7}}}
}

func TestUnregisteredFunctionRepliesUnimplemented(t *testing.T) {
	r := dispatch.NewRegistry()
	got := r.Handle(context.Background(), feeCall(42))
	if got.GetId() != 42 || got.GetError().GetCode() != v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNIMPLEMENTED {
		t.Fatalf("reply = %v, want id 42 UNIMPLEMENTED", got)
	}
	if got.GetResponse() != nil {
		t.Fatalf("response set on error reply: %v", got.GetResponse())
	}
}

func TestRegisteredHandlerFillsMatchingReplyField(t *testing.T) {
	r := dispatch.NewRegistry()
	dispatch.Register(r, func(_ context.Context, req *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
		return &v1.FeeScriptFeeResponse{Fee: req.GetExMem() * 2}, nil
	})
	got := r.Handle(context.Background(), feeCall(1))
	if got.GetError() != nil {
		t.Fatalf("unexpected error: %v", got.GetError())
	}
	if fee := got.GetFeeScriptFee().GetFee(); fee != 14 {
		t.Fatalf("fee = %d, want 14", fee)
	}
}

func TestHandlerErrorsMapToHarnessCodes(t *testing.T) {
	for name, tc := range map[string]struct {
		err  error
		want v1.HarnessErrorCode
	}{
		"plain":       {errors.New("boom"), v1.HarnessErrorCode_HARNESS_ERROR_CODE_INTERNAL},
		"bad request": {dispatch.BadRequest(errors.New("no margin")), v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST},
		"unsupported": {dispatch.Unsupported(errors.New("byron")), v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNSUPPORTED},
	} {
		t.Run(name, func(t *testing.T) {
			r := dispatch.NewRegistry()
			dispatch.Register(r, func(context.Context, *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
				return nil, tc.err
			})
			got := r.Handle(context.Background(), feeCall(3))
			if got.GetError().GetCode() != tc.want || got.GetResponse() != nil {
				t.Fatalf("reply = %v, want code %v and no response", got, tc.want)
			}
		})
	}
}

func TestCallWithoutRequestIsBadRequest(t *testing.T) {
	got := dispatch.NewRegistry().Handle(context.Background(), &v1.Call{Id: 9})
	if got.GetId() != 9 || got.GetError().GetCode() != v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST {
		t.Fatalf("reply = %v, want id 9 BAD_REQUEST", got)
	}
}

func TestImplementedListsRegisteredEnvelopeFields(t *testing.T) {
	r := dispatch.NewRegistry()
	dispatch.Register(r, func(context.Context, *v1.FeeScriptFeeRequest) (*v1.FeeScriptFeeResponse, error) {
		return &v1.FeeScriptFeeResponse{}, nil
	})
	got := r.Implemented()
	if len(got) != 1 || got[0] != "fee_script_fee" {
		t.Fatalf("Implemented() = %v, want [fee_script_fee]", got)
	}
}

func TestServeAnswersEachFrameAndSurvivesGarbage(t *testing.T) {
	r := dispatch.NewRegistry()
	var in bytes.Buffer
	good, _ := proto.Marshal(feeCall(5))
	_ = wire.WriteFrame(&in, []byte{0xff, 0xff, 0xff}) // not a Call
	_ = wire.WriteFrame(&in, good)

	var out bytes.Buffer
	if err := r.Serve(context.Background(), &in, &out); err != nil {
		t.Fatal(err)
	}
	var codes []v1.HarnessErrorCode
	for {
		b, err := wire.ReadFrame(&out)
		if err != nil {
			break
		}
		var rep v1.Reply
		if err := proto.Unmarshal(b, &rep); err != nil {
			t.Fatal(err)
		}
		codes = append(codes, rep.GetError().GetCode())
	}
	want := []v1.HarnessErrorCode{
		v1.HarnessErrorCode_HARNESS_ERROR_CODE_BAD_REQUEST,
		v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNIMPLEMENTED,
	}
	if len(codes) != 2 || codes[0] != want[0] || codes[1] != want[1] {
		t.Fatalf("codes = %v, want %v", codes, want)
	}
}
