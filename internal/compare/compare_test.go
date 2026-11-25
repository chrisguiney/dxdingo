package compare_test

import (
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/compare"
)

func fee(id, f uint64) *v1.Reply {
	return &v1.Reply{Id: id, Response: &v1.Reply_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeResponse{Fee: f}}}
}

func rejected(fs ...*v1.Failure) *v1.Reply {
	return &v1.Reply{Response: &v1.Reply_FeeScriptFee{FeeScriptFee: &v1.FeeScriptFeeResponse{
		Rejection: &v1.Rejection{Failures: fs},
	}}}
}

func harness(code v1.HarnessErrorCode) *v1.Reply {
	return &v1.Reply{Error: &v1.HarnessError{Code: code, Message: "x"}}
}

func TestVerdicts(t *testing.T) {
	unimpl := v1.HarnessErrorCode_HARNESS_ERROR_CODE_UNIMPLEMENTED
	internal := v1.HarnessErrorCode_HARNESS_ERROR_CODE_INTERNAL
	for name, tc := range map[string]struct {
		a, b *v1.Reply
		want compare.Verdict
	}{
		"equal outputs, ids differ": {fee(1, 5), fee(2, 5), compare.Match},
		"different outputs":         {fee(1, 5), fee(1, 6), compare.Mismatch},
		"rejections ignore order and detail": {
			rejected(&v1.Failure{Rule: "UTXO", Name: "A", Detail: "x"}, &v1.Failure{Rule: "UTXO", Name: "B"}),
			rejected(&v1.Failure{Rule: "UTXO", Name: "B", Detail: "y"}, &v1.Failure{Rule: "UTXO", Name: "A"}),
			compare.Match,
		},
		"rejection names differ": {
			rejected(&v1.Failure{Rule: "UTXO", Name: "A"}),
			rejected(&v1.Failure{Rule: "UTXO", Name: "B"}),
			compare.Mismatch,
		},
		"duplicate failure counts once": {
			rejected(&v1.Failure{Rule: "UTXO", Name: "A"}, &v1.Failure{Rule: "UTXO", Name: "A"}),
			rejected(&v1.Failure{Rule: "UTXO", Name: "A"}),
			compare.Match,
		},
		"accepted vs rejected":    {fee(1, 5), rejected(&v1.Failure{Name: "A"}), compare.Mismatch},
		"either unimplemented":    {harness(unimpl), fee(1, 5), compare.Skipped},
		"harness failure on one":  {fee(1, 5), harness(internal), compare.HarnessFailure},
		"harness failure on both": {harness(internal), harness(internal), compare.HarnessFailure},
	} {
		t.Run(name, func(t *testing.T) {
			if got := compare.Replies(tc.a, tc.b).Verdict; got != tc.want {
				t.Fatalf("verdict = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestMismatchNamesTheFunction(t *testing.T) {
	res := compare.Replies(fee(1, 5), fee(1, 6))
	if res.Function != "fee_script_fee" {
		t.Fatalf("Function = %q, want fee_script_fee", res.Function)
	}
}
