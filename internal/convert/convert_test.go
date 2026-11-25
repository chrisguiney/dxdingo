package convert_test

import (
	"math/big"
	"testing"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
)

func TestRatRoundTripReduces(t *testing.T) {
	got, err := convert.Rat(&v1.Rational{Numerator: "-6", Denominator: "4"})
	if err != nil {
		t.Fatal(err)
	}
	if got.Cmp(big.NewRat(-3, 2)) != 0 {
		t.Fatalf("got %v, want -3/2", got)
	}
	back := convert.RatProto(got)
	if back.GetNumerator() != "-3" || back.GetDenominator() != "2" {
		t.Fatalf("RatProto = %v, want -3/2", back)
	}
}

func TestRatRejectsMalformed(t *testing.T) {
	for name, r := range map[string]*v1.Rational{
		"nil":           nil,
		"zero den":      {Numerator: "1", Denominator: "0"},
		"negative den":  {Numerator: "1", Denominator: "-2"},
		"not a number":  {Numerator: "x", Denominator: "1"},
		"empty":         {},
		"fraction text": {Numerator: "1/2", Denominator: "1"},
	} {
		t.Run(name, func(t *testing.T) {
			if _, err := convert.Rat(r); err == nil {
				t.Fatalf("Rat(%v) accepted malformed input", r)
			}
		})
	}
}
