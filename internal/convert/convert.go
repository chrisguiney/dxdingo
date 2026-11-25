// Package convert maps protobuf scalar types to Go values.
package convert

import (
	"errors"
	"fmt"
	"math/big"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
)

// Rat decodes a Rational. The denominator must be positive.
func Rat(r *v1.Rational) (*big.Rat, error) {
	if r == nil {
		return nil, errors.New("rational: missing")
	}
	num, ok := new(big.Int).SetString(r.GetNumerator(), 10)
	if !ok {
		return nil, fmt.Errorf("rational: numerator %q is not a base-10 integer", r.GetNumerator())
	}
	den, ok := new(big.Int).SetString(r.GetDenominator(), 10)
	if !ok {
		return nil, fmt.Errorf("rational: denominator %q is not a base-10 integer", r.GetDenominator())
	}
	if den.Sign() <= 0 {
		return nil, fmt.Errorf("rational: denominator %s is not positive", den)
	}
	return new(big.Rat).SetFrac(num, den), nil
}

// RatProto encodes a reduced Rational.
func RatProto(r *big.Rat) *v1.Rational {
	return &v1.Rational{Numerator: r.Num().String(), Denominator: r.Denom().String()}
}
