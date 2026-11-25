package main

import (
	"fmt"
	"math/big"

	v1 "github.com/chrisguiney/dxdingo/gen/go/dxdingo/v1"
	"github.com/chrisguiney/dxdingo/internal/convert"
	"github.com/chrisguiney/dxdingo/internal/dispatch"
)

// Domain checks shared with dxnode-hs. An input the reference type system
// cannot represent (a UnitInterval above one, a zero share denominator) is
// UNSUPPORTED on both nodes, so the pair is skipped rather than one side
// crashing.

var (
	maxUint64 = new(big.Int).SetUint64(^uint64(0))
	ratOne    = big.NewRat(1, 1)
)

// ratArg decodes a required rational; malformed input is BAD_REQUEST.
func ratArg(name string, r *v1.Rational) (*big.Rat, error) {
	x, err := convert.Rat(r)
	if err != nil {
		return nil, dispatch.BadRequest(fmt.Errorf("%s: %w", name, err))
	}
	return x, nil
}

func fitsWord64Ratio(x *big.Rat) bool {
	return x.Num().CmpAbs(maxUint64) <= 0 && x.Denom().Cmp(maxUint64) <= 0
}

// unitInterval checks a value representable as a Haskell UnitInterval:
// [0, 1], numerator and denominator within Word64.
func unitInterval(name string, x *big.Rat) error {
	if x.Sign() < 0 || x.Cmp(ratOne) > 0 {
		return dispatch.Unsupported(fmt.Errorf("%s outside [0, 1]", name))
	}
	if !fitsWord64Ratio(x) {
		return dispatch.Unsupported(fmt.Errorf("%s does not fit a Word64 ratio", name))
	}
	return nil
}

// positiveUnitInterval is unitInterval excluding zero (ActiveSlotCoeff).
func positiveUnitInterval(name string, x *big.Rat) error {
	if x.Sign() == 0 {
		return dispatch.Unsupported(fmt.Errorf("%s is zero", name))
	}
	return unitInterval(name, x)
}

// nonNegativeInterval checks a value representable as NonNegativeInterval.
func nonNegativeInterval(name string, x *big.Rat) error {
	if x.Sign() < 0 {
		return dispatch.Unsupported(fmt.Errorf("%s is negative", name))
	}
	if !fitsWord64Ratio(x) {
		return dispatch.Unsupported(fmt.Errorf("%s does not fit a Word64 ratio", name))
	}
	return nil
}

// nonzero checks the denominators of StakeShare-style ratios.
func nonzero(pairs ...any) error {
	for i := 0; i+1 < len(pairs); i += 2 {
		if pairs[i+1].(uint64) == 0 {
			return dispatch.Unsupported(fmt.Errorf("%s must be nonzero", pairs[i]))
		}
	}
	return nil
}
