package main

import "github.com/chrisguiney/dxdingo/internal/dispatch"

// registerRuleLedger registers the composed ledger rules: PPUP, GOV, LEDGER
// and BBODY.
func registerRuleLedger(r *dispatch.Registry) {
	dispatch.Register(r, rulePpup)
	dispatch.Register(r, ruleGov)
	dispatch.Register(r, ruleLedger)
	dispatch.Register(r, ruleBlockBody)
}
