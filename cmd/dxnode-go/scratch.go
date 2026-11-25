package main

import (
	"fmt"
	"os"
	"sync"

	"github.com/blinklabs-io/dingo/dxoracle"
)

// scratch is the process-wide scratch database for SQL-backed functions.
// Opening it runs dingo's schema migrations (about 0.6 s), so it is opened
// on first use rather than at startup. Each call seeds, queries and rolls
// back inside one transaction.
var scratch struct {
	once sync.Once
	dir  string
	db   *dxoracle.Oracle
	err  error
}

func scratchOracle() (*dxoracle.Oracle, error) {
	scratch.once.Do(func() {
		dir, err := os.MkdirTemp("", "dxnode-go-")
		if err != nil {
			scratch.err = fmt.Errorf("scratch database dir: %w", err)
			return
		}
		scratch.dir = dir
		scratch.db, scratch.err = dxoracle.Open(dir)
	})
	return scratch.db, scratch.err
}

func closeScratch() {
	if scratch.db != nil {
		_ = scratch.db.Close()
	}
	if scratch.dir != "" {
		_ = os.RemoveAll(scratch.dir)
	}
}
