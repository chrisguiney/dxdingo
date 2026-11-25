package main

import (
	"os"
	"testing"
)

// The scratch database is process-wide; remove it once the tests finish.
func TestMain(m *testing.M) {
	code := m.Run()
	closeScratch()
	os.Exit(code)
}
