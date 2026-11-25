package wire_test

import (
	"bytes"
	"errors"
	"io"
	"testing"

	"github.com/chrisguiney/dxdingo/internal/wire"
)

func TestFrameHeaderIsBigEndianUint32Length(t *testing.T) {
	var buf bytes.Buffer
	if err := wire.WriteFrame(&buf, []byte("abc")); err != nil {
		t.Fatal(err)
	}
	want := []byte{0, 0, 0, 3, 'a', 'b', 'c'}
	if !bytes.Equal(buf.Bytes(), want) {
		t.Fatalf("frame = %x, want %x", buf.Bytes(), want)
	}
}

func TestReadFrameRoundTripsConsecutiveFrames(t *testing.T) {
	var buf bytes.Buffer
	for _, p := range [][]byte{[]byte("one"), {}, []byte("three")} {
		if err := wire.WriteFrame(&buf, p); err != nil {
			t.Fatal(err)
		}
	}
	for _, want := range []string{"one", "", "three"} {
		got, err := wire.ReadFrame(&buf)
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != want {
			t.Fatalf("got %q, want %q", got, want)
		}
	}
	if _, err := wire.ReadFrame(&buf); !errors.Is(err, io.EOF) {
		t.Fatalf("after last frame: err = %v, want io.EOF", err)
	}
}

func TestReadFrameTruncation(t *testing.T) {
	for name, in := range map[string][]byte{
		"header": {0, 0},
		"body":   {0, 0, 0, 5, 'a', 'b'},
	} {
		t.Run(name, func(t *testing.T) {
			_, err := wire.ReadFrame(bytes.NewReader(in))
			if !errors.Is(err, io.ErrUnexpectedEOF) {
				t.Fatalf("err = %v, want io.ErrUnexpectedEOF", err)
			}
		})
	}
}

func TestReadFrameRejectsOversizedLengthBeforeAllocating(t *testing.T) {
	_, err := wire.ReadFrame(bytes.NewReader([]byte{0xff, 0xff, 0xff, 0xff}))
	if !errors.Is(err, wire.ErrFrameTooLarge) {
		t.Fatalf("err = %v, want ErrFrameTooLarge", err)
	}
}
