// Package wire frames messages on the node transport: a 4-byte big-endian
// payload length followed by the payload.
package wire

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
)

// MaxFrameSize bounds a single payload. The length is read before the body,
// so a corrupt or hostile header must not trigger a 4 GiB allocation.
const MaxFrameSize = 256 << 20

// ErrFrameTooLarge reports a length header above MaxFrameSize.
var ErrFrameTooLarge = errors.New("wire: frame exceeds MaxFrameSize")

// WriteFrame writes one length-prefixed payload.
func WriteFrame(w io.Writer, payload []byte) error {
	if len(payload) > MaxFrameSize {
		return ErrFrameTooLarge
	}
	var hdr [4]byte
	binary.BigEndian.PutUint32(hdr[:], uint32(len(payload)))
	if _, err := w.Write(hdr[:]); err != nil {
		return fmt.Errorf("wire: write header: %w", err)
	}
	if _, err := w.Write(payload); err != nil {
		return fmt.Errorf("wire: write payload: %w", err)
	}
	return nil
}

// ReadFrame reads one length-prefixed payload. It returns io.EOF only when
// the stream ends cleanly between frames, and io.ErrUnexpectedEOF when it
// ends inside one.
func ReadFrame(r io.Reader) ([]byte, error) {
	var hdr [4]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		return nil, err
	}
	n := binary.BigEndian.Uint32(hdr[:])
	if n > MaxFrameSize {
		return nil, fmt.Errorf("%w: %d bytes", ErrFrameTooLarge, n)
	}
	buf := make([]byte, n)
	if _, err := io.ReadFull(r, buf); err != nil {
		if errors.Is(err, io.EOF) {
			return nil, io.ErrUnexpectedEOF
		}
		return nil, err
	}
	return buf, nil
}
