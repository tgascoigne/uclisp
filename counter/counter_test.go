package counter

import (
	"testing"
)

func TestCounter(t *testing.T) {
	counter := New()
	for i := 0; i < 10; i++ {
		x := <-counter
		if x != i {
			t.Error("Generated ID mismatch")
		}
	}
}
