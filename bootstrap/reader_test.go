package bootstrap

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/tgascoigne/uclisp/vm"
)

func TestReader(t *testing.T) {
	tests := []struct {
		Name     string
		Input    string
		Expected vm.Cell
	}{
		{
			Name:     "Read1",
			Input:    "(foo 1 2)",
			Expected: vm.List(vm.Symbol("foo"), vm.Int(1), vm.Int(2)),
		},
		{
			Name:     "Read2",
			Input:    "(foo (12 x)'y)",
			Expected: vm.List(vm.Symbol("foo"), vm.List(vm.Int(12), vm.Symbol("x")), vm.Symbol("'"), vm.Symbol("y")),
		},
	}

	for _, tc := range tests {
		tc := tc // capture range variable
		t.Run(tc.Name, func(t *testing.T) {
			reader := NewReader("<test>", strings.NewReader(tc.Input))
			elem, err := reader.ReadElem()

			assert.Equal(t, tc.Expected, elem)

			_, err = reader.ReadElem()
			assert.Equal(t, ErrUnexpectedEOF, err, "expected EOF")
		})
	}
}

func TestReaderEOF(t *testing.T) {
	reader := NewReader("<test>", strings.NewReader("(invalid-expr 1 2"))
	_, err := reader.ReadElem()
	assert.Equal(t, ErrUnexpectedEOF, err, "expected EOF")
}

func TestReaderUnexpectedRParn(t *testing.T) {
	reader := NewReader("<test>", strings.NewReader(")"))
	_, err := reader.ReadElem()
	assert.Equal(t, ErrUnexpectedRParen, err, "expected ErrUnexpectedRParen")
}
