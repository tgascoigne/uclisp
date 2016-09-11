package bootstrap

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/tgascoigne/uclisp/vm"
)

func TestCompilerQuoted(t *testing.T) {
	tests := []string{
		"(foo 1 2)",
		"(foo bar baz)",
		"foo",
		"20",
		"(a (b c) (d e (f)))",
	}
	for i, tc := range tests {
		tc := tc
		t.Run(fmt.Sprintf("Quote%v", i), func(t *testing.T) {
			quoted := fmt.Sprintf("(quote %v)", tc)
			reader := NewReader("<test>", strings.NewReader(quoted))
			el, err := reader.ReadElem()
			if err != nil {
				t.Error(err)
			}

			vm := vm.NewVM()

			code := Compile(vm, el)
			t.Logf("compiled %v -> %v", quoted, code)

			result := vm.Eval(code)
			assert.Equal(t, tc, fmt.Sprintf("%v", result))
		})
	}
}
