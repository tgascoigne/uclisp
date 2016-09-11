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

func TestCompilerDefine(t *testing.T) {
	// define foo
	test := "(define foo 200)"

	reader := NewReader("<test>", strings.NewReader(test))
	el, err := reader.ReadElem()
	if err != nil {
		t.Error(err)
	}

	mach := vm.NewVM()

	code := Compile(mach, el)
	t.Logf("compiled %v -> %v", test, code)

	result := mach.Eval(code)
	assert.Equal(t, vm.Int(200), result)

	// lookup foo
	test = "foo"

	reader = NewReader("<test>", strings.NewReader(test))
	el, err = reader.ReadElem()
	if err != nil {
		t.Error(err)
	}

	code = Compile(mach, el)
	t.Logf("compiled %v -> %v", test, code)

	result = mach.Eval(code)
	assert.Equal(t, vm.Int(200), result)
}

func TestCompilerSet(t *testing.T) {
	// define foo
	test := "(define foo 200)"

	reader := NewReader("<test>", strings.NewReader(test))
	el, err := reader.ReadElem()
	if err != nil {
		t.Error(err)
	}

	mach := vm.NewVM()

	code := Compile(mach, el)
	t.Logf("compiled %v -> %v", test, code)

	result := mach.Eval(code)
	assert.Equal(t, vm.Int(200), result)

	// set foo
	test = "(set foo 201)"

	reader = NewReader("<test>", strings.NewReader(test))
	el, err = reader.ReadElem()
	if err != nil {
		t.Error(err)
	}

	code = Compile(mach, el)
	t.Logf("compiled %v -> %v", test, code)

	result = mach.Eval(code)
	assert.Equal(t, vm.Int(201), result)

	// lookup foo
	test = "foo"

	reader = NewReader("<test>", strings.NewReader(test))
	el, err = reader.ReadElem()
	if err != nil {
		t.Error(err)
	}

	code = Compile(mach, el)
	t.Logf("compiled %v -> %v", test, code)

	result = mach.Eval(code)
	assert.Equal(t, vm.Int(201), result)
}
