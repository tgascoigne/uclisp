package uclisp_test

import (
	"strings"
	"testing"

	"github.com/tgascoigne/uclisp"
)

// macros are tested pretty thoroughly by the lisp test framework
// just test exceptional cases here

var macroExceptionTests = ExceptionTests{
	{"(macro)", uclisp.ErrArgCount},
	{"(macro (x y))", uclisp.ErrArgCount},
	{"(macro 1 `(+ 1 1))", uclisp.ErrNotAList},
	{"(macro (1 2) `(+ 1 1))", uclisp.ErrNotASymbol},
}

func TestMacro(t *testing.T) {
	macroExceptionTests.Do(t)
}

func TestMacroDebug(t *testing.T) {
	out := captureOutput(func() {
		BasicTest{"(progn (define *macro-debug* t) ((macro (x) x) (+ 1 1)))", uclisp.Integer(2)}.Do(t)
	})

	if !strings.Contains(out, "expanded to (+ 1 1)") {
		t.Errorf("*macro-debug* didn't debug expansions, out was %v")
	}
}
