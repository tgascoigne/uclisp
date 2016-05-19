package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

// The while builtin is tested more thoroughly by the lisp based unit tests.

var iterExceptionTests = ExceptionTests{
	{"(while)", uclisp.ErrArgCount},
}

func TestIter(t *testing.T) {
	iterExceptionTests.Do(t)
}
