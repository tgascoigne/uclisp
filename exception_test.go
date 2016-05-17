package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var stackTest = ExceptionTest{"(progn (define x 20) (set 'x 10) (let ((x missing-symbol)) x) x)", uclisp.ErrSymbolNotDefined}

func TestException(t *testing.T) {
	exception := stackTest.Do(t)
	if exception, ok := exception.(uclisp.Exception); ok {
		if exception.Trace().Depth() == 0 {
			t.Errorf("missing stack trace")
		}
	}
}
