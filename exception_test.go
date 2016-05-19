package uclisp_test

import (
	"strings"
	"testing"

	"github.com/tgascoigne/uclisp"
)

var stackTest = ExceptionTest{"(progn (define x 20) (set 'x 10) (let ((x missing-symbol)) x) x)", uclisp.ErrSymbolNotDefined}

func TestException(t *testing.T) {
	exception := stackTest.Do(t)
	if exception, ok := exception.(uclisp.Exception); ok {
		if exception.Trace().Size() == 0 {
			t.Errorf("missing stack trace")
		}

		if exception.Trace().Size() != 4 {
			t.Errorf("unexpected stack trace size")
		}
	}
}

func TestExceptionTrace(t *testing.T) {
	exception := stackTest.Do(t)
	if exception, ok := exception.(uclisp.Exception); ok {
		dump := exception.Trace().Dump(uclisp.BacktraceDepth)

		if dump == "" {
			t.Errorf("missing stack trace")
		}

		if strings.Count(dump, "\n") != 4 {
			t.Errorf("unexpected stack trace size")
		}

		if !strings.Contains(exception.Error(), dump) {
			t.Errorf("error string missing stack trace")
		}
	}
}

func TestExceptionEllipsis(t *testing.T) {
	exception := stackTest.Do(t)
	if exception, ok := exception.(uclisp.Exception); ok {
		dump := exception.Trace().Dump(1)

		if !strings.Contains(dump, "...") {
			t.Errorf("trace missing ellipsis")
		}

		dump = exception.Trace().Dump(0)

		if !strings.Contains(dump, "...") {
			t.Errorf("trace missing ellipsis")
		}
	}
}
