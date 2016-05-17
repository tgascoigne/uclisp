package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var throwTests = BasicTests{
	{"(catch 'foo (let ((x 20)) (throw 'foo 1) x))", uclisp.Integer(1)},
	{"(let ((x 20)) (catch 'foo (throw 'foo 20) (setq x 1)) x)", uclisp.Integer(20)},
}

func TestThrow(t *testing.T) {
	throwTests.Do(t)
}
