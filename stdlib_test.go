package uclisp_test

import "testing"

func TestStd(t *testing.T) {
	DoLispTest("lib/unittest.lisp", t)
}
