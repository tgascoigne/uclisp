package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var boolTests = BasicTests{
	{"(not nil)", uclisp.True},
	{"(not t)", uclisp.Nil},
}

func TestBoolean(t *testing.T) {
	boolTests.Do(t)
}
