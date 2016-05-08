package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var compTests = BasicTests{
	{"(eq nil nil)", uclisp.True},
	{"(eq nil t)", uclisp.Nil},
}

func TestComparisons(t *testing.T) {
	compTests.Do(t)
}
