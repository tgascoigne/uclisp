package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

type BasicTest struct {
	Expression string
	Result     uclisp.Elem
}

func (tc BasicTest) Do(t *testing.T) {
	t.Logf("Testing\t%v\n\texpecting\t%v", tc.Expression, tc.Result)

	expr := uclisp.Parse("test", tc.Expression)

	// Create a new global env for each test case for isolation
	uclisp.Global = uclisp.NewBasicEnv(uclisp.Builtin)

	result := expr.Eval(uclisp.Global)
	if !uclisp.Equal(uclisp.Global, result, tc.Result) {
		t.Errorf("Value incorrect:\n\tgot\t\t%v\n\texpected\t%v", result, tc.Result)
	}
}

type BasicTests []BasicTest

func (tcs BasicTests) Do(t *testing.T) {
	for _, tc := range tcs {
		tc.Do(t)
	}
}
