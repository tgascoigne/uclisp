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
	t.Logf("Testing %v, expecting %v", tc.Expression, tc.Result)

	expr := uclisp.Parse("test", tc.Expression)

	t.Logf("%v", expr)

	// Create a new global env for each test case for isolation
	uclisp.Global = uclisp.NewBasicEnv(uclisp.Builtin)

	result := expr.Eval(uclisp.Global)
	if !result.Equals(uclisp.Global, tc.Result) {
		t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
	}
}

type BasicTests []BasicTest

func (tcs BasicTests) Do(t *testing.T) {
	for _, tc := range tcs {
		tc.Do(t)
	}
}
