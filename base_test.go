package uclisp_test

import (
	"fmt"
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

	ctx := uclisp.NewContext()
	result := ctx.Begin(expr)
	if !uclisp.Equal(ctx, uclisp.Global, result, tc.Result) {
		t.Errorf("Value incorrect:\n\tgot\t\t%v\n\texpected\t%v", result, tc.Result)
	}
}

type BasicTests []BasicTest

func (tcs BasicTests) Do(t *testing.T) {
	for _, tc := range tcs {
		tc.Do(t)
	}
}

type ExceptionTest struct {
	Expression string
	Exception  error
}

func (tc ExceptionTest) Do(t *testing.T) (reterror error) {
	t.Logf("Testing\t%v\n\texpecting exception:\t%v", tc.Expression, tc.Exception)

	expr := uclisp.Parse("test", tc.Expression)

	// Create a new global env for each test case for isolation
	uclisp.Global = uclisp.NewBasicEnv(uclisp.Builtin)

	defer func() {
		if err, ok := reterror.(uclisp.Exception); ok {
			if err.BaseError() != tc.Exception {
				t.Errorf("Exception incorrect\n\tgot\t\t%v\n\texpected\t%v", err, tc.Exception)
			}
		} else {
			t.Errorf("Missing exception\n\texpected\t%v", tc.Exception)
		}
	}()

	defer func() {
		if err := recover(); err != nil {
			if err, ok := err.(uclisp.Exception); ok {
				reterror = err
			} else {
				panic(err)
			}
		}
	}()

	ctx := uclisp.NewContext()
	ctx.Begin(expr)
	return
}

type ExceptionTests []ExceptionTest

func (tcs ExceptionTests) Do(t *testing.T) {
	for _, tc := range tcs {
		tc.Do(t)
	}
}

func DoLispTest(path string, t *testing.T) {
	BasicTest{
		fmt.Sprintf("(load-file \"%v\")", path),
		uclisp.True,
	}.Do(t)
}
