package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

type foo struct {
	W  []int
	X  bar
	X2 *bar
}

type bar struct {
	Y int
	Z string
}

func (b *bar) DoBar(y int) {
	b.Y = y
}

var gobindTests = BasicTests{
	{"(defined native-obj)", uclisp.True},
	{"(with native-obj (defined X))", uclisp.True},
	{"(with native-obj (defined Z))", uclisp.Nil},
	{"(with native-obj (with X Y))", uclisp.Integer(20)},
	{"(with native-obj (with X2 (DoBar 40) Y))", uclisp.Integer(40)},
	//	{"(with native-obj (with X (DoBar 40) Y))", uclisp.Integer(40)},
	{"(with native-obj (with X Z))", uclisp.String("hello world")},
}

var gobindExceptionTests = ExceptionTests{}

func TestGoBindings(t *testing.T) {
	val := foo{
		W: []int{1, 2, 3},
		X: bar{
			Y: 20,
			Z: "hello world",
		},
	}

	val.X2 = &val.X

	env := uclisp.NewBasicEnv(uclisp.Builtin)
	env.Define(uclisp.Symbol("native-obj"), uclisp.Bind(val))

	gobindTests.DoWithEnvironment(t, env)
	gobindExceptionTests.Do(t)
}

var someFoo foo

var goTypeconvTests = []struct {
	GoVal   interface{}
	LispVal uclisp.Elem
}{
	{int(20), uclisp.Integer(20)},
	{string("foo"), uclisp.String("foo")},
}

func TestGoTypeconv(t *testing.T) {
	for _, tc := range goTypeconvTests {
		outElem := uclisp.Bind(tc.GoVal)
		if outElem != tc.LispVal {
			t.Errorf("Type binding failed. Input %v, Expected %v, got %v", tc.GoVal, tc.LispVal, outElem)
		}

		outVal := uclisp.Unbind(tc.LispVal)
		if outVal != tc.GoVal {
			t.Errorf("Type unbinding failed. Input %v, Expected %v, got %v", tc.LispVal, tc.GoVal, outVal)
		}
	}
}
