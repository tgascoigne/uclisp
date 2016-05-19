package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var typeassertTests = BasicTests{
	{"(let ((foo \"bar\")) (stringp foo))", uclisp.True},
	{"(stringp \"a\")", uclisp.True},
	{"(stringp 0)", uclisp.Nil},
	{"(stringp ())", uclisp.Nil},
	{"(stringp 'foo)", uclisp.Nil},
	{"(stringp (lambda (x) x))", uclisp.Nil},

	{"(let ((foo 'bar)) (symbolp foo))", uclisp.True},
	{"(symbolp \"a\")", uclisp.Nil},
	{"(symbolp 0)", uclisp.Nil},
	{"(symbolp ())", uclisp.Nil},
	{"(symbolp 'foo)", uclisp.True},
	{"(symbolp (lambda (x) x))", uclisp.Nil},

	{"(let ((foo ())) (listp foo))", uclisp.True},
	{"(listp \"a\")", uclisp.Nil},
	{"(listp 0)", uclisp.Nil},
	{"(listp ())", uclisp.True},
	{"(listp 'foo)", uclisp.Nil},
	{"(listp (lambda (x) x))", uclisp.Nil},

	{"(let ((foo 1)) (integerp foo))", uclisp.True},
	{"(integerp \"a\")", uclisp.Nil},
	{"(integerp 0)", uclisp.True},
	{"(integerp ())", uclisp.Nil},
	{"(integerp 'foo)", uclisp.Nil},
	{"(integerp (lambda (x) x))", uclisp.Nil},

	{"(let ((foo (lambda (x) x))) (procedurep foo))", uclisp.True},
	{"(procedurep \"a\")", uclisp.Nil},
	{"(procedurep 0)", uclisp.Nil},
	{"(procedurep ())", uclisp.Nil},
	{"(procedurep 'foo)", uclisp.Nil},
	{"(procedurep (lambda (x) x))", uclisp.True},
}

var typeassertExceptionTests = ExceptionTests{
	{"(stringp)", uclisp.ErrArgCount},
	{"(stringp x x)", uclisp.ErrArgCount},

	{"(symbolp)", uclisp.ErrArgCount},
	{"(symbolp x x)", uclisp.ErrArgCount},

	{"(listp)", uclisp.ErrArgCount},
	{"(listp x x)", uclisp.ErrArgCount},

	{"(integerp)", uclisp.ErrArgCount},
	{"(integerp x x)", uclisp.ErrArgCount},

	{"(procedurep)", uclisp.ErrArgCount},
	{"(procedurep x x)", uclisp.ErrArgCount},
}

func TestTypeAssert(t *testing.T) {
	typeassertTests.Do(t)
	typeassertExceptionTests.Do(t)
}
