package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

func TestTypeEqualsExceptions(t *testing.T) {
	ctx := uclisp.NewContext()

	ShouldThrow(t, uclisp.ErrNotAnInteger, func() {
		uclisp.Integer(4).Equals(ctx, uclisp.Global, uclisp.String("foo"))
	})

	ShouldThrow(t, uclisp.ErrNotAList, func() {
		uclisp.List{uclisp.Integer(1), uclisp.Integer(2)}.Equals(ctx, uclisp.Global, uclisp.String("foo"))
	})

	ShouldThrow(t, uclisp.ErrNotAString, func() {
		uclisp.String("foo").Equals(ctx, uclisp.Global, uclisp.Integer(4))
	})

	ShouldThrow(t, uclisp.ErrNotASymbol, func() {
		uclisp.Symbol("foo").Equals(ctx, uclisp.Global, uclisp.Integer(4))
	})

	ShouldThrow(t, uclisp.ErrIncomparable, func() {
		uclisp.Throw{}.Equals(ctx, uclisp.Global, uclisp.Throw{})
	})

	proc := uclisp.NewProcedure(func(ctx *uclisp.Context, env uclisp.Env, args []uclisp.Elem) uclisp.Elem {
		return uclisp.Nil
	})

	ShouldThrow(t, uclisp.ErrNotAProcedure, func() {
		proc.Equals(ctx, uclisp.Global, uclisp.Integer(4))
	})
}

func TestProcEquals(t *testing.T) {
	ctx := uclisp.NewContext()

	proc1 := uclisp.NewProcedure(func(ctx *uclisp.Context, env uclisp.Env, args []uclisp.Elem) uclisp.Elem {
		return uclisp.Nil
	})

	proc2 := uclisp.NewProcedure(func(ctx *uclisp.Context, env uclisp.Env, args []uclisp.Elem) uclisp.Elem {
		return uclisp.True
	})

	if proc1.Equals(ctx, uclisp.Global, proc2) {
		t.Errorf("proc1 == proc2")
	}

	if !proc1.Equals(ctx, uclisp.Global, proc1) {
		t.Errorf("proc1 != proc1")
	}
}

func TestTruthEquals(t *testing.T) {
	ctx := uclisp.NewContext()

	if uclisp.True.Equals(ctx, uclisp.Global, uclisp.Nil) != false {
		t.Errorf("True != Nil")
	}

	if uclisp.True.Equals(ctx, uclisp.Global, uclisp.True) != true {
		t.Errorf("True == True")
	}
}

func TestSelfEval(t *testing.T) {
	ctx := uclisp.NewContext()

	proc := uclisp.NewProcedure(func(ctx *uclisp.Context, env uclisp.Env, args []uclisp.Elem) uclisp.Elem {
		return uclisp.Nil
	})

	proc2 := proc.Eval(ctx, uclisp.Global)

	if !proc2.Equals(ctx, uclisp.Global, proc) {
		t.Errorf("proc didn't evaluate to proc. got %v and %v", proc, proc2)
	}

	if !uclisp.Nil.Eval(ctx, uclisp.Global).Equals(ctx, uclisp.Global, uclisp.Nil) {
		t.Errorf("Nil didn't evaluate to Nil")
	}

	if !uclisp.True.Eval(ctx, uclisp.Global).Equals(ctx, uclisp.Global, uclisp.True) {
		t.Errorf("True didn't evaluate to True")
	}
}

func TestTypeOf(t *testing.T) {
	if uclisp.TypeOf(uclisp.List{}) != uclisp.ListType {
		t.Errorf("TypeOf List != ListType")
	}

	if uclisp.TypeOf(uclisp.Integer(1)) != uclisp.IntegerType {
		t.Errorf("TypeOf Integer != IntegerType")
	}

	if uclisp.TypeOf(uclisp.String("")) != uclisp.StringType {
		t.Errorf("TypeOf String != StringType")
	}

	if uclisp.TypeOf(uclisp.Symbol("")) != uclisp.SymbolType {
		t.Errorf("TypeOf Symbol != SymbolType")
	}

	proc := uclisp.NewProcedure(func(ctx *uclisp.Context, env uclisp.Env, args []uclisp.Elem) uclisp.Elem {
		return uclisp.Nil
	})

	if uclisp.TypeOf(proc) != uclisp.ProcedureType {
		t.Errorf("TypeOf Procedure != ProcedureType")
	}

	if uclisp.TypeOf(dummyElem{}) != uclisp.UnknownType {
		t.Errorf("TypeOf dummyElem != UnknownType")
	}

}

type dummyElem struct{}

func (p dummyElem) Equals(ctx *uclisp.Context, env uclisp.Env, other uclisp.Elem) bool {
	return false
}

func (p dummyElem) Eval(ctx *uclisp.Context, env uclisp.Env) uclisp.Elem {
	return p
}
