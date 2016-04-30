package ast

import "errors"

func init() {
	Builtin.Define(Symbol("lambda"), SpecialForm{lambdaForm})
}

// Lambda is a Callable value whose body is a Prog
type Lambda struct {
	Bindings []Symbol
	Prog     Prog
}

func (l Lambda) Type() Type {
	return LambdaType
}

func (l Lambda) IsAtom() bool {
	return false
}

func (l Lambda) IsNil() bool {
	return false
}

func (l Lambda) Equals(env *Env, other Value) bool {
	return false
}

func (l Lambda) Call(parentEnv *Env, args List) Value {
	if len(args) != len(l.Bindings) {
		exceptionArgCountExpected("lambda", len(l.Bindings), len(args))
	}

	env := parentEnv.New()

	// bind symbols
	for i, sym := range l.Bindings {
		env.Define(sym, args[i].Eval(env))
	}

	return l.Prog.Eval(env)
}

var ErrInvalidArgList = errors.New("invalid argument list")

func lambdaForm(env *Env, args List) Value {
	if len(args) < 2 {
		exceptionArgCount("lambda", len(args))
	}

	var bindings []Symbol
	if b, ok := args[0].(ListForm); ok {
		bindings = make([]Symbol, len(b))
		for i, item := range b {
			if sym, ok := item.(Symbol); ok {
				bindings[i] = sym
			} else {
				exception(ErrInvalidSymbol, item)
			}
		}
	} else {
		exception(ErrInvalidArgList, args[0])
	}

	prog := make(Prog, len(args[1:]))
	for i, f := range args[1:] {
		prog[i] = f
	}

	return Lambda{bindings, prog}
}
