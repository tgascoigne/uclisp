package ast

// Lambda is a Callable value whose body is a Prog
type Lambda struct {
	argBindings []Symbol
	prog        Form
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

func (l Lambda) Call(parentEnv *Env, args List) Value {
	if len(args) != len(l.argBindings) {
		exceptionArgCountExpected("lambda", len(l.argBindings), len(args))
	}

	env := parentEnv.New()

	// bind symbols
	for i, sym := range l.argBindings {
		env.Set(sym, args[i].Eval(env))
	}

	return l.prog.Eval(env)
}
