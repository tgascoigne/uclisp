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

func (l Lambda) Call(env *Env, args List) Value {
	// create env
	// bind symbols
	return l.prog.Eval(env)
}
