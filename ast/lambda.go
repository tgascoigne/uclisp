package ast

// Lambda is a Callable value whose body is a Prog
type Lambda struct {
	argBindings []Symbol
	prog        Prog
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

func (l Lambda) Call(args List) Value {
	// create env
	// bind symbols
	return l.prog.Eval()
}
