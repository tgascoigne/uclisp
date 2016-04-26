package ast

type Integer int

func (i Integer) Type() Type {
	return IntegerType
}

func (i Integer) IsAtom() bool {
	return true
}

func (i Integer) IsNil() bool {
	return false
}

func (i Integer) Eval(env *Env) Value {
	return i
}
