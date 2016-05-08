package ast

type Integer int

func (i Integer) Type() Type {
	return IntegerType
}

func (i Integer) IsNil() bool {
	return false
}

func (i Integer) Equals(env Env, other Value) bool {
	return i == other
}
