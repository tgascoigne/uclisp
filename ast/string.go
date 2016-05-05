package ast

type String string

func (i String) Type() Type {
	return StringType
}

func (i String) IsNil() bool {
	return false
}

func (i String) Eval(env Env) Value {
	return i
}

func (i String) Equals(env Env, other Value) bool {
	return i == other
}
