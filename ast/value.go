package ast

type Type int

const (
	IntegerType Type = iota
	LambdaType
	ListType
	NilType
)

// Quoted wraps a Value into a Form, such that when evaluated it returns itsself.
// eg. '(1 2 3) evaluates to (1 2 3) rather than to a function call on the symbol '1'
type Quoted struct {
	V Value
}

func (q Quoted) Eval(env *Env) Value {
	return q.V
}

type Value interface {
	Type() Type
	IsAtom() bool
	IsNil() bool
	Equals(*Env, Value) bool
}
