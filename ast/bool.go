package ast

var (
	// Nil is the global nil value
	Nil nilValue = nilValue{}
	// True is the global t value
	True Integer = Integer(1)
)

func init() {
	Builtin.Define(Symbol("nil"), &Nil)
	Builtin.Define(Symbol("t"), &True)
}

type nilValue struct{}

func (n nilValue) Type() Type {
	return IntegerType
}

func (n nilValue) IsAtom() bool {
	return true
}

func (n nilValue) IsNil() bool {
	return true
}

func (n nilValue) Eval(env *Env) Value {
	return n
}

func (n nilValue) Equals(env *Env, other Value) bool {
	return other == Nil
}
