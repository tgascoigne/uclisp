package ast

var (
	Nil  nilValue = nilValue{}
	True Integer  = Integer(1)
)

func init() {
	Global.Set(Symbol("nil"), &Nil)
	Global.Set(Symbol("t"), &True)
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
