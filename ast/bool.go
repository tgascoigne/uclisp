package ast

var (
	Nil  nilValue = nilValue{}
	True Integer  = Integer(1)
)

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

func (n nilValue) Eval() Value {
	return n
}
