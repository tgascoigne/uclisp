package ast

type nilValue struct{}

var (
	Nil nilValue = nilValue{}
)

func (n nilValue) Type() Type {
	return IntegerType
}

func (n nilValue) IsAtom() bool {
	return true
}

func (n nilValue) IsNil() bool {
	return true
}
