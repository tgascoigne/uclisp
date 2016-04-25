package ast

type Type int

const (
	IntegerType Type = iota
	LambdaType
	NilType
)

type Value interface {
	Type() Type
	IsAtom() bool
	IsNil() bool
}
