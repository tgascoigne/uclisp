package ast

type Type int

const (
	IntegerType Type = iota
	LambdaType
	ListType
	NilType
)

type Value interface {
	Type() Type
	IsAtom() bool
	IsNil() bool
	Equals(*Env, Value) bool
}
