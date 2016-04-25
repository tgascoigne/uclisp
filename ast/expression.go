package ast

// An Expression is something which can be evaluated into a value
// Examples of expressions:
//   (+ 2 3)
//   (some-function)
//   some-symbol
//
// Primitive types also implement this interface to avoid having to make a distinction when evaluating.
// This is possibly the wrong terminology, but I'm not too worried about that for now.
type Expression interface {
	Eval() Value
}
