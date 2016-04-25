package ast

import "fmt"

var specialForms = map[Symbol]Value{}

// A Form is an object to be evaluated, a self evaluating object, or a symbol
type Form interface {
	Eval() Value
}

// FuncForm is a form whose car is a lambda to invoke, and whose cdr are passed as arguments.
type FuncForm struct {
	// Func is an expression which evaluates to a Callable
	Func Form
	Args List
}

func (f FuncForm) Eval() Value {
	fn := f.Func.Eval()
	if fn.Type() != LambdaType {
		exception(ErrNotCallable, fmt.Sprintf("%v", fn))
	}

	lambda := fn.(Callable)
	return lambda.Call(f.Args)
}

// A SpecialForm is a FuncForm which is built in to the interpreter
type SpecialForm struct {
	Fn func(args List) Value
}

func (f SpecialForm) Type() Type {
	return LambdaType
}

func (f SpecialForm) IsAtom() bool {
	return false
}

func (f SpecialForm) IsNil() bool {
	return false
}

func (f SpecialForm) Call(args List) Value {
	return f.Fn(args)
}
