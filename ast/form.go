package ast

import "fmt"

var specialForms = map[Symbol]Value{}

// Form is an expression which invokes a Callable with some arguments
type Form struct {
	// Func is an expression which evaluates to a Callable
	Func Expression
	Args List
}

func (f Form) Eval() Value {
	fn := f.Func.Eval()
	if fn.Type() != LambdaType {
		exception(ErrNotCallable, fmt.Sprintf("%v", fn))
	}

	lambda := fn.(Callable)
	return lambda.Call(f.Args)
}

// A SpecialForm is a Callable which is built in to the interpreter
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
