package ast

import "fmt"

var specialForms = map[Symbol]Callable{}

// A Form is an object to be evaluated, a self evaluating object, or a symbol
type Form interface {
	Eval(*Env) Value
}

// A List is a list of Forms. Can be interpreted as a FuncForm. A List is also a Form.
type List []Form

func (l List) Eval(env *Env) Value {
	fn := l[0].Eval(env)
	if fn.Type() != LambdaType {
		exception(ErrNotCallable, fmt.Sprintf("%v", fn))
	}
	lambda := fn.(Callable)

	var args []Form
	if len(l) > 1 {
		args = l[1:]
	}

	return lambda.Call(env, args)
}

// A SpecialForm is a FuncForm which is built in to the interpreter
// Symbols matching built in forms will eval to this type (via specialForms map)
type SpecialForm struct {
	Fn func(env *Env, args List) Value
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

func (f SpecialForm) Call(env *Env, args List) Value {
	return f.Fn(env, args)
}
