package ast

import "fmt"

// A Form is an object to be evaluated, a self evaluating object, or a symbol
type Form interface {
	Eval(Env) Value
}

// A ListForm is a List which can be evaluated as a function call
type ListForm List

func (l ListForm) Type() Type {
	return List(l).Type()
}

func (l ListForm) IsNil() bool {
	return List(l).IsNil()
}

func (l ListForm) Equals(env Env, other Value) bool {
	return List(l).Equals(env, other)
}

func (l ListForm) Eval(env Env) Value {
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

type Prog []Form

func (p Prog) Eval(env Env) Value {
	var result Value
	result = Nil
	for _, f := range p {
		result = f.Eval(env)
	}

	return result
}

// A SpecialForm is a Form which is built in to the interpreter
// Symbols matching built in forms will eval to this type (via specialForms map)
type SpecialForm struct {
	Fn func(env Env, args List) Value
}

func (f SpecialForm) Type() Type {
	return LambdaType
}

func (f SpecialForm) IsNil() bool {
	return false
}

func (f SpecialForm) Call(env Env, args List) Value {
	return f.Fn(env, args)
}

func (f SpecialForm) Equals(env Env, other Value) bool {
	// Why would you compare a special form?
	return false
}

type SimpleForm struct {
	Value
}

func (f SimpleForm) Eval(env Env) Value {
	return f.Value
}
