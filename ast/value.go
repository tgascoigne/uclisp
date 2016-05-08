package ast

import "errors"

type Type int

const (
	IntegerType Type = iota
	StringType
	LambdaType
	ListType
	SymbolType
	GoType
	NilType
)

var ErrNotAValue = errors.New("cannot be used as a value")

func init() {
	Builtin.Define(Symbol("list"), SpecialForm{listForm})
	Builtin.Define(Symbol("quote"), SpecialForm{quoteForm})
}

type Value interface {
	Type() Type
	IsNil() bool
	Equals(Env, Value) bool
}

// Quoted wraps a Value into a Form, such that when evaluated it returns itsself.
// eg. '(1 2 3) evaluates to (1 2 3) rather than to a function call on the symbol '1'
type Quoted struct {
	Value
}

func (q Quoted) Eval(env Env) Value {
	// When you evaluate a quoted list, all the resulting list's elements must be quoted
	// also, otherwise the contents of an element would be evaluated.
	switch q.Value.(type) {
	default:
		return q.Value
	}

	return Nil
}

// case List:
// 	newList := make(List, len(list))
// 	for i := range list {
// 		newList[i] = Quoted{list[i].(Value)}
// 	}
// 	return newList
// case ListForm:
// 	newList := make(ListForm, len(list))
// 	for i := range list {
// 		newList[i] = Quoted{list[i].(Value)}
// 	}
// 	return newList

func listForm(env Env, args List) Value {
	list := make(List, len(args))
	for i := range args {
		list[i] = SimpleForm{args[i].Eval(env)}
	}
	return list
}

func quoteForm(env Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("quote", len(args))
	}

	form := args[0]

	if value, ok := form.(Value); ok {
		return Quoted{value}
	}

	exception(ErrNotAValue, form)
	return Nil
}
