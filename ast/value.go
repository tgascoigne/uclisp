package ast

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

func init() {
	Builtin.Define(Symbol("list"), SpecialForm{listForm})
}

// Quoted wraps a Value into a Form, such that when evaluated it returns itsself.
// eg. '(1 2 3) evaluates to (1 2 3) rather than to a function call on the symbol '1'
type Quoted struct {
	Value
}

func (q Quoted) Eval(env Env) Value {
	return q.Value
}

type Value interface {
	Type() Type
	IsNil() bool
	Equals(Env, Value) bool
}

func listForm(env Env, args List) Value {
	list := make(List, len(args))
	for i := range args {
		list[i] = SimpleForm{args[i].Eval(env)}
	}
	return list
}
