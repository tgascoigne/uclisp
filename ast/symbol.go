package ast

type Symbol string

func (s Symbol) Eval() Value {
	return Global.Get(s)
}
