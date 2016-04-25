package ast

type Symbol string

func (s Symbol) Set(v Value) {

}

func (s Symbol) Eval() Value {
	if fn, ok := specialForms[s]; ok {
		return fn
	}
	return Nil
}
