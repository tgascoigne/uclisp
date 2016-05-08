package uclisp

// A List is a literal list
type List []Elem

func (l List) Eval(env Env) Elem {
	return l
}
