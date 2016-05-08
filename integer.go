package uclisp

// Integer is an int.
type Integer int

func (i Integer) Eval(env Env) Elem {
	return i
}
