package uclisp

// String is a string.
type String string

func (s String) Eval(env Env) Elem {
	return s
}
