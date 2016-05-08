package uclisp

// String is a string.
type String string

func (s String) Equals(env Env, o Elem) bool {
	other := AssertString(o)

	return s == other
}

func (s String) Eval(env Env) Elem {
	return s
}
