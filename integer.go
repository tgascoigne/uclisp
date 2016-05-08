package uclisp

// Integer is an int.
type Integer int

func (i Integer) Equals(env Env, o Elem) bool {
	other := AssertInteger(o)

	return i == other
}

func (i Integer) Eval(env Env) Elem {
	return i
}
