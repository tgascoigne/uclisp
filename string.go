package uclisp

// String is a string.
type String string

func (s String) Equals(env Env, o Elem) bool {
	other, err := AssertString(o)
	if err != nil {
		Raise(err, o)
	}

	return s == other
}

func (s String) Eval(ctx *Context, env Env) Elem {
	return s
}
