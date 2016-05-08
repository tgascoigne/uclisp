package uclisp

// A List is a literal list
type List []Elem

func (l List) Equals(env Env, o Elem) bool {
	other := AssertList(o)

	if len(l) != len(other) {
		return false
	}

	for i := range l {
		if !l[i].Equals(env, other[i]) {
			return false
		}
	}

	return true
}

func (l List) Eval(env Env) Elem {
	return l
}
