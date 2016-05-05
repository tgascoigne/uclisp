package ast

// A List is a list of Forms. Can be interpreted as a FuncForm. A List is also a Form.
type List []Form

func (l List) Type() Type {
	return ListType
}

func (l List) IsNil() bool {
	return len(l) == 0
}

func (l List) Equals(env Env, other Value) bool {
	var otherList List
	if j, ok := other.(List); ok {
		otherList = j
	} else {
		return false
	}

	if len(l) != len(otherList) {
		return false
	}

	for i := range l {
		lVal := l[i].Eval(env)
		oVal := otherList[i].Eval(env)
		if !lVal.Equals(env, oVal) {
			return false
		}
	}

	return true
}
