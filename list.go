package uclisp

import (
	"fmt"
	"strings"
)

// A List is a list of elements
// Evaluating a list treats it as a procedure call of the form (procedure args...)
type List []Elem

func (l List) Equals(env Env, o Elem) bool {
	other, err := AssertList(o)
	if err != nil {
		Raise(err, o)
	}

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
	if len(l) == 0 {
		return Nil
	}

	procElem := Eval(l[0], env)
	proc, err := AssertProcedure(procElem)
	if err != nil {
		Raise(err, procElem)
	}

	return proc.Call(env, l[1:])
}

func (l List) String() string {
	elems := make([]string, len(l))
	for i := range l {
		elems[i] = fmt.Sprintf("%v", l[i])
	}

	return fmt.Sprintf("(%v)", strings.Join(elems, " "))
}
