package uclisp

import (
	"fmt"
	"strings"
)

// A List is a list of elements
// Evaluating a list treats it as a procedure call of the form (procedure args...)
type List []Elem

func (l List) Equals(ctx *Context, env Env, o Elem) bool {
	other, err := AssertList(o)
	if err != nil {
		ctx.Raise(err, o)
	}

	if len(l) != len(other) {
		return false
	}

	for i := range l {
		if !l[i].Equals(ctx, env, other[i]) {
			return false
		}
	}

	return true
}

func (l List) Eval(ctx *Context, env Env) Elem {
	if len(l) == 0 {
		return Nil
	}

	procElem := ctx.Eval(l[0], env)
	proc, err := AssertProcedure(procElem)
	if err != nil {
		ctx.Raise(err, procElem)
	}

	return proc.Call(ctx, env, l[1:])
}

func (l List) String() string {
	elems := make([]string, len(l))
	for i := range l {
		elems[i] = fmt.Sprintf("%v", l[i])
	}

	return fmt.Sprintf("(%v)", strings.Join(elems, " "))
}
