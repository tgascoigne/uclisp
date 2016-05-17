package uclisp

import (
	"fmt"
	"strings"
)

var callStack = &Trace{make([]Elem, 0)}

type Trace struct {
	stack []Elem
}

func (t *Trace) Push(elm Elem) {
	t.stack = append(t.stack, elm)
}

func (t *Trace) Pop() Elem {
	if len(t.stack) == 0 {
		return Nil
	}
	elm := t.stack[len(t.stack)-1]
	t.stack = t.stack[:len(t.stack)-1]
	return elm
}

func (t *Trace) Dump(depth int) string {
	trace := ""
	for _, elm := range t.stack {
		trace += printStackFrame(elm, depth)
		trace += "\n"
	}

	return trace
}

func printStackFrame(elm Elem, depth int) string {
	if depth == 0 {
		return "..."
	}

	if list, err := AssertList(elm); err == nil {
		if depth > 1 {
			parts := make([]string, len(list))
			for i, elm := range list {
				parts[i] = printStackFrame(elm, depth-1)
			}
			return fmt.Sprintf("(%v)", strings.Join(parts, " "))
		} else {
			return "(...)"
		}
	}

	return fmt.Sprintf("%v", elm)
}

func Eval(elm Elem, env Env) Elem {
	callStack.Push(elm)

	result := elm.Eval(env)

	// Purposefully not deferred, because if Eval panics, we want to leave the stack trace as is
	callStack.Pop()
	return result
}
