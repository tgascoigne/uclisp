package uclisp

import (
	"fmt"
	"strings"
	"sync"
)

var callStack = &Trace{make([]Elem, 0)}

// Context is a thread of execution within a lisp program.
// A Context has its own call stack, but shares the Global environment with all other threads.
type Context struct {
	CallStack *Trace
	m         sync.Mutex
}

// NewContext constructs a new context
func NewContext() *Context {
	return &Context{
		CallStack: &Trace{make([]Elem, 0)},
	}
}

// Begin evaluates elm,
func (c *Context) Begin(elm Elem) Elem {
	c.m.Lock()
	defer c.m.Unlock()
	return c.Eval(elm, Global)
}

// Eval adds elm to the call stack, evaluates it, then removes it from the call stack.
// This is preferable to calling elm.Eval directly, as it allows a backtrace to be collected on exceptions.
func (c *Context) Eval(elm Elem, env Env) Elem {
	c.CallStack.Push(elm)

	result := elm.Eval(c, env)

	// Purposefully not deferred, because if Eval panics, we want to leave the stack trace as is
	c.CallStack.Pop()
	return result
}

// Trace is a stack of Elems which represents the trace from the root to the currently evaluating element
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

// Dump constructs a printable call trace from the root to the currently evaluating element
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
