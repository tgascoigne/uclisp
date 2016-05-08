package uclisp

import "errors"

var ErrNotAProg = errors.New("invalid procedure call")

// A Prog is a list of the form (procedure args...)
// Evaluating a prog calls procedure(args)
type Prog []Elem

func (p Prog) Eval(env Env) Elem {
	if len(p) == 0 {
		Raise(ErrNotAProg, p)
	}

	proc := AssertProc(p[0].Eval(env))
	return proc.Call(env, p[1:])
}
