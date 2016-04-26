package ast

type Symbol string

func (s Symbol) Eval(env *Env) Value {
	return env.Get(s)
}
