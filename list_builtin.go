package uclisp

func init() {
	Builtin.Set("list", Procedure(listForm))
}

func listForm(env Env, args []Elem) Elem {
	list := make(List, len(args))
	for i := range args {
		list[i] = args[i].Eval(env)
	}

	return list
}
