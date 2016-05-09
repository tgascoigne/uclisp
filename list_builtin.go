package uclisp

func init() {
	Builtin.Set("list", Procedure(listForm))
	Builtin.Set("car", Procedure(carForm))
	Builtin.Set("cdr", Procedure(cdrForm))
	Builtin.Set("last", Procedure(lastForm))
	Builtin.Set("butlast", Procedure(butlastForm))
}

func listForm(env Env, args []Elem) Elem {
	list := make(List, len(args))
	for i := range args {
		list[i] = args[i].Eval(env)
	}

	return list
}

func carForm(env Env, args []Elem) Elem {
	if len(args) == 0 {
		Raise(ErrArgCount, len(args), 1)
	}

	list, err := AssertList(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	if len(list) == 0 {
		return Nil
	}

	return list[0]
}

func cdrForm(env Env, args []Elem) Elem {
	if len(args) == 0 {
		Raise(ErrArgCount, len(args), 1)
	}

	list, err := AssertList(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	if len(list) == 0 {
		return Nil
	}

	return List(list[1:])
}

func lastForm(env Env, args []Elem) Elem {
	if len(args) == 0 {
		Raise(ErrArgCount, len(args), 1)
	}

	list, err := AssertList(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	if len(list) == 0 {
		return Nil
	}

	return List{list[len(list)-1]}
}

func butlastForm(env Env, args []Elem) Elem {
	if len(args) == 0 {
		Raise(ErrArgCount, len(args), 1)
	}

	list, err := AssertList(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	if len(list) == 0 {
		return Nil
	}

	return List(list[:len(list)-1])
}
