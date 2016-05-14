package uclisp

func init() {
	Builtin.Define("list", Procedure(listForm))
	Builtin.Define("car", Procedure(carForm))
	Builtin.Define("cdr", Procedure(cdrForm))
	Builtin.Define("last", Procedure(lastForm))
	Builtin.Define("butlast", Procedure(butlastForm))
	Builtin.Define("append", Procedure(appendForm))
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

func appendForm(env Env, args []Elem) Elem {
	result := List{}
	for _, list := range args {
		if list, err := AssertList(list.Eval(env)); err == nil {
			result = append(result, list...)
		} else {
			Raise(err)
		}
	}

	return result
}
