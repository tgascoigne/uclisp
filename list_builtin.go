package uclisp

func init() {
	Builtin.Define("list", Procedure(listForm))
	Builtin.Define("car", Procedure(carForm))
	Builtin.Define("cdr", Procedure(cdrForm))
	Builtin.Define("last", Procedure(lastForm))
	Builtin.Define("butlast", Procedure(butlastForm))
	Builtin.Define("append", Procedure(appendForm))
	Builtin.Define("nth", Procedure(nthForm))
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

	listElem := args[0].Eval(env)
	list, err := AssertList(listElem)
	if err != nil {
		Raise(err, listElem)
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

	listElem := args[0].Eval(env)
	list, err := AssertList(listElem)
	if err != nil {
		Raise(err, listElem)
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

	listElem := args[0].Eval(env)
	list, err := AssertList(listElem)
	if err != nil {
		Raise(err, listElem)
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

	listElem := args[0].Eval(env)
	list, err := AssertList(listElem)
	if err != nil {
		Raise(err, listElem)
	}

	if len(list) == 0 {
		return Nil
	}

	return List(list[:len(list)-1])
}

func appendForm(env Env, args []Elem) Elem {
	result := List{}
	for _, list := range args {
		listElem := list.Eval(env)
		if list, err := AssertList(listElem); err == nil {
			result = append(result, list...)
		} else {
			Raise(err, listElem)
		}
	}

	return result
}

func nthForm(env Env, args []Elem) Elem {
	if len(args) != 2 {
		Raise(ErrArgCount, len(args), 2)
	}

	nElem := args[0].Eval(env)
	n, err := AssertInteger(nElem)
	if err != nil {
		Raise(err, nElem)
	}

	listElem := args[1].Eval(env)
	list, err := AssertList(listElem)
	if err != nil {
		Raise(err, listElem)
	}

	if int(n) >= len(list) {
		return Nil
	}

	return list[n]
}
