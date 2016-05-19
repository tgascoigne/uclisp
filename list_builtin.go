package uclisp

func init() {
	Builtin.Define("cons", Procedure(consForm))
	Builtin.Define("list", Procedure(listForm))
	Builtin.Define("car", Procedure(carForm))
	Builtin.Define("cdr", Procedure(cdrForm))
	Builtin.Define("last", Procedure(lastForm))
	Builtin.Define("butlast", Procedure(butlastForm))
	Builtin.Define("append", Procedure(appendForm))
	Builtin.Define("nth", Procedure(nthForm))
}

func consForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 2 {
		ctx.Raise(ErrArgCount, len(args), 2)
	}

	car := ctx.Eval(args[0], env)
	cdr := ctx.Eval(args[1], env)
	list := List{car}
	if cdrList, err := AssertList(cdr); err == nil {
		list = append(list, cdrList...)
	} else {
		list = append(list, cdr)
	}

	return list
}

func listForm(ctx *Context, env Env, args []Elem) Elem {
	list := make(List, len(args))
	for i := range args {
		list[i] = ctx.Eval(args[i], env)
	}

	return list
}

func carForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	listElem := ctx.Eval(args[0], env)
	list, err := AssertList(listElem)
	if err != nil {
		ctx.Raise(err, listElem)
	}

	if len(list) == 0 {
		return Nil
	}

	return list[0]
}

func cdrForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	listElem := ctx.Eval(args[0], env)
	list, err := AssertList(listElem)
	if err != nil {
		ctx.Raise(err, listElem)
	}

	if len(list) == 0 {
		return Nil
	}

	return List(list[1:])
}

func lastForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	listElem := ctx.Eval(args[0], env)
	list, err := AssertList(listElem)
	if err != nil {
		ctx.Raise(err, listElem)
	}

	if len(list) == 0 {
		return Nil
	}

	return List{list[len(list)-1]}
}

func butlastForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	listElem := ctx.Eval(args[0], env)
	list, err := AssertList(listElem)
	if err != nil {
		ctx.Raise(err, listElem)
	}

	if len(list) == 0 {
		return Nil
	}

	return List(list[:len(list)-1])
}

func appendForm(ctx *Context, env Env, args []Elem) Elem {
	result := List{}
	for _, list := range args {
		listElem := ctx.Eval(list, env)
		if list, err := AssertList(listElem); err == nil {
			result = append(result, list...)
		} else {
			ctx.Raise(err, listElem)
		}
	}

	return result
}

func nthForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 2 {
		ctx.Raise(ErrArgCount, len(args), 2)
	}

	nElem := ctx.Eval(args[0], env)
	n, err := AssertInteger(nElem)
	if err != nil {
		ctx.Raise(err, nElem)
	}

	listElem := ctx.Eval(args[1], env)
	list, err := AssertList(listElem)
	if err != nil {
		ctx.Raise(err, listElem)
	}

	if int(n) >= len(list) {
		return Nil
	}

	return list[n]
}
