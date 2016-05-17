package uclisp

func init() {
	Builtin.Define("catch", Procedure(catchForm))
	Builtin.Define("throw", Procedure(throwForm))
}

type Throw struct {
	Tag   Symbol
	Value Elem
}

func catchForm(ctx *Context, env Env, args []Elem) (retval Elem) {
	if len(args) < 2 {
		ctx.Raise(ErrArgCount, len(args))
	}

	tagElem := ctx.Eval(args[0], env)
	tag, err := AssertSymbol(tagElem)
	if err != nil {
		ctx.Raise(err, tagElem)
	}

	defer func() {
		if throw := recover(); throw != nil {
			if throw, ok := throw.(Throw); ok {
				if Equal(ctx, env, throw.Tag, tag) {
					retval = throw.Value
					return
				} else {
					// Throw for a different tag; re-panic
					panic(throw)
				}
			} else {
				// Not a throw; re-panic
				panic(throw)
			}
		}
	}()

	body := append(List{Symbol("progn")}, args[1:]...)
	return ctx.Eval(body, env)
}

func throwForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 2 {
		ctx.Raise(ErrArgCount, len(args), 2)
	}

	tagElem := ctx.Eval(args[0], env)
	tag, err := AssertSymbol(tagElem)
	if err != nil {
		ctx.Raise(err, tagElem)
	}

	value := ctx.Eval(args[1], env)
	panic(Throw{
		Tag:   tag,
		Value: value,
	})

	return nil // never reached
}
