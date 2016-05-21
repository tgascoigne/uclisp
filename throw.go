package uclisp

import "errors"

func init() {
	Builtin.Define("catch", NewProcedure(catchForm))
	Builtin.Define("catch*", NewProcedure(catchallForm))
	Builtin.Define("throw", NewProcedure(throwForm))
}

type Throw struct {
	Tag   Symbol
	Value Elem
}

func (t Throw) Eval(ctx *Context, env Env) Elem {
	return t.Value
}

func (t Throw) Equals(ctx *Context, env Env, other Elem) bool {
	ctx.Raise(ErrIncomparable, t.Value)
	return false
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

var ErrGoValUnsupported = errors.New("unsupported go value: %v")

// catch* catches all exceptions and values, including those from native code
//   (catch* errsymbol (prog) (on-error)..)
// on an uncaught throw/panic(), on-error is executed, and the thrown value is bound to errsymbol
// the result of on-error is returned.
func catchallForm(ctx *Context, env Env, args []Elem) (retval Elem) {
	if len(args) < 3 {
		ctx.Raise(ErrArgCount, len(args))
	}

	tagElem := ctx.Eval(args[0], env)
	tag, err := AssertSymbol(tagElem)
	if err != nil {
		ctx.Raise(err, tagElem)
	}

	defer func() {
		if err := recover(); err != nil {
			var thrownElem Elem
			if thrown, ok := err.(Elem); ok {
				thrownElem = thrown
			} else {
				ctx.Raise(ErrGoValUnsupported, err)
			}
			errbody := append(List{Symbol("let"), List{List{tag, thrownElem}}}, args[2:]...)
			retval = ctx.Eval(errbody, env)
		}
	}()

	return ctx.Eval(args[1], env)
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
