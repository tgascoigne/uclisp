package ast

import "errors"

var ErrInvalidVarForm = errors.New("invalid variable binding list")
var ErrInvalidType = errors.New("invalid type")

func init() {
	Global.Set(Symbol("car"), SpecialForm{carForm})
	Global.Set(Symbol("cdr"), SpecialForm{cdrForm})
	Global.Set(Symbol("let"), SpecialForm{letForm})
	Global.Set(Symbol("progn"), SpecialForm{prognForm})
	Global.Set(Symbol("if"), SpecialForm{ifForm})
	Global.Set(Symbol("defvar"), SpecialForm{defvarForm})
	Global.Set(Symbol("lambda"), SpecialForm{lambdaForm})
}

func carForm(env *Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("car", len(args))
	}

	val := args[0].Eval(env)

	if val.IsNil() {
		return Nil
	}

	if val, ok := val.(List); ok {
		return val[0].Eval(env)
	} else {
		exception(ErrInvalidType, val)
	}

	return Nil
}

func cdrForm(env *Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("cdr", len(args))
	}

	val := args[0].Eval(env)

	if val.IsNil() {
		return Nil
	}

	if val, ok := val.(List); ok {
		if len(val) < 2 {
			return Nil
		}

		return val[1:]
	} else {
		exception(ErrInvalidType, val)
	}

	return Nil
}

func letForm(parentEnv *Env, args List) Value {
	if len(args) == 0 {
		exceptionArgCount("if", len(args))
	}

	env := parentEnv.New()

	if bindings, ok := args[0].(ListForm); ok {
		for _, b := range bindings {
			if b, ok := b.(ListForm); ok {
				// ((sym value) ..) syntax
				if len(b) != 2 {
					exception(ErrInvalidVarForm, b)
				}

				if sym, ok := b[0].(Symbol); ok {
					env.Set(sym, b[1].Eval(env))
				} else {
					exception(ErrInvalidSymbol, b)
				}
				continue
			}

			// (sym ..) syntax
			if sym, ok := b.(Symbol); ok {
				env.Set(sym, Nil)
			} else {
				exception(ErrInvalidSymbol, b)
			}
		}
	} else {
		exception(ErrInvalidVarForm, args[0])
	}

	var result Value
	result = Nil
	for _, f := range args[1:] {
		result = f.Eval(env)
	}

	return result
}

func prognForm(env *Env, args List) Value {
	prog := make(Prog, len(args))
	for i, f := range args {
		prog[i] = f
	}

	return prog.Eval(env)
}

func ifForm(env *Env, args List) Value {
	var test, then, els Form
	if len(args) == 2 {
		test, then = args[0], args[1]
	} else if len(args) == 3 {
		test, then, els = args[0], args[1], args[2]
	} else {
		exceptionArgCount("if", len(args))
	}

	if !test.Eval(env).IsNil() {
		return then.Eval(env)
	}

	if els != nil {
		return els.Eval(env)
	}

	return Nil
}

func defvarForm(env *Env, args List) Value {
	if len(args) != 2 {
		exceptionArgCount("defvar", len(args))
	}

	var symbolForm, value Form
	var symbol Symbol
	symbolForm, value = args[0], args[1]

	if s, ok := symbolForm.(Symbol); ok {
		symbol = s
	} else {
		exception(ErrInvalidSymbol, symbol)
	}

	Global.Set(symbol, value.Eval(env))

	return symbol
}

var ErrInvalidArgList = errors.New("invalid argument list")

func lambdaForm(env *Env, args List) Value {
	if len(args) < 2 {
		exceptionArgCount("lambda", len(args))
	}

	var bindings []Symbol
	if b, ok := args[0].(ListForm); ok {
		bindings = make([]Symbol, len(b))
		for i, item := range b {
			if sym, ok := item.(Symbol); ok {
				bindings[i] = sym
			} else {
				exception(ErrInvalidSymbol, item)
			}
		}
	} else {
		exception(ErrInvalidArgList, args[0])
	}

	prog := make(Prog, len(args[1:]))
	for i, f := range args[1:] {
		prog[i] = f
	}

	return Lambda{bindings, prog}
}

//(funcall (lambda (x y) (+ x y) (* x y)) 2 4)
