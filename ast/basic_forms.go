package ast

import (
	"errors"
	"fmt"
	"io/ioutil"
)

var ErrInvalidVarForm = errors.New("invalid variable binding list")
var ErrInvalidType = errors.New("invalid type")

func init() {
	Builtin.Define(Symbol("car"), SpecialForm{carForm})
	Builtin.Define(Symbol("cdr"), SpecialForm{cdrForm})
	Builtin.Define(Symbol("progn"), SpecialForm{prognForm})
	Builtin.Define(Symbol("if"), SpecialForm{ifForm})
	Builtin.Define(Symbol("load-file"), SpecialForm{loadFileForm})
	Builtin.Define(Symbol("eval"), SpecialForm{evalForm})
	Builtin.Define(Symbol("symbol-value"), SpecialForm{symbolValueForm})
}

func evalForm(env Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("eval", len(args))
	}

	form := args[0].Eval(env)
	if form, ok := form.(Form); ok {
		fmt.Printf("evaling %#v\n", form)
		val := form.Eval(env)
		fmt.Printf(".. into %#v\n", val)
		return val
	}

	return form
}

func symbolValueForm(env Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("symbol-value", len(args))
	}

	symbol := args[0].Eval(env)
	if symbol.Type() != SymbolType {
		exception(ErrInvalidType, symbol)
	}

	return env.Get(symbol.(Symbol))
}

func loadFileForm(parentenv Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("load-file", len(args))
	}

	sVal := args[0].Eval(parentenv)

	var path string
	if sVal.Type() == StringType {
		path = string(sVal.(String))
	} else {
		exception(ErrInvalidType, sVal)
	}

	//todo: better logging
	fmt.Printf("load-file: %v\n", path)

	data, err := ioutil.ReadFile(path)
	if err != nil {
		exception(err, path)
	}

	env := NewEnv(parentenv)
	prog := Parse(path, string(data))
	return prog.Eval(env)
}

func carForm(env Env, args List) Value {
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

func cdrForm(env Env, args List) Value {
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

func prognForm(env Env, args List) Value {
	prog := make(Prog, len(args))
	for i, f := range args {
		prog[i] = f
	}

	return prog.Eval(env)
}

func ifForm(env Env, args List) Value {
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
