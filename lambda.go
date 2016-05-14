package uclisp

import "errors"

func init() {
	Builtin.Define("lambda", Procedure(lambdaForm))
}

var ErrInvalidArgSpec = errors.New("invalid argument specification")

var optionalKeyword = Symbol("&optional")
var restKeyword = Symbol("&rest")

type argSpec struct {
	required []Symbol
	optional []Symbol
	rest     Symbol
}

func parseArgSpec(args []Symbol) argSpec {
	spec := argSpec{
		required: make([]Symbol, 0),
		optional: make([]Symbol, 0),
	}

	// stage = 0 (required) 1 (optional) 2 (rest)
	stage := 0

	for _, sym := range args {
		if sym == optionalKeyword {
			stage = 1
			continue
		}

		if sym == restKeyword {
			stage = 2
			continue
		}

		switch stage {
		case 0:
			spec.required = append(spec.required, sym)
		case 1:
			spec.optional = append(spec.optional, sym)
		case 2:
			if spec.rest != EmptySymbol {
				Raise(ErrInvalidArgSpec, args)
			}
			spec.rest = sym
		}
	}

	return spec
}

func (spec argSpec) hasOptionalArgs() bool {
	return len(spec.optional) > 0
}

func (spec argSpec) hasRestArg() bool {
	return spec.rest != EmptySymbol
}

func (spec argSpec) Bind(env Env, args []Elem) Env {
	bound := NewBasicEnv(env)

	if len(args) < len(spec.required) {
		Raise(ErrArgCount, len(args), len(spec.required))
		return bound
	}

	var sym Symbol
OUTER:
	for i, arg := range args {
		switch {
		case i < len(spec.required):
			sym = spec.required[i]
		case i >= len(spec.required) && (i-len(spec.required)) < len(spec.optional):
			sym = spec.optional[i-len(spec.required)]
		default:
			sym = spec.rest
			val := List(args[i:])
			bound.Define(sym, val)
			break OUTER
		}

		bound.Define(sym, arg)
	}

	// all missing optionals are defined to Nil
	for _, sym := range append(spec.optional, spec.rest) {
		if !bound.Defined(sym) {
			bound.Define(sym, Nil)
		}
	}

	return bound
}

func lambdaForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	argSpec, err := AssertList(args[0])
	if err != nil {
		Raise(err)
	}

	argSpecSymbols := make([]Symbol, len(argSpec))
	for i := range argSpec {
		argSpecSymbols[i], err = AssertSymbol(argSpec[i])
		if err != nil {
			Raise(err)
		}
	}

	bindings := parseArgSpec(argSpecSymbols)

	body := append(List{Symbol("progn")}, args[1:]...)

	return Procedure(func(callerEnv Env, args []Elem) Elem {
		args = args[:] // copy original list
		for i := range args {
			// arguments are evaluated in the caller's environment
			args[i] = args[i].Eval(callerEnv)
		}

		// ... but the body of the function is evaluated in the environment at the point
		// of declaration.
		// Roughly emulates lexical/dynamic binding
		bound := bindings.Bind(env, args)
		return body.Eval(bound)
	})
}
