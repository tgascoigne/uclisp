package ast

import "errors"

var ErrNoSymbol = errors.New("no such symbol")

func init() {
	Builtin.Define(Symbol("let"), SpecialForm{letForm})
	Builtin.Define(Symbol("set"), SpecialForm{setForm})
	Builtin.Define(Symbol("defvar"), SpecialForm{defvarForm})
}

// Env is a mapping of Symbol -> Value.
// An Environment is heirarchical: each Env has a parent Env, and all Envs are rooted at Global.
// Nested Envs are used for scoping of variables.
type Env struct {
	Map    map[Symbol]Value
	Parent *Env
}

// Builtin is the set of built in symbols
var Builtin *Env = NewEnv(nil)

// Global is the interpreter's global scope.
var Global *Env = NewEnv(Builtin)

func NewEnv(parent *Env) *Env {
	return &Env{
		Map:    make(map[Symbol]Value),
		Parent: parent,
	}
}

// Define a symbol within this scope
func (e *Env) Define(s Symbol, v Value) {
	e.Map[s] = v
}

// Set sets a value within this or a parent's scope
func (e *Env) Set(s Symbol, v Value) {
	if _, ok := e.Map[s]; ok {
		e.Map[s] = v
		return
	}

	if e.Parent != nil {
		e.Parent.Set(s, v)
		return
	}

	exception(ErrNoSymbol, string(s))
}

// Test if s is set in this scope
func (e *Env) Defined(s Symbol) bool {
	if _, ok := e.Map[s]; ok {
		return true
	}

	if e.Parent != nil {
		return e.Parent.Defined(s)
	}

	return false
}

// Get looks up a value within a given scope.
// If the symbol does not exist in the current scope, it recurses up until it reaches the global scope.
func (e *Env) Get(s Symbol) Value {
	if v, ok := e.Map[s]; ok {
		return v
	}

	if e.Parent != nil {
		return e.Parent.Get(s)
	}

	exception(ErrNoSymbol, string(s))
	return nil
}

// New creates an environment with a parent environment of 'e'
func (e *Env) New() *Env {
	return NewEnv(e)
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

	if !Global.Defined(symbol) {
		Global.Define(symbol, value.Eval(env))
	}

	return symbol
}

func setForm(env *Env, args List) Value {
	if len(args) != 2 {
		exceptionArgCount("set", len(args))
	}
	var symbolForm, value Form
	var symbol Symbol
	symbolForm, value = args[0], args[1]

	if s, ok := symbolForm.(Symbol); ok {
		symbol = s
	} else {
		exception(ErrInvalidSymbol, symbol)
	}

	if !env.Defined(symbol) {
		exception(ErrNoSymbol, symbol)
	}

	env.Set(symbol, value.Eval(env))
	return env.Get(symbol)
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
					env.Define(sym, b[1].Eval(env))
				} else {
					exception(ErrInvalidSymbol, b)
				}
				continue
			}

			// (sym ..) syntax
			if sym, ok := b.(Symbol); ok {
				env.Define(sym, Nil)
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
