package ast

import (
	"errors"
	"fmt"
)

var ErrImmutableEnv = errors.New("immutable environment")
var ErrNotAnEnvironment = errors.New("not a valid environment")

func init() {
	Builtin.Define(Symbol("let"), SpecialForm{letForm})
	Builtin.Define(Symbol("set"), SpecialForm{setForm})
	Builtin.Define(Symbol("define"), SpecialForm{defineForm})
	Builtin.Define(Symbol("dumpenv"), SpecialForm{dumpenvForm})
}

// Env is a mapping of Symbol -> Value.
// An Environment is heirarchical: each Env has a parent Env, and all Envs are rooted at Global.
// Nested Envs are used for scoping of variables.
type Env interface {
	Map() map[Symbol]Value
	Parent() Env
	Define(s Symbol, v Value)
	Defined(s Symbol) bool
	Set(s Symbol, v Value)
	Get(s Symbol) Value
}

type env struct {
	m      map[Symbol]Value
	parent Env
}

func (e *env) Map() map[Symbol]Value {
	return e.m
}

func (e *env) Parent() Env {
	return e.parent
}

// Builtin is the set of built in symbols
var Builtin Env = NewEnv(nil)

// Global is the interpreter's global scope.
var Global Env = NewEnv(Builtin)

func NewEnv(parent Env) Env {
	return &env{
		m:      make(map[Symbol]Value),
		parent: parent,
	}
}

// Define a symbol within this scope
func (e *env) Define(s Symbol, v Value) {
	e.m[s] = v
}

// Set sets a value within this or a parent's scope
func (e *env) Set(s Symbol, v Value) {
	if _, ok := e.m[s]; ok {
		e.m[s] = v
		return
	}

	if e.parent != nil {
		e.parent.Set(s, v)
		return
	}

	exception(ErrNoSymbol, string(s))
}

// Test if s is set in this scope
func (e *env) Defined(s Symbol) bool {
	if _, ok := e.m[s]; ok {
		return true
	}

	if e.parent != nil {
		return e.parent.Defined(s)
	}

	return false
}

// Get looks up a value within a given scope.
// If the symbol does not exist in the current scope, it recurses up until it reaches the global scope.
func (e *env) Get(s Symbol) Value {
	if v, ok := e.m[s]; ok {
		return v
	}

	if e.parent != nil {
		return e.parent.Get(s)
	}

	exception(ErrNoSymbol, string(s))
	return nil
}

func defineForm(env Env, args List) Value {
	if len(args) != 2 {
		exceptionArgCount("define", len(args))
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

func setForm(env Env, args List) Value {
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

func letForm(parentenv Env, args List) Value {
	if len(args) == 0 {
		exceptionArgCount("if", len(args))
	}

	env := NewEnv(parentenv)

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

func dumpenvForm(env Env, args List) Value {
	//todo: dump in a more sensible way
	fmt.Printf("%V\n", env)
	return Nil
}
