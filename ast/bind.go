package ast

import (
	"fmt"
	"reflect"
)

func init() {
	Builtin.Define(Symbol("with"), SpecialForm{withForm})
}

func withForm(parentenv Env, args List) Value {
	if len(args) < 2 {
		exceptionArgCount("with", len(args))
	}

	arg0val := args[0].Eval(parentenv)

	var env Env

	if goEnv, ok := arg0val.(*GoEnv); ok {
		env = goEnv.With(parentenv)
	} else {
		exception(ErrNotAnEnvironment, arg0val)
	}

	var result Value
	result = Nil
	for _, f := range args[1:] {
		result = f.Eval(env)
	}

	return result
}

// BindType produces a value which can be used to access and manipulate go values from lisp
func BindType(o interface{}) Value {
	return &GoEnv{V: o}
}

// GoEnv is an environment which provides access to methods and fields (if V is a struct) of a go type
type GoEnv struct {
	V interface{}
	// maps of symbol to reflect indices
	fields     map[Symbol][]int
	methods    map[Symbol]int
	ptrMethods map[Symbol]int
}

func (e *GoEnv) Type() Type {
	return GoType
}

func (e *GoEnv) IsNil() bool {
	return e.V == nil
}

func (e *GoEnv) Equals(env Env, other Value) bool {
	if other, ok := other.(*GoEnv); ok {
		return e.V == other.V
	}

	return false
}

func (e *GoEnv) Eval(env Env) Value {
	return typeConvIn(e.V)
}

func (e *GoEnv) reflect() {
	if e.fields != nil && e.methods != nil {
		// nothing to do
		return
	}

	ptrTyp := reflect.TypeOf(e.V)
	typ := ptrTyp
	if typ.Kind() == reflect.Ptr || typ.Kind() == reflect.Interface {
		typ = typ.Elem()
	}

	e.fields = make(map[Symbol][]int)
	for i := 0; i < typ.NumField(); i++ {
		fieldType := typ.Field(i)
		e.fields[Symbol(fieldType.Name)] = fieldType.Index
	}

	e.methods = make(map[Symbol]int)
	for i := 0; i < ptrTyp.NumMethod(); i++ {
		methodType := ptrTyp.Method(i)
		e.methods[Symbol(methodType.Name)] = methodType.Index
	}
}

func (e *GoEnv) Map() map[Symbol]Value {
	m := make(map[Symbol]Value)

	e.reflect()

	for field, index := range e.fields {
		m[field] = e.getField(index)
	}

	for method, index := range e.methods {
		m[method] = e.getMethod(index)
	}

	return m
}

func (e *GoEnv) Define(s Symbol, v Value) {
	exception(ErrImmutableEnv, e)
}

func (e *GoEnv) Defined(s Symbol) bool {
	m := e.Map()
	if _, ok := m[s]; ok {
		return true
	}
	return false
}

func (e *GoEnv) Set(s Symbol, v Value) {
	e.reflect()

	if index, ok := e.fields[s]; ok {
		e.setField(s, index, v)
		return
	}

	if _, ok := e.methods[s]; ok {
		exception(ErrImmutable, s)
	}

	exception(ErrNoSymbol, s)
}

func (e *GoEnv) Get(s Symbol) Value {
	e.reflect()

	if index, ok := e.fields[s]; ok {
		return e.getField(index)
	}

	if index, ok := e.methods[s]; ok {
		return e.getMethod(index)
	}

	return nil
}

var ErrImmutableSet = fmt.Errorf("%s, binding on non-pointer value?", ErrImmutable.Error())

func (e *GoEnv) setField(s Symbol, index []int, v Value) {
	e.reflect()

	envVal := reflect.ValueOf(e.V)
	if envVal.Kind() == reflect.Ptr || envVal.Kind() == reflect.Interface {
		envVal = envVal.Elem()
	}

	field := envVal.FieldByIndex(index)

	if !field.CanSet() {
		exception(ErrImmutableSet, s)
	}

	convValue := typeConvOut(v)
	field.Set(reflect.ValueOf(convValue))
}

func (e *GoEnv) getField(index []int) *GoEnv {
	e.reflect()

	envVal := reflect.ValueOf(e.V)
	if envVal.Kind() == reflect.Ptr || envVal.Kind() == reflect.Interface {
		envVal = envVal.Elem()
	}

	val := envVal.FieldByIndex(index)
	return &GoEnv{V: val.Interface()}
}

func (e *GoEnv) getMethod(index int) Value {
	e.reflect()

	return GoFunc{e.V, index}
}

func (e *GoEnv) With(env Env) Env {
	return goEnvInst{e, env}
}

// goEnvInst is an instance of a GoEnv. Used to provide a parent without modifying the original
// object.
type goEnvInst struct {
	*GoEnv
	parent Env
}

func (e goEnvInst) Defined(s Symbol) bool {
	if e.GoEnv.Defined(s) {
		return true
	}

	return e.parent.Defined(s)
}

func (e goEnvInst) Set(s Symbol, v Value) {
	if e.GoEnv.Defined(s) {
		e.GoEnv.Set(s, v)
		return
	}

	e.parent.Set(s, v)
}

func (e goEnvInst) Get(s Symbol) Value {
	if v := e.GoEnv.Get(s); v != nil {
		return v
	}

	return e.parent.Get(s)
}

func (e goEnvInst) Parent() Env {
	return e.parent
}
