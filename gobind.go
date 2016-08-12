package uclisp

import (
	"errors"
	"fmt"
	"reflect"
)

type GoValue struct {
	V reflect.Value
	// maps of symbol to reflect indices
	fields     map[Symbol][]int
	methods    map[Symbol]int
	ptrMethods map[Symbol]int
}

func (e *GoValue) Equals(ctx *Context, env Env, other Elem) bool {
	if other, ok := other.(*GoValue); ok {
		return e.V.Interface() == other.V.Interface()
	}

	return false
}

func (e *GoValue) Eval(ctx *Context, env Env) Elem {
	return BindValue(e.V)
}

func (e *GoValue) reflect() {
	if e.fields != nil && e.methods != nil {
		// nothing to do
		return
	}

	typ := e.V.Type()
	ptrType := typ
	if typ.Kind() == reflect.Ptr || typ.Kind() == reflect.Interface {
		typ = typ.Elem()
	}

	e.fields = make(map[Symbol][]int)
	for i := 0; i < typ.NumField(); i++ {
		fieldType := typ.Field(i)
		e.fields[Symbol(fieldType.Name)] = fieldType.Index
	}

	e.methods = make(map[Symbol]int)
	for i := 0; i < ptrType.NumMethod(); i++ {
		methodType := ptrType.Method(i)
		e.methods[Symbol(methodType.Name)] = methodType.Index
	}
}

func (e *GoValue) Map(ctx *Context) map[Symbol]Elem {
	m := make(map[Symbol]Elem)

	e.reflect()

	for field, index := range e.fields {
		m[field] = e.getField(ctx, index)
	}

	for method, index := range e.methods {
		m[method] = e.getMethod(ctx, index)
	}

	return m
}

var ErrImmutableValue = errors.New("immutable value")

func (e *GoValue) Define(s Symbol, v Elem) {
	panic(ErrImmutableValue)
}

func (e *GoValue) Defined(ctx *Context, s Symbol) bool {
	m := e.Map(ctx)
	if _, ok := m[s]; ok {
		return true
	}
	return false
}

func (e *GoValue) Set(ctx *Context, s Symbol, v Elem) {
	e.reflect()

	if index, ok := e.fields[s]; ok {
		e.setField(ctx, s, index, v)
		return
	}

	if _, ok := e.methods[s]; ok {
		panic(ErrImmutableValue)
	}

	ctx.Raise(ErrNotASymbol, s)
}

func (e *GoValue) Get(ctx *Context, s Symbol) Elem {
	e.reflect()

	if index, ok := e.fields[s]; ok {
		return e.getField(ctx, index)
	}

	if index, ok := e.methods[s]; ok {
		return e.getMethod(ctx, index)
	}

	return nil
}

var ErrImmutableSet = fmt.Errorf("%s, binding on non-pointer value?", ErrImmutableValue.Error())

func (e *GoValue) setField(ctx *Context, s Symbol, index []int, v Elem) {
	e.reflect()

	envVal := e.V
	if envVal.Kind() == reflect.Ptr || envVal.Kind() == reflect.Interface {
		envVal = envVal.Elem()
	}

	field := envVal.FieldByIndex(index)

	if !field.CanSet() {
		ctx.Raise(ErrImmutableSet, s)
	}

	convValue := Unbind(v)
	field.Set(reflect.ValueOf(convValue))
}

func (e *GoValue) getField(ctx *Context, index []int) Elem {
	e.reflect()

	envVal := e.V
	if envVal.Kind() == reflect.Ptr || envVal.Kind() == reflect.Interface {
		envVal = envVal.Elem()
	}

	val := envVal.FieldByIndex(index)
	return BindValue(val)
}

func (e *GoValue) getMethod(ctx *Context, index int) Elem {
	e.reflect()

	return NewGoProcedure(e.V, index)
}

func (e *GoValue) With(env Env) Env {
	return goEnvInst{e, env}
}

// goEnvInst is an instance of a GoValue. Used to provide a parent without modifying the original
// object.
type goEnvInst struct {
	*GoValue
	parent Env
}

func (e goEnvInst) Defined(ctx *Context, s Symbol) bool {
	if e.GoValue.Defined(ctx, s) {
		return true
	}

	return e.parent.Defined(ctx, s)
}

func (e goEnvInst) Define(s Symbol, v Elem) {
	e.parent.Define(s, v)
}

func (e goEnvInst) Set(ctx *Context, s Symbol, v Elem) {
	if e.GoValue.Defined(ctx, s) {
		e.GoValue.Set(ctx, s, v)
		return
	}

	e.parent.Set(ctx, s, v)
}

func (e goEnvInst) Get(ctx *Context, s Symbol) Elem {
	if v := e.GoValue.Get(ctx, s); v != nil {
		return v
	}

	return e.parent.Get(ctx, s)
}

func (e goEnvInst) Parent() Env {
	return e.parent
}

func NewGoProcedure(recvVal reflect.Value, index int) Procedure {
	return NewProcedure(func(ctx *Context, env Env, args []Elem) Elem {
		method := recvVal.Method(index)

		goArgs := make([]reflect.Value, len(args))
		for i := range args {
			v := args[i].Eval(ctx, env)
			goArgs[i] = reflect.ValueOf(Unbind(v))
		}

		goOutputs := method.Call(goArgs)

		outputs := make(List, len(goOutputs))
		for i := range goOutputs {
			v := BindValue(goOutputs[i])
			switch v := v.(type) {
			case List:
				outputs[i] = v
			default:
				outputs[i] = List{v}
			}
		}

		if len(outputs) == 1 {
			return outputs[0].Eval(ctx, env)
		}

		return outputs
	})
}
