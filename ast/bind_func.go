package ast

import "reflect"

type GoFunc struct {
	v     reflect.Value
	index int
}

func (f GoFunc) Type() Type {
	return LambdaType
}

func (f GoFunc) IsNil() bool {
	return f.v.IsNil()
}

func (f GoFunc) Equals(env Env, other Value) bool {
	return false
}

func (f GoFunc) Call(env Env, args List) Value {
	recvVal := f.v
	method := recvVal.Method(f.index)

	goArgs := make([]reflect.Value, len(args))
	for i := range args {
		v := args[i].Eval(env)
		goArgs[i] = reflect.ValueOf(typeConvOut(v))
	}

	goOutputs := method.Call(goArgs)

	outputs := make(List, len(goOutputs))
	for i := range goOutputs {
		v := BindValue(goOutputs[i])
		switch v := v.(type) {
		case Form:
			outputs[i] = v
		default:
			outputs[i] = SimpleForm{v}
		}
	}

	if len(outputs) == 1 {
		return outputs[0].Eval(env)
	}

	return outputs
}
