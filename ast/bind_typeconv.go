package ast

import "reflect"

// Bind produces a value which can be used to access and manipulate go values from lisp
func Bind(o interface{}) Value {
	return BindValue(reflect.ValueOf(o))
}

// BindValue produces a value which can be used to access and manipulate go values from lisp
func BindValue(o reflect.Value) Value {
	if o.Kind() == reflect.Int {
		return Integer(int(o.Int()))
	}

	return &GoEnv{V: o}
}

func typeConvOut(v Value) interface{} {
	if v.Type() == IntegerType {
		return int(v.(Integer))
	}

	// Can't do any type conversion
	return v
}
