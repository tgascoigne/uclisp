package uclisp

import "reflect"

// Bind produces a value which can be used to access and manipulate go values from lisp
func Bind(o interface{}) Elem {
	return BindValue(reflect.ValueOf(o))
}

// BindValue produces a value which can be used to access and manipulate go values from lisp
func BindValue(o reflect.Value) Elem {
	if o.Kind() == reflect.Int {
		return Integer(int(o.Int()))
	}

	if o.Kind() == reflect.String {
		return String(o.String())
	}

	return &GoValue{V: o}
}

// UnbindValue attempts to unwrap a lisp value into a go native type
func Unbind(v Elem) interface{} {
	if v, err := AssertInteger(v); err == nil {
		return int(v)
	}

	if v, err := AssertString(v); err == nil {
		return string(v)
	}

	// Can't do any type conversion
	return v
}
