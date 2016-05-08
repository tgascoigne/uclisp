package uclisp

import "errors"

func IsNil(e Elem) bool {
	if list, ok := e.(List); ok {
		return len(list) == 0
	}

	return false
}

var ErrNotASymbol = errors.New("not a symbol: %v")

// AssertSymbol raises an exception if e is not a Symbol. Returns the concrete type on success.
func AssertSymbol(e Elem) Symbol {
	if sym, ok := e.(Symbol); ok {
		return sym
	}

	Raise(ErrNotASymbol, e)
	return Symbol("")
}

var ErrNotAList = errors.New("not a list: %v")

// AssertList raises an exception if e is not a List. Returns the concrete type on success.
func AssertList(e Elem) List {
	if list, ok := e.(List); ok {
		return list
	}

	Raise(ErrNotAList, e)
	return List{}
}

var ErrNotAnInteger = errors.New("not an integer: %v")

// AssertInteger raises an exception if e is not an Integer. Returns the concrete type on success.
func AssertInteger(e Elem) Integer {
	if integer, ok := e.(Integer); ok {
		return integer
	}

	Raise(ErrNotAnInteger, e)
	return Integer(0)
}

var ErrNotAString = errors.New("not a string: %v")

// AssertString raises an exception if e is not an String. Returns the concrete type on success.
func AssertString(e Elem) String {
	if string, ok := e.(String); ok {
		return string
	}

	Raise(ErrNotAString, e)
	return String(0)
}

var ErrNotAProcedure = errors.New("not a procedure: %v")

// AssertProcedure raises an exception if e is not an Procedure. Returns the concrete type on success.
func AssertProcedure(e Elem) Procedure {
	if proc, ok := e.(Procedure); ok {
		return proc
	}

	Raise(ErrNotAProcedure, e)
	return nil
}
