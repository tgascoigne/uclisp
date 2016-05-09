package uclisp

import "errors"

func IsNil(e Elem) bool {
	if list, ok := e.(List); ok {
		return len(list) == 0
	}

	return false
}

var ErrNotASymbol = errors.New("not a symbol: %v")

// AssertSymbol returns an error if e is not a Symbol. Returns the concrete type on success.
func AssertSymbol(e Elem) (Symbol, error) {
	if sym, ok := e.(Symbol); ok {
		return sym, nil
	}

	return Symbol(""), ErrNotASymbol
}

var ErrNotAList = errors.New("not a list: %v")

// AssertList returns an error if e is not a List. Returns the concrete type on success.
func AssertList(e Elem) (List, error) {
	if list, ok := e.(List); ok {
		return list, nil
	}

	return List{}, ErrNotAList
}

var ErrNotAnInteger = errors.New("not an integer: %v")

// AssertInteger returns an error if e is not an Integer. Returns the concrete type on success.
func AssertInteger(e Elem) (Integer, error) {
	if integer, ok := e.(Integer); ok {
		return integer, nil
	}

	return Integer(0), ErrNotAnInteger
}

var ErrNotAString = errors.New("not a string: %v")

// AssertString returns an error if e is not an String. Returns the concrete type on success.
func AssertString(e Elem) (String, error) {
	if string, ok := e.(String); ok {
		return string, nil
	}

	return String(0), ErrNotAString
}

var ErrNotAProcedure = errors.New("not a procedure: %v")

// AssertProcedure returns an error if e is not an Procedure. Returns the concrete type on success.
func AssertProcedure(e Elem) (Procedure, error) {
	if proc, ok := e.(Procedure); ok {
		return proc, nil
	}

	return nil, ErrNotAProcedure
}
