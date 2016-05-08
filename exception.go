package uclisp

import (
	"errors"
	"fmt"
)

// Raise raises an error
func Raise(err error, args ...interface{}) {
	// todo: replace with proper exception handling
	panic(fmt.Errorf(err.Error(), args...))
}

var ErrNotAnInteger = errors.New("not an integer")

// AssertInt raises an exception if e is not an Integer. Returns the concrete type on success.
func AssertInt(e Elem) Integer {
	if integer, ok := e.(Integer); ok {
		return integer
	}

	Raise(ErrNotAnInteger, e)
	return Integer(0)
}

var ErrNotAProcedure = errors.New("not a procedure")

// AssertProc raises an exception if e is not an Procedure. Returns the concrete type on success.
func AssertProc(e Elem) Procedure {
	if proc, ok := e.(Procedure); ok {
		return proc
	}

	Raise(ErrNotAProcedure, e)
	return nil
}
