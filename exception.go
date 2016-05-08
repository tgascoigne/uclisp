package uclisp

import (
	"errors"
	"fmt"
)

var ErrArgCount = errors.New("argument count mismatch: expected %v")

// Raise raises an error
func Raise(err error, args ...interface{}) {
	// todo: replace with proper exception handling
	panic(fmt.Errorf(err.Error(), args...))
}
