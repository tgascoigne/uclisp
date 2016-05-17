package uclisp

import (
	"errors"
	"fmt"
)

const (
	BacktraceDepth int = 4
)

var ErrArgCount = errors.New("argument count mismatch: got %v expected %v")

type Exception struct {
	error  error
	detail []interface{}
	trace  Trace
}

func (exc *Exception) BaseError() error {
	return exc.error
}

func (exc *Exception) Trace() *Trace {
	return &exc.trace
}

func (exc Exception) Error() string {
	errorString := fmt.Sprintf(exc.error.Error(), exc.detail...)
	trace := exc.Trace().Dump(BacktraceDepth)
	return fmt.Sprintf("Error: %v\n\nTrace:\n%v\n", errorString, trace)
}

// Raise raises an error
func Raise(err error, args ...interface{}) {
	exc := Exception{err, args, *callStack}
	panic(exc)
}
