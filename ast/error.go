package ast

import (
	"errors"
	"fmt"
)

var ErrArgumentCount = errors.New("wrong number of arguments")

type DetailedError struct {
	err    error
	detail interface{}
}

func (e DetailedError) Cause() error {
	return e.err
}

func (e DetailedError) Error() string {
	return fmt.Sprintf("%v: %v", e.err.Error(), e.detail)
}

func exceptionArgCount(callee string, given int) {
	exception(ErrArgumentCount, fmt.Sprintf("%v, got %v", callee, given))
}

func exceptionArgCountExpected(callee string, expected, given int) {
	exception(ErrArgumentCount, fmt.Sprintf("%v, wanted %v got %v", callee, expected, given))
}

func exception(err error, detail interface{}) {
	detailedError := DetailedError{err, detail}
	panic(detailedError)
}
