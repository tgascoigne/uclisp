package ast

import (
	"errors"
	"fmt"
)

var ErrArgumentCount = errors.New("wrong number of arguments")

func exceptionArgCount(callee string, given int) {
	exception(ErrArgumentCount, fmt.Sprintf("%v, got %v", callee, given))
}

func exception(err error, detail interface{}) {
	panic(fmt.Errorf("%v: %v", err.Error(), detail))
}
