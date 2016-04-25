package ast

import "errors"

var ErrNotCallable = errors.New("not a callable expression")

// Callable is a value which can be called with a list of arguments
type Callable interface {
	Value
	Call(args List) Value
}
