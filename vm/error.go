package vm

import "fmt"

// Raise signals an error
func Raise(err error) {
	panic(err)
}

func typeError(typ string, value Elem) {
	Raise(fmt.Errorf("Incorrect type: wanted %v, %v", typ, value))
}
