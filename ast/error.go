package ast

import "fmt"

func exception(err error, detail string) {
	panic(fmt.Errorf("%v: %v", err.Error(), detail))
}
