package uclisp

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
)

func bootVM(t *testing.T) *VM {
	data, err := ioutil.ReadFile("boot.lbc")
	if err != nil {
		panic(err)
	}

	control, err := Read(string(data))
	assert.NoError(t, err, "Error reading boot code")

	vm := NewVM(true)
	for _, sexpr := range control {
		vm.Eval(vm.Compile(sexpr))
	}

	return vm
}

func TestDefine(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(define foo 1) foo")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, Int(1), result)
}

func TestLambda2(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(define foo (lambda (x y) (+ x y))) (foo 3 4)")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, Int(7), result)
}
