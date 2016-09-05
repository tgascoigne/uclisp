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

	vm := NewVM()

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

func TestCar(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(car '(foo (1 2)))")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, Symbol("foo"), result)
}

func TestCdr(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(cdr '(foo (1 2)))")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, List(List(Int(1), Int(2))), result)
}

func TestAppend(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(define foo '(1 2)) (append foo 3) foo")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem

	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, List(Int(1), Int(2), Int(3)), result)
}

func TestIf1(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(if 1 1 2)")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, Int(1), result)
}

func TestIf2(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(if '() 1 2)")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, Int(2), result)
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

func TestMap(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("(map '(1 2 3) (lambda (x) (+ x 1)))")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, List(Int(2), Int(3), Int(4)), result)
}

func TestBackquote(t *testing.T) {
	vm := bootVM(t)

	control, err := Read("`(foo ,(+ 1 1))")
	assert.NoError(t, err, "Error reading bytecode")

	var result Elem
	for _, sexpr := range control {
		result = vm.Eval(vm.Compile(sexpr))
	}

	assert.Equal(t, List(Symbol("foo"), Int(2)), result)
}
