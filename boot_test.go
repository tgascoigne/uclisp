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
	_, _, c, _ := vm.Registers()
	for _, instrs := range control {
		c.SetCar(instrs)
		vm.Execute()
	}

	return vm
}

func TestDefine(t *testing.T) {
	vm := bootVM(t)
	s, _, c, _ := vm.Registers()

	control, err := Read("<LOAD (foo 1) LOAD define LOOKUP APPLY LOAD foo LOOKUP>")
	assert.NoError(t, err, "Error reading bytecode")

	c.SetCar(control[0])

	vm.Execute()

	result, _ := pop(AssertCell(s.Car()))
	assert.Equal(t, Int(1), result)
}
