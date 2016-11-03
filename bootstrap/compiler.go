package bootstrap

import (
	"fmt"

	"github.com/tgascoigne/uclisp/vm"
)

type macroFunc func(*Compiler, vm.Elem) []vm.Elem

var macros = map[vm.Symbol]macroFunc{}

func Compile(vm *vm.VM, elem vm.Elem) vm.Cell {
	return NewCompiler(vm).Compile(elem)
}

type Compiler struct {
	*vm.VM
}

func NewCompiler(vm *vm.VM) *Compiler {
	return &Compiler{vm}
}

func (c *Compiler) Compile(elem vm.Elem) vm.Cell {
	return vm.List(c.compile(elem)...)
}

func (c *Compiler) compile(elem vm.Elem) []vm.Elem {
	//fmt.Printf("compiling %v\n", elem)
	switch elem := elem.(type) {
	case vm.Cell:
		return c.compileExpr(elem)
	case vm.Symbol:
		return c.compileLookup(elem)
	case vm.Int:
		return c.compileConstant(elem)
	case vm.Op:
		return c.compileConstant(elem)
	default:
		panic(fmt.Sprintf("unknown compile element: %v\n", elem))
	}
	panic("unreachable")
}

func (c *Compiler) compileQuoted(elem vm.Elem) []vm.Elem {
	switch elem := elem.(type) {
	case vm.Cell:
		return c.compileQuotedList(elem)
	case vm.Symbol:
		return c.compileConstant(elem)
	case vm.Int:
		return c.compileConstant(elem)
	case vm.Op:
		return c.compileConstant(elem)
	default:
		panic(fmt.Sprintf("unknown compile element: %v\n", elem))
	}
	panic("unreachable")
}

func (c *Compiler) compileConstant(elem vm.Elem) []vm.Elem {
	return []vm.Elem{vm.OpLOAD, elem}
}

func (c *Compiler) compileLookup(elem vm.Elem) []vm.Elem {
	return c.interpolate(c.compileConstant(elem), vm.OpLOOKUP)
}

func (c *Compiler) isMacro(elem vm.Elem) macroFunc {
	if sym, ok := elem.(vm.Symbol); ok {
		return macros[sym]
	}
	return nil
}

func (c *Compiler) expandMacro(fn macroFunc, list vm.Cell) []vm.Elem {
	return fn(c, list)
}

func (c *Compiler) compileExpr(list vm.Cell) []vm.Elem {
	//fmt.Printf("compileExpr %v\n", list)
	if macro := c.isMacro(list.Car()); macro != nil {
		return c.expandMacro(macro, vm.AssertCell(list.Cdr()))
	}

	if list == vm.Nil {
		return c.compileConstant(vm.Nil)
	}

	result := make([]interface{}, 0)
	// Compile the args
	result = append(result, c.compileList(vm.AssertCell(list.Cdr())))
	// Compile the function
	result = append(result, c.compile(list.Car()))
	// Apply!
	result = append(result, vm.OpAPPLY)
	return c.interpolate(result)
}

func (c *Compiler) compileList(list vm.Cell) []vm.Elem {
	//fmt.Printf("compileList %v\n", list)
	result := make([]interface{}, 0)
	result = append(result, vm.OpLOAD, vm.Nil)

	if list == vm.Nil {
		return c.interpolate(result)
	}

	list.Reverse().ForEach(func(el vm.Elem) bool {
		result = append(result, c.compile(el))
		result = append(result, vm.OpCONS)
		return false
	})

	return c.interpolate(result)
}

func (c *Compiler) compileQuotedList(list vm.Cell) []vm.Elem {
	result := make([]interface{}, 0)
	result = append(result, vm.OpLOAD, vm.Nil)

	if list == vm.Nil {
		return c.interpolate(result)
	}

	list.Reverse().ForEach(func(el vm.Elem) bool {
		result = append(result, c.compileQuoted(el))
		result = append(result, vm.OpCONS)
		return false
	})

	return c.interpolate(result)
}

func (c *Compiler) interpolate(instrs ...interface{}) []vm.Elem {
	result := make([]vm.Elem, 0)
	for _, el := range instrs {
		if nested, ok := el.([]vm.Elem); ok {
			for _, el := range nested {
				result = append(result, c.interpolate(el)...)
			}
			continue
		}

		if nested, ok := el.([]interface{}); ok {
			result = append(result, c.interpolate(nested...)...)
			continue
		}

		if el, ok := el.(vm.Elem); ok {
			result = append(result, el)
			continue
		}

		panic(fmt.Sprintf("cannot interpolate type: %T", el))
	}
	return result
}
