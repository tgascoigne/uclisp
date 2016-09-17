package bootstrap

import (
	"fmt"

	"github.com/tgascoigne/uclisp/vm"
)

func init() {
	macros[vm.Symbol("quote")] = (*Compiler).macroQuote
	macros[vm.Symbol("define")] = (*Compiler).macroDefine
	macros[vm.Symbol("set")] = (*Compiler).macroSet
	macros[vm.Symbol("progn")] = (*Compiler).macroProgn
	macros[vm.Symbol("bytecode")] = (*Compiler).macroBytecode
	macros[vm.Symbol("lambda")] = (*Compiler).macroLambda
	macros[vm.Symbol("if")] = (*Compiler).macroIf
	macros[vm.Symbol("let")] = (*Compiler).macroLet
	macros[vm.Symbol("backquote")] = (*Compiler).compileBackquoted
}

func (c *Compiler) macroQuote(elem vm.Elem) []vm.Elem {
	if cell, ok := elem.(vm.Cell); ok {
		return c.compileQuoted(cell.Car())
	}
	return c.compileQuoted(elem)
}

func (c *Compiler) macroDefine(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem).ExpandList()
	symbol := args[0]
	value := args[1]

	result := make([]interface{}, 0)

	// Load the env list
	result = append(result, vm.OpLOAD, vm.EnvReg, vm.OpLOOKUP, vm.OpCAR)
	// Load the current env
	result = append(result, vm.OpCAR)
	// Create the new binding pair
	result = append(result, c.compile(value))
	result = append(result, c.compileQuoted(symbol), vm.OpCONS) // symbol is implicitly quoted, a-la setq
	// Cons it onto the current env
	result = append(result, vm.OpCONS)
	// Load the env list (again)
	result = append(result, vm.OpLOAD, vm.EnvReg, vm.OpLOOKUP, vm.OpCAR)
	// Update the current env to the newly updated list
	result = append(result, vm.OpSETCAR)
	// Lookup the new var for the return value
	result = append(result, c.compile(symbol))
	return c.interpolate(result)
}

func (c *Compiler) macroSet(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem).ExpandList()
	symbol := args[0]
	value := args[1]

	result := make([]interface{}, 0)

	// Compile the new value
	result = append(result, c.compile(value))
	// Load the cell containing the var binding
	result = append(result, c.compile(symbol), vm.OpLOOKUPC)
	// Set the new value
	result = append(result, vm.OpSETCDR)
	// Lookup the new var for the return value
	result = append(result, c.compile(symbol))
	return c.interpolate(result)
}

func (c *Compiler) macroProgn(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem).ExpandList()

	result := make([]interface{}, 0)

	for _, expr := range args[:len(args)-1] {
		// Compile all but the final expr, ignoring the result
		result = append(result, c.compile(expr), vm.OpDROP)
	}

	// Compile the final expr, and return the result
	finalExpr := args[len(args)-1]
	result = append(result, c.compile(finalExpr))
	return c.interpolate(result)
}

func (c *Compiler) macroBytecode(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem).ExpandList()
	instrs := vm.AssertCell(args[0]).ExpandList()

	result := make([]interface{}, 0)

	for i := 0; i < len(instrs); i++ {
		opSym := instrs[i]
		op, ok := vm.ParseOp(vm.AssertSymbol(opSym))
		if !ok {
			panic(fmt.Sprintf("unsupported opcode: %v", opSym))
		}

		result = append(result, op)

		if op == vm.OpLOAD {
			i++
			result = append(result, instrs[i])
		}
	}

	return c.interpolate(result)
}

func (c *Compiler) macroLambda(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem)
	argSpec := args.Car()
	body := vm.AssertCell(args.Cdr())

	bodyInstrs := vm.List(append(c.macroProgn(body), vm.OpRETURN)...)

	return c.compile(vm.List(vm.Symbol("quote"),
		vm.List(vm.Symbol("lambda"), argSpec,
			bodyInstrs)))
}

func (c *Compiler) macroIf(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem).ExpandList()

	pred := args[0]
	then := args[1]
	els := vm.Elem(vm.Nil)
	if len(args) == 3 {
		els = args[2]
	}

	result := make([]interface{}, 0)

	result = append(result, c.compile(pred))
	result = append(result, vm.OpLOAD, vm.List(append(c.compile(then), vm.OpJOIN)...))
	result = append(result, vm.OpLOAD, vm.List(append(c.compile(els), vm.OpJOIN)...))
	result = append(result, vm.OpSELECT)
	return c.interpolate(result)
}

func (c *Compiler) macroLet(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem)
	bindings := vm.AssertCell(args.Car())
	body := vm.AssertCell(args.Cdr())

	argSpec, args := vm.Nil, vm.Nil
	for _, bind := range bindings.ExpandList() {
		bindCell := vm.AssertCell(bind)
		argSpec = vm.Cons(bindCell.Car(), argSpec)
		args = vm.Cons(vm.Cadr(bindCell), args)
	}

	result := make([]interface{}, 0)
	result = append(result, c.compileList(args))
	result = append(result, c.macroLambda(vm.Cons(argSpec, body)))
	result = append(result, vm.OpAPPLY)

	return c.interpolate(result)
}

func (c *Compiler) compileBackquotedList(list vm.Cell) []vm.Elem {
	result := make([]interface{}, 0)
	result = append(result, vm.OpLOAD, vm.Nil)

	fmt.Printf("compiling backquoted list %v\n", list)

	if list == vm.Nil {
		return c.interpolate(result)
	}

	list.Reverse().ForEach(func(el vm.Elem) bool {
		if vm.Consp(el) {
			el := vm.AssertCell(el)
			if el.Car().Equal(vm.Symbol("splice")) {
				/*				instrs := vm.List(c.compile(vm.Cadr(el))...)
								fmt.Printf("evaling %v to splice\n", instrs)
								cell := vm.AssertCell(c.VM.Eval(instrs))
								fmt.Printf("splicing %v in\n", cell)
								return cell.Reverse().ForEach(func(el vm.Elem) bool {
									result = append(result, c.compile(el))
									result = append(result, vm.OpCONS)
									return false
								})*/

				result = append(result, c.compile(vm.Cadr(el)))
				return false
			}
		}
		result = append(result, c.compileBackquoted(vm.List(el)))
		result = append(result, vm.OpCONS)
		return false
	})

	return c.interpolate(result)
}

func (c *Compiler) compileBackquoted(elem vm.Elem) []vm.Elem {
	args := vm.AssertCell(elem)
	elem = args.Car()

	if vm.Consp(elem) {
		elem := vm.AssertCell(elem)
		if elem.Car().Equal(vm.Symbol("unquote")) {
			return c.compile(vm.Cadr(elem))
		} else {
			return c.compileBackquotedList(elem)
		}
	} else {
		return c.compileQuoted(elem)
	}
}
