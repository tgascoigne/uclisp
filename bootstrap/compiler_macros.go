package bootstrap

import "github.com/tgascoigne/uclisp/vm"

func init() {
	macros[vm.Symbol("quote")] = (*Compiler).macroQuote
	macros[vm.Symbol("define")] = (*Compiler).macroDefine
	macros[vm.Symbol("set")] = (*Compiler).macroDefine
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
	result = append(result, c.compileQuoted(symbol), vm.OpLOOKUPC)
	// Set the new value
	result = append(result, vm.OpSETCDR)
	// Lookup the new var for the return value
	result = append(result, c.compile(symbol))
	return c.interpolate(result)
}
