package bootstrap

import "github.com/tgascoigne/uclisp/vm"

func init() {
	macros[vm.Symbol("quote")] = (*Compiler).macroQuote
}

func (c *Compiler) macroQuote(elem vm.Elem) []vm.Elem {
	if cell, ok := elem.(vm.Cell); ok {
		return c.compileQuoted(cell.Car())
	}
	return c.compileQuoted(elem)
}
