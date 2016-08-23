package uclisp

import "fmt"

const (
	QuoteSymbol    = Symbol("quote")
	BytecodeSymbol = Symbol("bytecode")
)

func Quote(elem Elem) Elem {
	return List(QuoteSymbol, elem)
}

func Bytecode(elem Elem) Elem {
	return List(BytecodeSymbol, elem)
}

func Compile(elem Elem) Cell {
	result := compile(elem)
	return List(result...)
}

func compile(elem Elem) []Elem {
	if Nilp(elem) {
		return []Elem{OpLOAD, Nil}
	}

	if expr, ok := elem.(Cell); ok {
		return compileExpr(expr)
	}

	if sym, ok := elem.(Symbol); ok {
		return []Elem{OpLOAD, sym, OpLOOKUP}
	}

	if i, ok := elem.(Int); ok {
		return []Elem{OpLOAD, i}
	}

	panic(fmt.Sprintf("unknown compile element: %v", elem))
}

func compileExpr(expr Cell) []Elem {
	fn := expr.Car()

	// TODO insert compiler macros
	if fn.Equal(QuoteSymbol) {
		return []Elem{OpLOAD, AssertCell(expr.Cdr()).Car()}
	}

	if fn.Equal(BytecodeSymbol) {
		return compileBytecode(AssertCell(AssertCell(expr.Cdr()).Car()))
	}

	args := AssertCell(expr.Cdr())
	instrs := append(compileList(args), compile(fn)...)
	return append(instrs, OpAPPLY)
}

func compileBytecode(instructions Cell) []Elem {
	return instructions.ExpandList()
}

func compileList(list Cell) []Elem {
	instrs := []Elem{OpLOAD, Nil}
	list.reverse().forEach(func(e Elem) bool {
		instrs = append(instrs, compile(e)...)
		instrs = append(instrs, OpCONS)
		return false
	})
	return instrs
}
