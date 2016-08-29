package uclisp

import "fmt"

const (
	QuoteSymbol    = Symbol("quote")
	BytecodeSymbol = Symbol("bytecode")
	MacroSymbol    = Symbol("macro")
	LambdaSymbol   = Symbol("lambda")
)

func Quote(elem Elem) Elem {
	return List(QuoteSymbol, elem)
}

func Bytecode(elem Elem) Elem {
	return List(BytecodeSymbol, elem)
}

func (vm *VM) Compile(elem Elem) Cell {
	fmt.Printf("Compiling %v\n", elem)
	result := vm.compile(elem)
	fmt.Printf("Compile %v -> %v\n", elem, result)
	return List(result...)
}

func (vm *VM) compile(elem Elem) []Elem {
	if Nilp(elem) {
		return []Elem{OpLOAD, Nil}
	}

	if expr, ok := elem.(Cell); ok {
		return vm.compileExpr(expr)
	}

	if sym, ok := elem.(Symbol); ok {
		return []Elem{OpLOAD, sym, OpLOOKUP}
	}

	if i, ok := elem.(Int); ok {
		return []Elem{OpLOAD, i}
	}

	panic(fmt.Sprintf("unknown compile element: %v", elem))
}

func (vm *VM) compileExpr(expr Cell) []Elem {
	fn := expr.Car()

	if _, ok := fn.(Symbol); ok {
		// Check if the symbol is a macro
		result := vm.Eval(vm.Compile(fn))
		if fn, ok := result.(Cell); ok {
			fmt.Printf("fn %v is a %v\n", expr.Car(), fn.Car())
			if fn.Car().Equal(MacroSymbol) {
				fmt.Printf("expanding macro %v\n", expr.Car())
				args := expr.Cdr()
				expansion := AssertCell(vm.Eval(List(OpLOAD, args, OpLOAD, fn, OpAPPLY)))
				return vm.Compile(expansion).ExpandList()
			}
		}
	}

	if fn.Equal(QuoteSymbol) {
		return []Elem{OpLOAD, AssertCell(expr.Cdr()).Car()}
	}

	if fn.Equal(BytecodeSymbol) {
		return vm.compileBytecode(AssertCell(AssertCell(expr.Cdr()).Car()))
	}

	args := AssertCell(expr.Cdr())
	instrs := append(vm.compileList(args), vm.compile(fn)...)
	return append(instrs, OpAPPLY)
}

func (vm *VM) compileBytecode(instr Cell) []Elem {
	var elem Elem
	var opcode Op

	result := make([]Elem, 0)

	for !instr.Equal(Nil) {
		elem, instr = pop(instr)
		opSym := AssertSymbol(elem)
		if op, ok := opCodeMap[string(opSym)]; ok {
			opcode = op
		} else {
			panic(fmt.Sprintf("invalid opcode: %v", opSym))
		}

		result = append(result, opcode)

		switch opcode {
		case OpLOAD:
			elem, instr = pop(instr)
			result = append(result, elem)

		case OpSELECT:
			elem, instr = pop(instr)
			result = append(result, elem)

			elem, instr = pop(instr)
			result = append(result, elem)

		default:

		}

	}
	return result
}

func (vm *VM) compileList(list Cell) []Elem {
	instrs := []Elem{OpLOAD, Nil}
	list.reverse().forEach(func(e Elem) bool {
		instrs = append(instrs, vm.compile(e)...)
		instrs = append(instrs, OpCONS)
		return false
	})
	return instrs
}
