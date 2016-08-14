package uclisp

func push(e Elem, c Cell) Cell {
	return Cons(e, c)
}

func pop(stack Cell) (Elem, Cell) {
	return stack.Car(), AssertCell(stack.Cdr())
}

func popOp(stack Cell) (Op, Cell) {
	var e Elem
	e, stack = pop(stack)
	return AssertOp(e), stack
}

func popInt(stack Cell) (Int, Cell) {
	var e Elem
	e, stack = pop(stack)
	return AssertInt(e), stack
}

func popCell(stack Cell) (Cell, Cell) {
	var e Elem
	e, stack = pop(stack)
	return AssertCell(e), stack
}

func popSymbol(stack Cell) (Symbol, Cell) {
	var e Elem
	e, stack = pop(stack)
	return AssertSymbol(e), stack
}
