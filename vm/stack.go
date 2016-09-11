package vm

func push(e Elem, c Cell) Cell {
	return Cons(e, c)
}

func pushAll(c1, c2 Cell) Cell {
	c1.Reverse().ForEach(func(el Elem) bool {
		c2 = push(el, c2)
		return false
	})
	return c2
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
