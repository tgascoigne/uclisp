package uclisp

import "fmt"

// VM is an instance of the virtual machine
type VM struct {
	trace      bool
	s, e, c, d Cell
}

// NewVM constructs a VM
func NewVM(trace bool) *VM {
	vm := &VM{
		trace: trace,
		s:     Cons(Nil, Nil),
		e:     Cons(Nil, Nil),
		c:     Cons(Nil, Nil),
		d:     Cons(Nil, Nil),
	}

	e := List(mapToAlist(map[Elem]Elem{
		Symbol("%stack"):   vm.s,
		Symbol("%env"):     vm.e,
		Symbol("%control"): vm.c,
		Symbol("%dump"):    vm.d,
	}))
	vm.e.SetCar(e)

	return vm
}

func (vm *VM) Eval(expr Elem) Elem {
	instrs := Compile(expr)

	s, e, c, d :=
		AssertCell(vm.s.Car()),
		AssertCell(vm.e.Car()),
		AssertCell(vm.c.Car()),
		AssertCell(vm.d.Car())

	// Store the current s,e,c to d
	d = push(List(s, e, c), d)

	// Create a new frame
	//e = push(Nil, e)
	vm.s.SetCar(Nil)
	vm.e.SetCar(e)
	vm.c.SetCar(instrs)

	vm.execute()
	result, _ := pop(AssertCell(vm.s.Car()))

	// Restore the old s,e,c from d
	sec, d := pop(d)
	regs := AssertCell(sec).ExpandList()
	s, e, c = AssertCell(regs[0]),
		AssertCell(regs[1]),
		AssertCell(regs[2])

	vm.s.SetCar(s)
	//vm.e.SetCar(e)
	vm.c.SetCar(c)
	vm.d.SetCar(d)

	return result
}

// Execute processes until the control register is empty
func (vm *VM) execute() {
	s, e, c, d :=
		AssertCell(vm.s.Car()),
		AssertCell(vm.e.Car()),
		AssertCell(vm.c.Car()),
		AssertCell(vm.d.Car())

	for !c.Equal(Nil) {
		if vm.trace {
			fmt.Printf("s: %v\ne: %v\nc: %v\nd: %v\n\n", s, e, c, d)
		}
		s, e, c, d = step(s, e, c, d)

		vm.s.SetCar(s)
		vm.e.SetCar(e)
		vm.c.SetCar(c)
		vm.d.SetCar(d)
	}
}

// Registers returns the set of SECD registers
func (vm *VM) Registers() (Cell, Cell, Cell, Cell) {
	return vm.s, vm.e, vm.c, vm.d
}

func step(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	var op Op
	op, c = popOp(c)

	return opTable[op](s, e, c, d)
}

type stepFunc func(s, e, c, d Cell) (Cell, Cell, Cell, Cell)

var opTable = map[Op]stepFunc{
	OpNOP:     instNOP,
	OpLOAD:    instLOAD,
	OpLOOKUP:  instLOOKUP,
	OpLOOKUPC: instLOOKUPC,
	OpCONS:    instCONS,
	OpCAR:     instCAR,
	OpCDR:     instCDR,
	OpSETCAR:  instSETCAR,
	OpSETCDR:  instSETCDR,
	OpAPPLY:   instAPPLY,
	OpRETURN:  instRETURN,
	OpSELECT:  instSELECT,
	OpJOIN:    instJOIN,
	OpEQUAL:   instEQUAL,
	OpADD:     instADD,
	OpSUB:     instSUB,
	OpMUL:     instMUL,
	OpDIV:     instDIV,
	OpMOD:     instMOD,
}

func instNOP(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	return s, e, c, d
}

func instLOAD(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	v, c := pop(c)
	s = push(v, s)
	return s, e, c, d
}

func instLOOKUP(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	c = push(OpCDR, c)
	c = push(OpLOOKUPC, c)
	return s, e, c, d
}

func instLOOKUPC(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	sym, s := popSymbol(s)
	if !e.forEach(func(el Elem) bool {
		pair := assoc(sym, AssertCell(el))
		if !pair.Equal(Nil) {
			s = push(pair, s)
			return true
		}
		return false
	}) {
		s = push(Nil, s)
	}
	return s, e, c, d
}

func instCONS(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	car, s := pop(s)
	cdr, s := pop(s)
	s = push(Cons(car, cdr), s)
	return s, e, c, d
}

func instCAR(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	s = push(cell.Car(), s)
	return s, e, c, d
}

func instCDR(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	s = push(cell.Cdr(), s)
	return s, e, c, d
}

func instSETCAR(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	cell.car, s = pop(s)
	return s, e, c, d
}

func instSETCDR(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	cell.cdr, s = pop(s)
	return s, e, c, d
}

func instAPPLY(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	fn, s := popCell(s)
	args, s := popCell(s)
	d = push(List(s, e, c), d)
	s = Nil
	e = push(pairlis(AssertCell(fn.Car()), args), e)

	// todo: compile instruction
	c = Compile(AssertCell(fn.Cdr()))
	return s, e, c, d
}

func instRETURN(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	retFrame, d := popCell(d)
	result, s := pop(s)
	list := retFrame.ExpandList()
	s, e, c = AssertCell(list[0]), AssertCell(list[1]), AssertCell(list[2])
	s = push(result, s)
	return s, e, c, d
}

func instSELECT(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	p1, c := popCell(c)
	p2, c := popCell(c)
	cond, s := pop(s)
	var path Cell
	if cond.Equal(Nil) {
		path = p2
	} else {
		path = p1
	}
	d = push(c, d)

	return s, e, path, d
}

func instJOIN(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	c, d = popCell(d)
	return s, e, c, d
}

func instEQUAL(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := pop(s)
	b, s := pop(s)
	result := a.Equal(b)
	s = push(Bool(result), s)
	return s, e, c, d
}

func instADD(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a + b)
	s = push(result, s)
	return s, e, c, d
}

func instSUB(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a - b)
	s = push(result, s)
	return s, e, c, d
}

func instMUL(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a * b)
	s = push(result, s)
	return s, e, c, d
}

func instDIV(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a / b)
	s = push(result, s)
	return s, e, c, d
}

func instMOD(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a % b)
	s = push(result, s)
	return s, e, c, d
}
