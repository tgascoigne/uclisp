package vm

import "fmt"

const TraceVar = Symbol("*trace*")
const HaltVar = Symbol("*halt*")

const (
	StackReg     = Symbol("%stack")
	EnvReg       = Symbol("%env")
	ControlReg   = Symbol("%control")
	DumpReg      = Symbol("%dump")
	LambdaSymbol = Symbol("lambda")
)

type ReadFunc func() Elem
type PrintFunc func(Elem)

// VM is an instance of the virtual machine
type VM struct {
	s, e, c, d Cell
	traceCell  Cell
	haltCell   Cell
	ReadFunc
	PrintFunc
}

// NewVM constructs a VM
func NewVM() *VM {
	vm := &VM{
		s: Cons(Nil, Nil),
		e: Cons(Nil, Nil),
		c: Cons(Nil, Nil),
		d: Cons(Nil, Nil),
	}

	baseEnv := map[Elem]Elem{
		StackReg:   vm.s,
		EnvReg:     vm.e,
		ControlReg: vm.c,
		DumpReg:    vm.d,
		TraceVar:   Nil,
		HaltVar:    Nil,
	}

//	for str, op := range opCodeMap {
//		sym := Symbol("$" + str)
//		baseEnv[sym] = op
//	}

	e := List(mapToAlist(baseEnv))

	vm.e.SetCar(e)

	vm.traceCell = AssertCell(vm.Eval(List(OpLOAD, TraceVar, OpLOOKUPC)))
	vm.haltCell = AssertCell(vm.Eval(List(OpLOAD, HaltVar, OpLOOKUPC)))

	return vm
}

func (vm *VM) Eval(instrs Cell) Elem {
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

func (vm *VM) Dump() {
	s, e, c, d :=
		AssertCell(vm.s.Car()),
		AssertCell(vm.e.Car()),
		AssertCell(vm.c.Car()),
		AssertCell(vm.d.Car())
	fmt.Printf("s: %v\ne: %v\nc: %v\nd: %v\n\n", s, e, c, d)
	//	fmt.Printf("s: %v\ne: %v\nc: %v\n\n", s, e, c)
}

// Execute processes until the control register is empty
func (vm *VM) execute() {
	s, e, c, d :=
		AssertCell(vm.s.Car()),
		AssertCell(vm.e.Car()),
		AssertCell(vm.c.Car()),
		AssertCell(vm.d.Car())
		/*
			defer func() {
				if r := recover(); r != nil {
					vm.Dump()
				}
			}()
		*/
	for !c.Equal(Nil) && vm.haltCell.Cdr().Equal(Nil) {
		if !vm.traceCell.Cdr().Equal(Nil) {
			vm.Dump()
		}
		s, e, c, d = vm.step(s, e, c, d)

		vm.s.SetCar(s)
		vm.e.SetCar(e)
		vm.c.SetCar(c)
		vm.d.SetCar(d)
	}
}

// Halt sets *halt* to t, causing the vm to immediately terminate
func (vm *VM) Halt() {
	vm.haltCell.SetCdr(True)
}

// Registers returns the set of SECD registers
func (vm *VM) Registers() (Cell, Cell, Cell, Cell) {
	return vm.s, vm.e, vm.c, vm.d
}

func (vm *VM) step(s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	var op Op
	op, c = popOp(c)

	return opTable[op](vm, s, e, c, d)
}

type stepFunc func(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell)

var opTable map[Op]stepFunc

func init() {
	opTable = map[Op]stepFunc{
		OpNOP:     instNOP,
		OpLOAD:    instLOAD,
		OpLOOKUP:  instLOOKUP,
		OpLOOKUPC: instLOOKUPC,
		OpCONS:    instCONS,
		OpCAR:     instCAR,
		OpCDR:     instCDR,
		OpSETCAR:  instSETCAR,
		OpSETCDR:  instSETCDR,
		OpTYPE:    instTYPE,
		OpAPPLY:   instAPPLY,
		OpDAPPLY:  instDAPPLY,
		OpRETURN:  instRETURN,
		OpEVAL:    instEVAL,
		OpDROP:    instDROP,
		OpDUP:     instDUP,
		OpSELECT:  instSELECT,
		OpJOIN:    instJOIN,
		OpEQUAL:   instEQUAL,
		OpADD:     instADD,
		OpSUB:     instSUB,
		OpMUL:     instMUL,
		OpDIV:     instDIV,
		OpMOD:     instMOD,
		OpREAD:    instREAD,
		OpPRINT:   instPRINT,
	}
}

func instNOP(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	return s, e, c, d
}

func instLOAD(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	v, c := pop(c)
	s = push(v, s)
	return s, e, c, d
}

func instLOOKUP(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	c = push(OpCDR, c)
	c = push(OpLOOKUPC, c)
	return s, e, c, d
}

func instLOOKUPC(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	sym, s := popSymbol(s)
	if !e.ForEach(func(el Elem) bool {
		pair := assoc(sym, AssertCell(el))
		if !pair.Equal(Nil) {
			s = push(pair, s)
			return true
		}
		return false
	}) {
		fmt.Printf("lookup of %v failed\n", sym)
		s = push(Nil, s)
	}
	return s, e, c, d
}

func instCONS(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	car, s := pop(s)
	cdr, s := pop(s)
	s = push(Cons(car, cdr), s)
	return s, e, c, d
}

func instCAR(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	s = push(cell.Car(), s)
	return s, e, c, d
}

func instCDR(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	s = push(cell.Cdr(), s)
	return s, e, c, d
}

func instSETCAR(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	cell.car, s = pop(s)
	return s, e, c, d
}

func instSETCDR(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	cell, s := popCell(s)
	cell.cdr, s = pop(s)
	return s, e, c, d
}

func instTYPE(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	elem, s := pop(s)
	s = push(elem.Type(), s)
	return s, e, c, d
}

func instAPPLY(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	fn, s := popCell(s)
	args, s := popCell(s)
	fnElems := fn.ExpandList()
	//fmt.Printf("fn is %v & %v & %v\n", fnElems[0], fnElems[1], fnElems[2])
	argSpec := AssertCell(fnElems[1])
	body := AssertCell(fnElems[2])
	//fmt.Printf("argspec is %v\n", argSpec)
	//fmt.Printf("args is %v\n", args)
	//fmt.Printf("body is %v\n", body)

	bindings := pairargs(argSpec, args)
	//fmt.Printf("bindings are %v\n", bindings)

	d = push(List(s, e, c), d)
	s = Nil
	e = push(bindings, e)
	c = body
	return s, e, c, d
}

func instDAPPLY(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	fn, s := popCell(s)
	bindings, s := popCell(s)
	fnElems := fn.ExpandList()
	body := AssertCell(fnElems[2])

	//fmt.Printf("bindings are %v\n", bindings)

	d = push(List(s, e, c), d)
	s = Nil
	e = push(bindings, e)
	c = body
	return s, e, c, d
}

func instRETURN(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	retFrame, d := popCell(d)
	result, s := pop(s)
	list := retFrame.ExpandList()
	s, e, c = AssertCell(list[0]), AssertCell(list[1]), AssertCell(list[2])
	s = push(result, s)
	return s, e, c, d
}

func instDROP(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	_, s = pop(s)
	return s, e, c, d
}

func instDUP(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	v, s := pop(s)
	s = push(v, s)
	s = push(v, s)
	return s, e, c, d
}

func instEVAL(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	code, s := pop(s)
	result := vm.Eval(AssertCell(code))
	s = push(result, s)
	return s, e, c, d
}

func instSELECT(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	p2, s := popCell(s)
	p1, s := popCell(s)
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

func instJOIN(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	c, d = popCell(d)
	return s, e, c, d
}

func instEQUAL(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := pop(s)
	b, s := pop(s)
	result := a.Equal(b)
	s = push(Bool(result), s)
	return s, e, c, d
}

func instADD(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a + b)
	s = push(result, s)
	return s, e, c, d
}

func instSUB(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a - b)
	s = push(result, s)
	return s, e, c, d
}

func instMUL(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a * b)
	s = push(result, s)
	return s, e, c, d
}

func instDIV(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a / b)
	s = push(result, s)
	return s, e, c, d
}

func instMOD(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	a, s := popInt(s)
	b, s := popInt(s)
	result := Int(a % b)
	s = push(result, s)
	return s, e, c, d
}

func instREAD(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	expr := vm.ReadFunc()
	s = push(expr, s)
	return s, e, c, d
}

func instPRINT(vm *VM, s, e, c, d Cell) (Cell, Cell, Cell, Cell) {
	expr, s := pop(s)
	vm.PrintFunc(expr)
	return s, e, c, d
}
