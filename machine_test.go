package uclisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func testSequence(t *testing.T, control Cell, expected Elem, description string) {
	testSequenceWithEnv(t, control, Nil, expected, description)
}

func testSequenceWithEnv(t *testing.T, control, env Cell, expected Elem, description string) {
	t.Logf("Test sequence: %v", description)

	vm := NewVM(false)
	s, e, c, _ := vm.Registers()
	c.SetCar(control)
	if !env.Equal(Nil) {
		realEnv := AssertCell(e.Car())
		env.reverse().forEach(func(el Elem) bool {
			realEnv = push(el, realEnv)
			return false
		})
		e.SetCar(realEnv)
	}

	vm.execute()

	result, _ := pop(AssertCell(s.Car()))
	assert.Equal(t, expected, result)
}

func TestLOAD(t *testing.T) {
	constants := []Elem{Int(1), Nil, True, Cons(Int(3), Nil), List(Int(1), Int(2), Int(3))}

	for _, constant := range constants {
		c := List(OpLOAD, constant)

		testSequence(t, c, constant, "LOAD constant")
	}
}

func TestLOOKUP(t *testing.T) {
	e := List(
		List(Cons(Symbol("foo"), Int(1)), Cons(Symbol("bar"), Int(2))),
	)

	c := List(OpLOAD, Symbol("foo"), OpLOOKUP)
	testSequenceWithEnv(t, c, e, Int(1), "LOOKUP symbol")

	c = List(OpLOAD, Symbol("bar"), OpLOOKUP)
	testSequenceWithEnv(t, c, e, Int(2), "LOOKUP symbol")

	c = List(OpLOAD, Symbol("baz"), OpLOOKUP)
	testSequenceWithEnv(t, c, e, Nil, "LOOKUP symbol")
}

func TestLOOKUPC(t *testing.T) {
	e := List(
		List(Cons(Symbol("foo"), Int(1)), Cons(Symbol("bar"), Int(2))),
		List(Cons(Symbol("baz"), Int(3)), Cons(Symbol("bar"), Int(4))),
	)

	c := List(OpLOAD, Symbol("foo"), OpLOOKUPC)
	testSequenceWithEnv(t, c, e, Cons(Symbol("foo"), Int(1)), "LOOKUPC symbol")

	c = List(OpLOAD, Symbol("bar"), OpLOOKUPC)
	testSequenceWithEnv(t, c, e, Cons(Symbol("bar"), Int(2)), "LOOKUPC symbol")

	c = List(OpLOAD, Symbol("baz"), OpLOOKUPC)
	testSequenceWithEnv(t, c, e, Cons(Symbol("baz"), Int(3)), "LOOKUPC symbol")
}

func TestSETCADR(t *testing.T) {
	// Set the value of bar to 8
	e := List(
		List(Cons(Symbol("baz"), Int(2))),
		List(Cons(Symbol("foo"), Int(1)), Cons(Symbol("bar"), Int(2))),
	)

	c := List(OpLOAD, Int(8), OpLOAD, Symbol("bar"), OpLOOKUPC, OpSETCDR, OpLOAD, Symbol("bar"), OpLOOKUP)
	testSequenceWithEnv(t, c, e, Int(8), "SETCDR")

	// Move the value for foo to bar
	e = List(
		List(Cons(Symbol("baz"), Int(2))),
		List(Cons(Symbol("foo"), Int(1))),
	)

	c = List(OpLOAD, Symbol("bar"), OpLOAD, Symbol("foo"), OpLOOKUPC, OpSETCAR, OpLOAD, Symbol("bar"), OpLOOKUP)
	testSequenceWithEnv(t, c, e, Int(1), "SETCAR")
}

func TestCONS(t *testing.T) {
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(8), OpCONS), Cons(Int(8), Int(2)), "cons")
	testSequence(t, List(OpLOAD, Nil, OpLOAD, Int(8), OpCONS), List(Int(8)), "cons list")
	testSequence(t, List(OpLOAD, Nil, OpLOAD, Int(8), OpCONS, OpLOAD, Int(2), OpCONS), List(Int(2), Int(8)), "cons list 2")
	testSequence(t, List(OpLOAD, Cons(Int(8), Int(2)), OpCAR), Int(8), "car")
	testSequence(t, List(OpLOAD, Cons(Int(8), Int(2)), OpCDR), Int(2), "cdr")
}

func TestEQ(t *testing.T) {
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(2), OpEQUAL), True, "eq 1")
	testSequence(t, List(OpLOAD, Int(1), OpLOAD, Int(2), OpEQUAL), Nil, "eq 2")
}

func TestSELECTJOIN(t *testing.T) {
	testSequence(t, List(OpLOAD, True, OpSELECT, List(OpLOAD, Int(1), OpJOIN), List(OpLOAD, Int(2), OpJOIN)), Int(1), "sel then")
	testSequence(t, List(OpLOAD, Nil, OpSELECT, List(OpLOAD, Int(1), OpJOIN), List(OpLOAD, Int(2), OpJOIN)), Int(2), "sel else")
}

func TestArithmetic(t *testing.T) {
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(2), OpADD), Int(4), "addition")
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(5), OpSUB), Int(3), "subtraction")
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(4), OpMUL), Int(8), "multiplication")
	testSequence(t, List(OpLOAD, Int(2), OpLOAD, Int(2), OpDIV), Int(1), "division")
	testSequence(t, List(OpLOAD, Int(6), OpLOAD, Int(8), OpMOD), Int(2), "modulo")
}

func TestLambda(t *testing.T) {
	argspec := List(Symbol("a"), Symbol("b"))
	body := List(OpLOAD, Symbol("a"), OpLOOKUP, OpLOAD, Symbol("b"), OpLOOKUP, OpLOAD, Symbol("c"), OpLOOKUP, OpADD, OpADD, OpRETURN)
	fn := List(LambdaSymbol, argspec, body)

	e := List(
		List(Cons(Symbol("c"), Int(1))),
	)
	c := List(OpLOAD, Nil, OpLOAD, Int(2), OpCONS, OpLOAD, Int(3), OpCONS, OpLOAD, fn, OpAPPLY)

	testSequenceWithEnv(t, c, e, Int(1+2+3), "lambda")
}

/*

func TestATOM(t *testing.T) {
	testSequence(t, List(OpLOAD, Int(2), OpATOM), True, "atom 1")
	testSequence(t, List(OpLOAD, Cons(Int(8), Nil), OpATOM), Nil, "atom 2")
}
*/
