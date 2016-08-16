package uclisp

// Op is an instruction's opcode
type Op int

//go:generate stringer -type=Op

const (
	OpNOP Op = iota
	OpLOAD
	OpLOOKUP
	OpLOOKUPC
	OpCONS
	OpCAR
	OpCDR
	OpSETCAR
	OpSETCDR
	OpAPPLY
	OpRETURN
	OpSELECT
	OpJOIN
	OpEQUAL
	OpADD
	OpSUB
	OpMUL
	OpDIV
	OpMOD

	OpEND // marker, invalid opcode
)

// Equal returns true if the argument is equal in value
func (i Op) Equal(other Elem) bool {
	if o, ok := other.(Op); ok {
		return i == o
	}

	return false
}

// AssertOp throws a type error if e is not a Op
func AssertOp(e Elem) Op {
	if c, ok := e.(Op); ok {
		return c
	}
	typeError("opcodep", e)
	return OpNOP
}

// Bytecode is the compiled executable code
type Bytecode Cell

// Equal returns true if the argument is equal in value
func (b Bytecode) Equal(other Elem) bool {
	if o, ok := other.(Bytecode); ok {
		return Cell(b).Equal(Cell(o))
	}

	return false
}
