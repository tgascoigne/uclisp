package vm

import "fmt"

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
	OpTYPE
	OpAPPLY
	OpRETURN
	OpEVAL
	OpDROP
	OpDUP
	OpSELECT
	OpJOIN
	OpEQUAL
	OpADD
	OpSUB
	OpMUL
	OpDIV
	OpMOD
	OpREAD
	OpPRINT

	OpEND // marker, invalid opcode
)

// Equal returns true if the argument is equal in value
func (i Op) Equal(other Elem) bool {
	if o, ok := other.(Op); ok {
		return i == o
	}

	return false
}

func (i Op) Type() Symbol {
	return Symbol("op")
}

func ParseOp(sym Symbol) (result Op, ok bool) {
	if sym[0] == '$' {
		sym = sym[1:]
	}

	if op, ok := opCodeMap[string(sym)]; ok {
		return op, true
	}
	return OpNOP, false
}

// AssertOp throws a type error if e is not a Op
func AssertOp(e Elem) Op {
	if c, ok := e.(Op); ok {
		return c
	}
	typeError("opcodep", e)
	return OpNOP
}

var opCodeMap = map[string]Op{}

func init() {
	for i := OpNOP; i < OpEND; i++ {
		opStr := fmt.Sprintf("%v", i)
		opStr = opStr[2:] // trim off 'Op' prefix
		opCodeMap[opStr] = i
	}
}
