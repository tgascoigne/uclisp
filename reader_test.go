package uclisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func doBytecodeReadTest(t *testing.T, instr string, expected Cell) {
	bc, err := Read(instr)
	assert.NoError(t, err, "Bytecode read failed")
	assert.Equal(t, []Elem{
		expected,
	}, bc, "Bytecode read incorrect")
}

func TestBytecodeRead(t *testing.T) {
	doBytecodeReadTest(t, "<LOAD 100>", List(OpLOAD, Int(100)))
	doBytecodeReadTest(t, `<LOAD 200
LOAD 1
SELECT
  <LOAD 100>
  <LOAD 200>
ADD>`,
		List(
			OpLOAD, Int(200),
			OpLOAD, Int(1),
			OpSELECT,
			List(OpLOAD, Int(100)),
			List(OpLOAD, Int(200)),
			OpADD))

	doBytecodeReadTest(t, "<LOAD <LOAD foo>>", List(OpLOAD, List(OpLOAD, Symbol("foo"))))

	doBytecodeReadTest(t, "<LOAD (foo bar baz)>", List(OpLOAD, List(Symbol("foo"), Symbol("bar"), Symbol("baz"))))

}
