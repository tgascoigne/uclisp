package parser

import (
	"encoding/json"
	"fmt"
	"lisp/ast"
	"testing"
)

func dump(prog ast.Prog) string {
	astStr, _ := json.Marshal(prog)
	return fmt.Sprintf("%v", string(astStr))
}

func TestAdd1(t *testing.T) {
	prog := Parse("test", "(+ 2 (+ 3 4 5))")
	expr := prog[0]

	t.Logf(dump(prog))

	result := expr.Eval()
	intResult := result.(ast.Integer)
	if int(intResult) != 14 {
		t.Errorf("Value incorrect: got %v expected 14", intResult)
	}
}
