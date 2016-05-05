package ast_test

import (
	"fmt"

	"github.com/tgascoigne/uclisp/ast"
	"github.com/tgascoigne/uclisp/parser"
)

type Foo struct {
	X, Y int
}

type Bar struct {
	F Foo
	Z int
}

func (b *Bar) Sum(q int) int {
	return b.F.X + b.F.Y + b.Z + q
}

func Example() {
	var bar Bar
	ast.Global.Define(ast.Symbol("bar"), ast.Bind(&bar))
	prog := parser.Parse("example.lsp", `
(with bar
  (with F
    (set X 2)
    (set Y 4))
  (set Z 8)
  (Sum 20)
  )
`)
	result := prog.Eval(ast.Global)
	fmt.Println(result)
	// Output:
	// 34
}
