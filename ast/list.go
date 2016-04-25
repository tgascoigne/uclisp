package ast

type List []Expression

type Prog List

func (p Prog) Eval() Value {
	var result Value
	for _, expr := range p {
		result = expr.Eval()
	}
	return result
}
