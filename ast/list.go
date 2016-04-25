package ast

type List []Form

type Prog List

func (p Prog) Eval() Value {
	var result Value
	for _, expr := range p {
		result = expr.Eval()
	}
	return result
}
