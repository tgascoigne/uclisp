package uclisp

// Elem is something which can be evaluated
type Elem interface {
	Eval(Env) Elem
}
