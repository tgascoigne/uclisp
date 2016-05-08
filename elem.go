package uclisp

// Elem is something which can be evaluated and compared
type Elem interface {
	Eval(Env) Elem
	Equals(Env, Elem) bool
}
