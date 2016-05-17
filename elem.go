package uclisp

// Elem is something which can be evaluated and compared
type Elem interface {
	Eval(*Context, Env) Elem
	Equals(*Context, Env, Elem) bool
}
