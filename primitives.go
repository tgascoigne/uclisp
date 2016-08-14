package uclisp

// Int is an integer Elem
type Int int

// Equal returns true if the argument is equal in value
func (i Int) Equal(other Elem) bool {
	if o, ok := other.(Int); ok {
		return i == o
	}

	return false
}

// AssertInt throws a type error if e is not a Int
func AssertInt(e Elem) Int {
	if c, ok := e.(Int); ok {
		return c
	}
	typeError("integerp", e)
	return Int(0)
}

// Symbol is an Elem which represents a lisp symbol
type Symbol string

// Equal returns true if the argument is equal in value
func (s Symbol) Equal(other Elem) bool {
	if o, ok := other.(Symbol); ok {
		return s == o
	}

	return false
}

// AssertSymbol throws a type error if e is not a Symbol
func AssertSymbol(e Elem) Symbol {
	if c, ok := e.(Symbol); ok {
		return c
	}
	typeError("symbolp", e)
	return Symbol("")
}
