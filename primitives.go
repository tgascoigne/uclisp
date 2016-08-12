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

// Symbol is an Elem which represents a lisp symbol
type Symbol string

// Equal returns true if the argument is equal in value
func (s Symbol) Equal(other Elem) bool {
	if o, ok := other.(Symbol); ok {
		return s == o
	}

	return false
}
