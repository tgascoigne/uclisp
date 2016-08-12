package uclisp

// Elem is any value which can be passed to and from lisp
type Elem interface {
	Equal(Elem) bool
}

// cellInternal is the internal representation of a cell
type cellInternal struct {
	car, cdr Elem
}

// Nil is the empty list
var Nil Cell = Cell{nil}

// Cell is a pointer to a lisp cell
type Cell struct {
	*cellInternal
}

// Equal returns true if the argument is equal in value
func (c Cell) Equal(other Elem) bool {
	if o, ok := other.(Cell); ok {
		if c == Nil || o == Nil {
			return c == o
		}

		if !c.car.Equal(o.car) {
			return false
		}

		// If this cell is equal, then continue to compare cdrs
		return c.cdr.Equal(o.cdr)
	}
	return false
}

// Expand returns both car and cdr
func (c Cell) Expand() (car, cdr Elem) {
	return c.car, c.cdr
}

// SetCar sets the car
func (c Cell) SetCar(car Elem) {
	c.car = car
}

// SetCdr sets the cdr
func (c Cell) SetCdr(cdr Elem) {
	c.cdr = cdr
}

// Car returns car
func (c Cell) Car() Elem {
	return c.car
}

// Cdr returns cdr
func (c Cell) Cdr() Elem {
	return c.cdr
}

// Cons creates a new cell
func Cons(car, cdr Elem) Cell {
	return Cell{&cellInternal{
		car: car,
		cdr: cdr,
	}}
}

// List creates a new list
func List(elems ...Elem) Cell {
	list := Nil
	for i := len(elems) - 1; i > 0; i-- {
		list = Cons(elems[i], list)
	}
	return list
}
