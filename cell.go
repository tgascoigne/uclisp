package uclisp

import (
	"fmt"
	"strings"
)

// Elem is any value which can be passed to and from lisp
type Elem interface {
	Equal(Elem) bool
	Type() Symbol
}

// cellInternal is the internal representation of a cell
type cellInternal struct {
	car, cdr Elem
}

// Nil is the empty list
var Nil = Cell{nil}

// True is a unique truth value
var True = Cell{new(cellInternal)}

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

func (c Cell) Type() Symbol {
	if c.Equal(Nil) {
		return Symbol("nil")
	}
	return Symbol("cons")
}

func (c Cell) String() string {
	var formatCell func(el Elem, depth int) string
	formatCell = func(el Elem, depth int) string {
		if c, ok := el.(Cell); ok {

			if c.Equal(Nil) {
				return "()"
			}

			if depth > 4 {
				return "..."
			}

			if _, ok := c.cdr.(Cell); !ok {
				// cdr is not another cell, so it's a plain old cons
				return fmt.Sprintf("(%v . %v)", formatCell(c.car, depth+1), formatCell(c.cdr, depth+1))
			}

			strs := make([]string, 0)

			c.forEach(func(el Elem) bool {
				strs = append(strs, formatCell(el, depth+1))
				return false
			})

			return fmt.Sprintf("(%v)", strings.Join(strs, " "))
		} else {
			return fmt.Sprintf("%v", el)
		}
	}

	return formatCell(c, 0)
}

// Expand returns both car and cdr
func (c Cell) Expand() (car, cdr Elem) {
	return c.car, c.cdr
}

// ExpandList flattens and returns all elements of a list
func (c Cell) ExpandList() []Elem {
	list := make([]Elem, 0)
	c.forEach(func(el Elem) bool {
		list = append(list, el)
		return false
	})
	return list
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
	if c.Equal(Nil) {
		return Nil
	}
	return c.car
}

// Cdr returns cdr
func (c Cell) Cdr() Elem {
	if c.Equal(Nil) {
		return Nil
	}
	return c.cdr
}

// AssertCell throws a type error if e is not a Cell
func AssertCell(e Elem) Cell {
	if c, ok := e.(Cell); ok {
		return c
	}
	typeError("consp", e)
	return Nil
}

// Cons creates a new cell
func Cons(car, cdr Elem) Cell {
	return Cell{&cellInternal{
		car: car,
		cdr: cdr,
	}}
}

func Nilp(c Elem) bool {
	return c.Equal(Nil)
}

// Bool converts a boolean to a Cell
func Bool(b bool) Cell {
	if b {
		return True
	}
	return Nil
}

// List creates a new list
func List(elems ...Elem) Cell {
	list := Nil
	for i := len(elems) - 1; i >= 0; i-- {
		list = Cons(elems[i], list)
	}
	return list
}

type forFunc func(Elem) bool

func (c Cell) forEach(fn forFunc) bool {
	iter := c
	for {
		if fn(iter.Car()) {
			return true
		}
		if iter.Cdr().Equal(Nil) {
			break
		}
		iter = AssertCell(iter.Cdr())
	}
	return false
}

func (c Cell) reverse() Cell {
	result := Nil
	c.forEach(func(e Elem) bool {
		result = Cons(e, result)
		return false
	})
	return result
}
