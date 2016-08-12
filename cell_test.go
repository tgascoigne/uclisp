package uclisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCellExpand(t *testing.T) {
	cell := Cons(Int(1), Int(2))
	car, cdr := cell.Expand()
	assert.EqualValues(t, []Elem{Int(1), Int(2)}, []Elem{car, cdr}, "car and cdr not expanded correctly")
}

func TestCellEquality(t *testing.T) {
	a := Cons(Int(1), Nil)
	b := Cons(Int(1), Nil)
	assert.True(t, a.Equal(b), "Values should be equal")

	a = Cons(Int(1), Nil)
	b = Cons(Int(2), Nil)
	assert.False(t, a.Equal(b), "Values should not be equal")

	a = List(Int(1), Int(2), Int(3))
	b = List(Int(1), Int(2), Int(3))
	assert.True(t, a.Equal(b), "Values should be equal")

	a = List(Int(1), Int(2), Int(3))
	b = List(Int(1), Int(2))
	assert.False(t, a.Equal(b), "Values should not be equal")

	a = List(Int(1), Int(2), Int(3))
	b = List(Int(1), Int(1), Int(3))
	assert.False(t, a.Equal(b), "Values should not be equal")
}

func TestCellCopy(t *testing.T) {
	a := Cons(Int(1), Nil)
	assert.Equal(t, Int(1), a.Car(), "Cell constructed incorrectly")
	b := a
	assert.Equal(t, Int(1), b.Car(), "Cell copied incorrectly")
	a.SetCar(Int(2))
	assert.Equal(t, Int(2), a.Car(), "Cell copied incorrectly")
	assert.Equal(t, Int(2), b.Car(), "Cell copied incorrectly")
}
