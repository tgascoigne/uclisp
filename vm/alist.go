package vm

import "fmt"

func assoc(e Elem, c Cell) Cell {
	result := Nil
	c.ForEach(func(el Elem) bool {
		c := AssertCell(el)
		if c.Car().Equal(e) {
			result = c
			return true
		}
		return false
	})
	return result
}

func mapToAlist(m map[Elem]Elem) Cell {
	list := Nil
	for k, v := range m {
		list = push(Cons(k, v), list)
	}
	return list
}

func pairlis(a, b Cell) Cell {
	list := Nil
	for {
		if (a.Equal(Nil) && !b.Equal(Nil)) || (b.Equal(Nil) && !a.Equal(Nil)) {
			Raise(fmt.Errorf("pairlis: mismatch in list count, %v, %v", a, b))
		}

		if a.Equal(Nil) {
			break
		}

		list = push(Cons(a.Car(), b.Car()), list)

		a = AssertCell(a.Cdr())
		b = AssertCell(b.Cdr())
	}
	return list.Reverse()
}

const (
	restSymbol = Symbol("&rest")
)

func pairargs(a, b Cell) Cell {
	list := Nil
	for {
		if a.Equal(Nil) && !b.Equal(Nil) {
			Raise(fmt.Errorf("pairargs: mismatch in list count, %v, %v", a, b))
		}

		if a.Car().Equal(restSymbol) {
			restBinding := Cadr(a)
			list = push(Cons(restBinding, b), list)
			break
		}

		if b.Equal(Nil) && !a.Equal(Nil) {
			Raise(fmt.Errorf("pairargs: mismatch in list count, %v, %v", a, b))
		}

		if a.Equal(Nil) {
			break
		}

		list = push(Cons(a.Car(), b.Car()), list)

		a = AssertCell(a.Cdr())
		b = AssertCell(b.Cdr())
	}
	return list.Reverse()
}
