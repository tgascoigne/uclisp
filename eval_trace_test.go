package uclisp

import "testing"

func TestTraceStack(t *testing.T) {
	ctx := NewContext()

	trace := &Trace{make([]Elem, 0)}
	trace.Push(Integer(1))
	if trace.Size() != 1 {
		t.Errorf("Unexpected trace size")
	}

	trace.Push(Integer(2))
	if trace.Size() != 2 {
		t.Errorf("Unexpected trace size")
	}

	if !Equal(ctx, Global, trace.Pop(), Integer(2)) {
		t.Errorf("Unexpected element popped from trace")
	}

	if trace.Size() != 1 {
		t.Errorf("Unexpected trace size")
	}

	trace.Push(Integer(3))
	if trace.Size() != 2 {
		t.Errorf("Unexpected trace size")
	}

	if !Equal(ctx, Global, trace.Pop(), Integer(3)) {
		t.Errorf("Unexpected element popped from trace")
	}

	if trace.Size() != 1 {
		t.Errorf("Unexpected trace size")
	}

	if !Equal(ctx, Global, trace.Pop(), Integer(1)) {
		t.Errorf("Unexpected element popped from trace")
	}

	if trace.Size() != 0 {
		t.Errorf("Unexpected trace size")
	}

	if !Equal(ctx, Global, trace.Pop(), Nil) {
		t.Errorf("Unexpected element popped from trace")
	}

	if trace.Size() != 0 {
		t.Errorf("Unexpected trace size")
	}
}
