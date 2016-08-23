package uclisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompile1(t *testing.T) {
	// (+ 1 2)
	expr := List(Symbol("+"), Int(1), Int(2))
	result := compile(expr)
	t.Logf("compile %#v -> %#v", expr, result)
	assert.Equal(t, []Elem{OpLOAD, Nil, OpLOAD, Int(2), OpCONS, OpLOAD, Int(1), OpCONS, OpLOAD, Symbol("+"), OpLOOKUP, OpAPPLY}, result)
}

func TestCompile2(t *testing.T) {
	// (+ 1 (+ 2 3))
	expr := List(Symbol("+"), Int(1), List(Symbol("+"), Int(2), Int(3)))
	result := compile(expr)
	t.Logf("compile %#v -> %#v", expr, result)
	assert.Equal(t, []Elem{OpLOAD, Nil, OpLOAD, Nil, OpLOAD, Int(3), OpCONS, OpLOAD, Int(2), OpCONS, OpLOAD, Symbol("+"), OpLOOKUP, OpAPPLY, OpCONS, OpLOAD, Int(1), OpCONS, OpLOAD, Symbol("+"), OpLOOKUP, OpAPPLY}, result)
}

func TestCompile3(t *testing.T) {
	// (cons 'a 1)
	expr := List(Symbol("#cons"), Symbol("a"), Int(1))
	result := compile(expr)
	t.Logf("compile %#v -> %#v", expr, result)
	assert.Equal(t, []Elem{OpLOAD, Nil, OpLOAD, Int(1), OpCONS, OpLOAD, Symbol("a"), OpLOOKUP, OpCONS, OpLOAD, Symbol("#cons"), OpLOOKUP, OpAPPLY}, result)
}
