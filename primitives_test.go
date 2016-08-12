package uclisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIntEquality(t *testing.T) {
	assert.True(t, Int(1).Equal(Int(1)), "Values should be equal")
	assert.False(t, Int(1).Equal(Int(2)), "Values should not be equal")
}

func TestSymbolEquality(t *testing.T) {
	assert.True(t, Symbol("foo").Equal(Symbol("foo")), "Values should be equal")
	assert.False(t, Symbol("foo").Equal(Symbol("bar")), "Values should not be equal")
}
