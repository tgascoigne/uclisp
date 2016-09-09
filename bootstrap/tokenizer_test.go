package bootstrap

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTokenizer(t *testing.T) {
	tests := []struct {
		Name     string
		Input    string
		Expected []Token
	}{
		{
			Name:  "Tok1",
			Input: "(foo 1 2)",
			Expected: []Token{
				Token{"(", LParenTok},
				Token{"foo", SymTok},
				Token{"1", IntTok},
				Token{"2", IntTok},
				Token{")", RParenTok},
			},
		},
		{
			Name:  "Tok2",
			Input: "(foo (12 x)'y)",
			Expected: []Token{
				Token{"(", LParenTok},
				Token{"foo", SymTok},
				Token{"(", LParenTok},
				Token{"12", IntTok},
				Token{"x", SymTok},
				Token{")", RParenTok},
				Token{"'", SymTok},
				Token{"y", SymTok},
				Token{")", RParenTok},
			},
		},
	}

	for _, tc := range tests {
		tc := tc // capture range variable
		t.Run(tc.Name, func(t *testing.T) {
			tokenizer := NewTokenizer(strings.NewReader(tc.Input))
			tokens, err := tokenizer.ReadAll()
			if err != nil {
				t.Errorf("Error tokenizing: %v", err)
			}

			assert.Equal(t, tc.Expected, tokens)
		})
	}
}
