package bootstrap

import (
	"io"
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
				Token{Value: "(", Type: LParenTok, start: 0, end: 1},
				Token{Value: "foo", Type: SymTok, start: 1, end: 4},
				Token{Value: "1", Type: IntTok, start: 5, end: 6},
				Token{Value: "2", Type: IntTok, start: 7, end: 8},
				Token{Value: ")", Type: RParenTok, start: 8, end: 9},
			},
		},
		{
			Name:  "Tok2",
			Input: "(foo (12 x)'y)",
			Expected: []Token{
				Token{Value: "(", Type: LParenTok, start: 0, end: 1},
				Token{Value: "foo", Type: SymTok, start: 1, end: 4},
				Token{Value: "(", Type: LParenTok, start: 5, end: 6},
				Token{Value: "12", Type: IntTok, start: 6, end: 8},
				Token{Value: "x", Type: SymTok, start: 9, end: 10},
				Token{Value: ")", Type: RParenTok, start: 10, end: 11},
				Token{Value: "'", Type: SymTok, start: 11, end: 12},
				Token{Value: "y", Type: SymTok, start: 12, end: 13},
				Token{Value: ")", Type: RParenTok, start: 13, end: 14},
			},
		},
	}

	for _, tc := range tests {
		tc := tc // capture range variable
		t.Run(tc.Name, func(t *testing.T) {
			tokenizer := NewTokenizer("<test>", strings.NewReader(tc.Input))
			tokens, err := tokenizer.ReadAll()
			if err != nil {
				t.Errorf("Error tokenizing: %v", err)
			}

			assert.Equal(t, tc.Expected, tokens)

			_, err = tokenizer.Next()
			assert.Equal(t, io.EOF, err, "expected EOF")
		})
	}
}

func TestTokenizerPeekUnread(t *testing.T) {
	var tok Token
	var err error
	tokenizer := NewTokenizer("<test>", strings.NewReader("(foo 1 2)"))

	tokens := []Token{
		Token{Value: "(", Type: LParenTok, start: 0, end: 1},
		Token{Value: "foo", Type: SymTok, start: 1, end: 4},
		Token{Value: "1", Type: IntTok, start: 5, end: 6},
		Token{Value: "2", Type: IntTok, start: 7, end: 8},
		Token{Value: ")", Type: RParenTok, start: 8, end: 10},
	}

	tok, err = tokenizer.Next()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[0], tok, "unexpected token")

	tok, err = tokenizer.Peek()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[1], tok, "unexpected token")

	tok, err = tokenizer.Peek()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[1], tok, "unexpected token")

	tokenizer.Prev()

	tok, err = tokenizer.Peek()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[0], tok, "unexpected token")

	tok, err = tokenizer.Next()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[0], tok, "unexpected token")

	tok, err = tokenizer.Next()
	assert.NoError(t, err, "error getting token")
	assert.Equal(t, tokens[1], tok, "unexpected token")
}

func TestTokenizerLineOffset(t *testing.T) {
	tokenizer := NewTokenizer("<test>", strings.NewReader(`(foo 1 2)
(bar baz 4 5)
(baz)`))

	expected := []struct {
		Token
		line, start, end int
	}{
		{
			Token{Value: "(", Type: LParenTok, start: 0, end: 1},
			1, 0, 1,
		},
		{
			Token{Value: "foo", Type: SymTok, start: 1, end: 4},
			1, 1, 4,
		},
		{
			Token{Value: "1", Type: IntTok, start: 5, end: 6},
			1, 5, 6,
		},
		{
			Token{Value: "2", Type: IntTok, start: 7, end: 8},
			1, 7, 8,
		},
		{
			Token{Value: ")", Type: RParenTok, start: 8, end: 9},
			1, 8, 9,
		},
		{
			Token{Value: "(", Type: LParenTok, start: 10, end: 11},
			2, 0, 1,
		},
		{
			Token{Value: "bar", Type: SymTok, start: 11, end: 14},
			2, 1, 4,
		},
		{
			Token{Value: "baz", Type: SymTok, start: 15, end: 18},
			2, 5, 8,
		},
		{
			Token{Value: "4", Type: IntTok, start: 19, end: 20},
			2, 9, 10,
		},
		{
			Token{Value: "5", Type: IntTok, start: 21, end: 22},
			2, 11, 12,
		},
		{
			Token{Value: ")", Type: RParenTok, start: 22, end: 23},
			2, 12, 13,
		},
		{
			Token{Value: "(", Type: LParenTok, start: 24, end: 25},
			3, 0, 1,
		},
		{
			Token{Value: "baz", Type: SymTok, start: 25, end: 28},
			3, 1, 4,
		},
		{
			Token{Value: ")", Type: RParenTok, start: 28, end: 29},
			3, 4, 5,
		},
	}

	tokens, err := tokenizer.ReadAll()
	if err != nil {
		t.Errorf("Error tokenizing: %v", err)
	}

	for i, expTok := range expected {
		tok := tokens[i]
		assert.Equal(t, expTok.Token, tok)

		line, start, end := tokenizer.TokenPosition(tok)
		assert.Equal(t, expTok.line, line, "line mismatch at token %v", expTok)
		assert.Equal(t, expTok.start, start, "line start offset mismatch at token %v", expTok)
		assert.Equal(t, expTok.end, end, "line end offset mismatch at token %v", expTok)
	}

}
