package bootstrap

import (
	"bufio"
	"errors"
	"io"
	"strconv"
	"strings"
	"unicode"
)

type TokenType int

const (
	LParenTok TokenType = iota
	RParenTok
	IntTok
	SymTok
)

type Token struct {
	Value string
	Type  TokenType
}

func (t Token) String() string {
	return t.Value
}

func (t Token) Int() (int, error) {
	return strconv.Atoi(t.Value)
}

type Tokenizer struct {
	reader *bufio.Reader
	tokens []Token
	caret  int
}

func NewTokenizer(reader io.Reader) *Tokenizer {
	return &Tokenizer{
		reader: bufio.NewReader(reader),
		tokens: make([]Token, 0),
	}
}

func isSpecial(r rune) bool {
	if strings.ContainsAny(string(r), "()'`,@") {
		return true
	}
	return false
}

func (t *Tokenizer) pushTok(s string) {
	tok := Token{
		Value: s,
	}

	_, atoiErr := strconv.Atoi(s)

	switch {
	case s == "(":
		tok.Type = LParenTok
	case s == ")":
		tok.Type = RParenTok
	case atoiErr == nil:
		tok.Type = IntTok
	default:
		tok.Type = SymTok
	}

	t.tokens = append(t.tokens, tok)
}

func (t *Tokenizer) fill() (err error) {
	r := t.reader

	var currentTok string
	for {
		chr, _, err := r.ReadRune()

		if err != nil {
			if err == io.EOF {
				if currentTok == "" {
					return io.EOF
				}

				// EOF, but we have a token
				t.pushTok(currentTok)
				return nil
			}
		}

		if isSpecial(chr) {
			if currentTok == "" {
				// First char and only is a paren
				t.pushTok(string(chr))
			} else {
				// Found a paren in the middle of a symbol,
				// ignore it and return the symbol
				r.UnreadRune()
				t.pushTok(currentTok)
			}
			return nil
		}

		if unicode.IsSpace(chr) {
			if currentTok == "" {
				// We haven't found a token yet, just whitespace. Consume it...
				continue
			} else {
				// Whitespace in the middle of a token, return the token
				t.pushTok(currentTok)
				return nil
			}
		}

		currentTok = currentTok + string(chr)
	}

	panic("not reachable")
}

func (t *Tokenizer) ensureNext() (err error) {
	if t.caret > len(t.tokens)-1 {
		return t.fill()
	}
	return nil
}

func (t *Tokenizer) Peek() (Token, error) {
	err := t.ensureNext()
	if err != nil {
		return Token{}, err
	}

	return t.tokens[t.caret], nil
}

func (t *Tokenizer) Next() (Token, error) {
	err := t.ensureNext()
	if err != nil {
		return Token{}, err
	}

	tok := t.tokens[t.caret]
	t.caret++

	return tok, nil
}

func (t *Tokenizer) Prev() error {
	if t.caret == 0 {
		return errors.New("at the start of input")
	}

	t.caret--
	return nil
}

func (t *Tokenizer) ReadAll() ([]Token, error) {
	tokens := make([]Token, 0)

	for {
		tok, err := t.Next()
		if err != nil {
			if err == io.EOF {
				break
			}

			return tokens, err
		}

		tokens = append(tokens, tok)
	}

	return tokens, nil
}
