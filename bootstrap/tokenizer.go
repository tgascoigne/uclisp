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
	Value      string
	Type       TokenType
	start, end int
}

func (t Token) String() string {
	return t.Value
}

func (t Token) Int() (int, error) {
	return strconv.Atoi(t.Value)
}

type Tokenizer struct {
	reader   *bufio.Reader
	tokens   []Token
	filename string
	// lineMap maps character offsets to line numbers
	lineMap     []int
	tokenOffset int
	charOffset  int
}

func NewTokenizer(filename string, reader io.Reader) *Tokenizer {
	return &Tokenizer{
		reader:   bufio.NewReader(reader),
		tokens:   make([]Token, 0),
		filename: filename,
		lineMap:  []int{0},
	}
}

func (t *Tokenizer) TokenPosition(tok Token) (line, start, end int) {
	for l, offset := range t.lineMap {
		if offset <= tok.start {
			line = l
		}
	}

	lineOffset := t.lineMap[line]
	return line + 1, tok.start - lineOffset, tok.end - lineOffset
}

func (t *Tokenizer) readRune() (rune, error) {
	chr, _, err := t.reader.ReadRune()
	t.charOffset++

	if chr == '\n' {
		t.lineMap = append(t.lineMap, t.charOffset)
	}

	return chr, err
}

func (t *Tokenizer) unreadRune() error {
	err := t.reader.UnreadRune()
	if err != nil {
		return err
	}

	t.charOffset--
	return nil
}

func (t *Tokenizer) pushTok(s string, start, end int) {
	tok := Token{
		Value: s,
		start: start,
		end:   end,
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
	var currentTok string
	var tokStart int

	for {
		chr, err := t.readRune()

		if err != nil {
			if err == io.EOF {
				if currentTok == "" {
					return io.EOF
				}

				// EOF, but we have a token
				t.pushTok(currentTok, tokStart, t.charOffset)
				return nil
			}
		}

		if isSpecial(chr) {
			if currentTok == "" {
				// First and only char is a paren
				tokStart = t.charOffset - 1
				t.pushTok(string(chr), tokStart, t.charOffset)
			} else {
				// Found a paren in the middle of a symbol,
				// ignore it and return the symbol
				err := t.unreadRune()
				if err != nil {
					// Shouldn't happen, because we definitely read a rune in this loop
					panic(err)
				}

				t.pushTok(currentTok, tokStart, t.charOffset)
			}
			return nil
		}

		if unicode.IsSpace(chr) || isCommentChar(chr) {
			if isCommentChar(chr) {
				// Comment; consume until EOL
				for {
					next, err := t.readRune()
					if next == '\n' || err != nil {
						break
					}
				}
			}

			if currentTok == "" {
				// We haven't found a token yet, just whitespace. Consume it...
				continue
			} else {
				// Whitespace in the middle of a token, return the token
				err := t.unreadRune()
				if err != nil {
					// Shouldn't happen, because we definitely read a rune in this loop
					panic(err)
				}

				t.pushTok(currentTok, tokStart, t.charOffset)
				return nil
			}
		}

		if currentTok == "" {
			// first character of a token
			tokStart = t.charOffset - 1
		}
		currentTok = currentTok + string(chr)
	}

	panic("not reachable")
}

func (t *Tokenizer) ensureNext() (err error) {
	if t.tokenOffset > len(t.tokens)-1 {
		return t.fill()
	}
	return nil
}

func (t *Tokenizer) Peek() (Token, error) {
	err := t.ensureNext()
	if err != nil {
		return Token{}, err
	}

	return t.tokens[t.tokenOffset], nil
}

func (t *Tokenizer) Next() (Token, error) {
	err := t.ensureNext()
	if err != nil {
		return Token{}, err
	}

	tok := t.tokens[t.tokenOffset]
	t.tokenOffset++

	return tok, nil
}

func (t *Tokenizer) Prev() error {
	if t.tokenOffset == 0 {
		return errors.New("at the start of input")
	}

	t.tokenOffset--
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

func isCommentChar(r rune) bool {
	if strings.ContainsAny(string(r), ";") {
		return true
	}
	return false
}

func isSpecial(r rune) bool {
	if strings.ContainsAny(string(r), "()'`,@") {
		return true
	}
	return false
}
