package uclisp

import (
	"bufio"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

func Read(input string) ([]Elem, error) {
	scanner := bufio.NewScanner(strings.NewReader(input))
	scanner.Split(lispSplitFunc)

	result := make([]Elem, 0)
	for scanner.Scan() {
		expr := readElement(scanner)
		if !expr.Equal(Nil) {
			result = append(result, expr)
		}
	}
	return result, nil
}

func readElement(scanner *bufio.Scanner) Elem {
	switch scanner.Text() {
	// TODO insert reader macros
	case "'":
		scanner.Scan()
		return List(QuoteSymbol, readElement(scanner))
	case "<":
		return readBytecode(scanner)
	case "(":
		return readSexpr(scanner)
	case "":
	default:
		return readConstant(scanner)
	}
	return Nil
}

func readBytecode(scanner *bufio.Scanner) Elem {
	elems := make([]Elem, 0)

	scanner.Scan()
	for {
		if scanner.Text() != ">" {
			elem := readElement(scanner)
			elems = append(elems, elem)
			scanner.Scan()
		} else {
			break
		}
	}

	return Bytecode(List(elems...))
}

func readSexpr(scanner *bufio.Scanner) Elem {
	elems := make([]Elem, 0)

	scanner.Scan()
	for {
		if scanner.Text() != ")" {
			elem := readElement(scanner)
			elems = append(elems, elem)
			scanner.Scan()
		} else {
			break
		}
	}

	return List(elems...)
}

func readConstant(scanner *bufio.Scanner) Elem {
	i, err := strconv.Atoi(scanner.Text())
	if err != nil {
		return Symbol(scanner.Text())
	}
	return Int(i)
}

func isPunctuation(r rune) bool {
	return strings.ContainsRune("<>()'", r)
}

func isSymbolCharacter(r rune) bool {
	return !isWhitespace(r) && !isPunctuation(r)
}

func isWhitespace(r rune) bool {
	return unicode.IsSpace(r) || strings.ContainsRune("\n\r", r)
}

func lispSplitFunc(data []byte, atEOF bool) (advance int, token []byte, err error) {
	peekChar := func() (rune, int) {
		if advance > len(data) {
			return utf8.RuneError, 0
		}
		return utf8.DecodeRune(data[advance:])
	}

	nextChar := func() rune {
		r, bytes := peekChar()
		advance += bytes
		return r
	}

	// swallow whitespace
	for {
		r, i := peekChar()
		if isWhitespace(r) {
			advance += i
		} else {
			break
		}
	}

	if len(data[advance:]) == 0 {
		if atEOF {
			return advance, nil, bufio.ErrFinalToken
		} else {
			// Nothing left, but not at eof yet...
			return 0, nil, nil
		}
	}

	// If the next char is punctuation, return it
	if r, _ := peekChar(); isPunctuation(r) {
		r = nextChar()
		token := make([]byte, 8)
		i := utf8.EncodeRune(token, r)
		return advance, token[:i], nil
	}

	// Otherwise, accept a run of runes
	start := advance
	for {
		r, i := peekChar()
		if r != utf8.RuneError && isSymbolCharacter(r) {
			advance += i
		} else {
			break
		}
	}

	return advance, data[start:advance], nil
}
