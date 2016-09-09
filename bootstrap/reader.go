package bootstrap

import (
	"errors"
	"io"

	"github.com/tgascoigne/uclisp/vm"
)

var ErrUnexpectedRParen = errors.New("Unexpected ')'")
var ErrUnexpectedEOF = errors.New("Unexpected EOF")

type Reader struct {
	t *Tokenizer
}

func NewReader(reader io.Reader) *Reader {
	return &Reader{
		t: NewTokenizer(reader),
	}
}

func (r *Reader) wrapError(err error) error {
	if err == io.EOF {
		return ErrUnexpectedEOF
	}
	return err
}

func (r *Reader) ReadElem() (vm.Elem, error) {
	nextTok, err := r.t.Peek()
	if err != nil {
		return nil, r.wrapError(err)
	}

	switch nextTok.Type {
	case LParenTok:
		return r.readList()
	case RParenTok:
		return nil, ErrUnexpectedRParen
	case IntTok:
		return r.readInt()
	case SymTok:
		return r.readSymbol()
	default:
		panic("unknown token")
	}

	panic("unreachable")
	return nil, nil
}

func (r *Reader) expectTok() (Token, error) {
	tok, err := r.t.Next()
	if err != nil {
		return Token{}, r.wrapError(err)
	}

	return tok, nil
}

func (r *Reader) readList() (vm.Elem, error) {
	opening, err := r.expectTok()
	if err != nil {
		return nil, err
	}

	if opening.Type != LParenTok {
		panic("expecting lparen")
	}

	elems := make([]vm.Elem, 0)
	for {
		tok, err := r.t.Peek()
		if err != nil {
			return nil, r.wrapError(err)
		}

		// end of list
		if tok.Type == RParenTok {
			r.t.Next()
			break
		}

		elem, err := r.ReadElem()
		if err != nil {
			return nil, err
		}

		elems = append(elems, elem)
	}

	return vm.List(elems...), nil
}

func (r *Reader) readInt() (vm.Elem, error) {
	tok, err := r.expectTok()
	if err != nil {
		return nil, err
	}

	ival, err := tok.Int()
	if err != nil {
		return nil, err
	}

	return vm.Int(ival), nil
}

func (r *Reader) readSymbol() (vm.Elem, error) {
	tok, err := r.expectTok()
	if err != nil {
		return nil, err
	}

	sval := tok.String()

	return vm.Symbol(sval), nil
}