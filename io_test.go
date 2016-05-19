package uclisp_test

import (
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/tgascoigne/uclisp"
)

var ioTests = BasicTests{
	{"(let ((foo \"bar\")) (message foo))", uclisp.String("bar")},
	{"(let ((foo \"bar %v\") (bar 2)) (message foo bar))", uclisp.String("bar 2")},
	{"(file-exists-p \"doesnt-exist\")", uclisp.Nil},
}

var ioExceptionTests = ExceptionTests{
	{"(message)", uclisp.ErrArgCount},
	{"(message 2)", uclisp.ErrNotAString},
	{"(file-exists-p 2)", uclisp.ErrNotAString},
	{"(file-exists-p)", uclisp.ErrArgCount},
	{"(file-exists-p \"a\" \"b\")", uclisp.ErrArgCount},
	{"(load-file 2)", uclisp.ErrNotAString},
	{"(load-file)", uclisp.ErrArgCount},
	{"(load-file \"a\" \"b\")", uclisp.ErrArgCount},
	{"(read-file 2)", uclisp.ErrNotAString},
	{"(read-file)", uclisp.ErrArgCount},
	{"(read-file \"a\" \"b\")", uclisp.ErrArgCount},
	{"(read-file \"doesnt-exist\")", uclisp.ErrIO},
}

func TestIo(t *testing.T) {
	ioTests.Do(t)
	ioExceptionTests.Do(t)
}

func writeTempFile(contents string) string {
	file, err := ioutil.TempFile("", "uclisp")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	fmt.Fprint(file, contents)

	return file.Name()
}

func TestFileExists(t *testing.T) {
	path := writeTempFile("foo")
	BasicTest{fmt.Sprintf("(file-exists-p \"%v\")", path), uclisp.True}.Do(t)
	BasicTest{fmt.Sprintf("(let ((path \"%v\")) (file-exists-p path))", path), uclisp.True}.Do(t)
}

func TestFileReadLoad(t *testing.T) {
	path := writeTempFile("(+ 1 1)")
	BasicTest{fmt.Sprintf("(read-file \"%v\")", path), uclisp.List{uclisp.Symbol("progn"), uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(1)}}}.Do(t)
	BasicTest{fmt.Sprintf("(load-file \"%v\")", path), uclisp.Integer(2)}.Do(t)
}
