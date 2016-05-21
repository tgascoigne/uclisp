package uclisp

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

const (
	SymLoadFileName = Symbol("*load-file-path*")
)

func init() {
	Builtin.Define("file-exists-p", NewProcedure(fileexistsForm))
	Builtin.Define("load-file", NewProcedure(loadfileForm))
	Builtin.Define("read-file", NewProcedure(readfileForm))
	Builtin.Define("message", NewProcedure(messageForm))
}

func messageForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	formatEl := ctx.Eval(args[0], env)
	format, err := AssertString(formatEl)
	if err != nil {
		ctx.Raise(err, formatEl)
	}

	args = args[1:]
	argsIface := make([]interface{}, len(args))
	for i := range args {
		// arguments are evaluated in the caller's environment
		argsIface[i] = ctx.Eval(args[i], env)
	}

	out := fmt.Sprintf(string(format), argsIface...)
	log.Println(out)
	return String(out)
}

func fileexistsForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	pathElem := ctx.Eval(args[0], env)
	path, err := AssertString(pathElem)
	if err != nil {
		ctx.Raise(err, pathElem)
	}

	if _, err := os.Stat(string(path)); os.IsNotExist(err) {
		return Nil
	}

	return True
}

func loadfileForm(ctx *Context, parentEnv Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	env := NewBasicEnv(parentEnv)
	prog := readfileForm(ctx, env, args)
	return ctx.Eval(prog, env)
}

var ErrIO = errors.New("io error: %v")

func readfileForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	pathElem := ctx.Eval(args[0], env)
	path, err := AssertString(pathElem)
	if err != nil {
		ctx.Raise(err, pathElem)
	}

	//todo: better logging
	fmt.Printf("read-file: %v\n", path)

	data, err := ioutil.ReadFile(string(path))
	if err != nil {
		ctx.Raise(ErrIO, err)
	}

	env.Define(SymLoadFileName, path)

	prog := Parse(string(path), string(data))
	return prog
}
