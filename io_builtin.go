package uclisp

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

const (
	SymLoadFileName = Symbol("*load-file-path*")
)

func init() {
	Builtin.Define("file-exists-p", Procedure(fileexistsForm))
	Builtin.Define("load-file", Procedure(loadfileForm))
	Builtin.Define("read-file", Procedure(readfileForm))
	Builtin.Define("message", Procedure(messageForm))
}

func messageForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	format, err := AssertString(args[0])
	if err != nil {
		ctx.Raise(err, args[0])
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
		ctx.Raise(err, path)
	}

	env.Define(SymLoadFileName, path)

	prog := Parse(string(path), string(data))
	return prog
}
