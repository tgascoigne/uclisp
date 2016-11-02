package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/peterh/liner"
	"github.com/tgascoigne/uclisp/bootstrap"
	"github.com/tgascoigne/uclisp/vm"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".uclisp")
	machine    = vm.NewVM()
	line       = liner.NewLiner()
)

func dump(val interface{}) string {
	return fmt.Sprintf("%v", val)
}

func main() {
	flag.Parse()

	defer line.Close()

	line.SetCtrlCAborts(true)

	log.SetPrefix("")
	log.SetFlags(0)

	if f, err := os.Open(history_fn); err == nil {
		line.ReadHistory(f)
		f.Close()
	}

	defer func() {
		if f, err := os.Create(history_fn); err != nil {
			log.Print("Error writing history file: ", err)
		} else {
			line.WriteHistory(f)
			f.Close()
		}
	}()

	machine.ReadFunc = promptLine
	machine.PrintFunc = func(el vm.Elem) {
		log.Print(dump(el))
	}

	loadFile(flag.Arg(0))
}

func promptLine() vm.Elem {
	var err error
	var el vm.Elem
	var expr string
	if expr, err = line.Prompt("> "); err == nil {
		if strings.TrimSpace(expr) != "" {
			line.AppendHistory(expr)
		}

		el, err = readLine(expr)
		if err == nil {
			return el
		}
	}

	if err == liner.ErrPromptAborted || err == io.EOF {
		machine.Halt()
		log.Print("Aborted")
	} else {
		log.Printf("Error reading line: %T\n", err)
	}
	return vm.Nil
}

func readLine(line string) (vm.Elem, error) {
	reader := bootstrap.NewReader("<repl>", strings.NewReader(line))
	el, err := reader.ReadElem()
	if err != nil {
		if err == io.EOF {
			// empty strings aren't an error
			return vm.Nil, nil
		}

		log.Printf("read: %v", err)
		return vm.Nil, err
	}

	return el, nil
}

func loadFile(path string) {
	fd, err := os.Open(path)
	if err != nil {
		panic(err)
	}

	defer fd.Close()

	reader := bootstrap.NewReader(path, fd)
	for {
		el, err := reader.ReadElem()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.Println(err)
		}
		code := bootstrap.Compile(machine, el)
		machine.Eval(code)
	}
}
