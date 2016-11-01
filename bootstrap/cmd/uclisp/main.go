package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
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

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	go func() {
		for _ = range c {
			if f, err := os.Create(history_fn); err != nil {
				log.Print("Error writing history file: ", err)
			} else {
				line.WriteHistory(f)
				f.Close()
			}
		}
	}()

	machine.ReadFunc = promptLine
	machine.PrintFunc = func(el vm.Elem) {
		log.Print(dump(el))
	}

	loadFile(flag.Arg(0))
}

func promptLine() vm.Elem {
	if expr, err := line.Prompt("> "); err == nil {
		line.AppendHistory(expr)
		return readLine(expr)
	} else if err == liner.ErrPromptAborted || err == io.EOF {
		log.Print("Aborted")
		os.Exit(1)
	} else {
		log.Print("Error reading line: ", err)
	}
	return vm.Nil
}

func readLine(line string) vm.Elem {
	reader := bootstrap.NewReader("<repl>", strings.NewReader(line))
	el, err := reader.ReadElem()
	if err != nil {
		log.Printf("read: %v", err)
		return nil
	}

	return el
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
