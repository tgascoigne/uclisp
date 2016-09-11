package main

import (
	"fmt"
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
)

func dump(val interface{}) string {
	return fmt.Sprintf("%v", val)
}

func main() {
	line := liner.NewLiner()
	defer line.Close()

	line.SetCtrlCAborts(true)

	if f, err := os.Open(history_fn); err == nil {
		line.ReadHistory(f)
		f.Close()
	}

	log.SetPrefix("")
	log.SetFlags(0)

	for {
		if expr, err := line.Prompt("> "); err == nil {
			doLine(expr)
			line.AppendHistory(expr)
		} else if err == liner.ErrPromptAborted {
			log.Print("Aborted")
			break
		} else {
			log.Print("Error reading line: ", err)
		}
	}

	if f, err := os.Create(history_fn); err != nil {
		log.Print("Error writing history file: ", err)
	} else {
		line.WriteHistory(f)
		f.Close()
	}
}

func doLine(line string) {
	reader := bootstrap.NewReader("<repl>", strings.NewReader(line))
	el, err := reader.ReadElem()
	if err != nil {
		log.Printf("read: %v", err)
		return
	}

	code := bootstrap.Compile(machine, el)
	log.Printf("compiled %v -> %v\n", line, code)
	result := machine.Eval(code)
	log.Println(dump(result))
}
