package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/tgascoigne/uclisp"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".uclisp")
)

func dump(val interface{}) string {
	return fmt.Sprintf("%v", val)
}

func main() {
	uclisp.Global.Define(uclisp.SymLoadFileName, uclisp.String("stdin"))

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
	defer func() {
		if r := recover(); r != nil {
			log.Println("Parse error: ", r)
		}
	}()

	prog := uclisp.Parse("stdin", line)
	if prog == nil {
		panic("parse returned nil")
	}
	result := prog.Eval(uclisp.Global)
	log.Println(dump(result))
}
