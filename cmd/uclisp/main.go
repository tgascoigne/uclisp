package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/tgascoigne/uclisp/ast"
	"github.com/tgascoigne/uclisp/parser"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".uclisp")
)

type fooObject struct {
	X, Y int
}

func dump(val ast.Value) string {
	astStr, _ := json.Marshal(val)
	return fmt.Sprintf("%v", string(astStr))
}

func main() {
	line := liner.NewLiner()
	defer line.Close()

	line.SetCtrlCAborts(true)

	// Autocomplete from the global namespace
	line.SetCompleter(func(line string) (c []string) {
		for n, _ := range ast.Global.Map() {
			if strings.HasPrefix(string(n), strings.ToLower(line)) {
				c = append(c, string(n))
			}
		}
		return
	})

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

	prog := parser.Parse("stdin", line)
	if prog == nil {
		panic("parse returned nil")
	}
	result := prog.Eval(ast.Global)
	log.Println(dump(result))
}
