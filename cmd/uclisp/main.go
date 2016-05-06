package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/tgascoigne/uclisp/ast"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".uclisp")
)

type fooObject struct {
	X, Y int
	Bar  struct {
		Z int
	}
}

func dump(val interface{}) string {
	return fmt.Sprintf("%#v", val)
}

func main() {
	foo := fooObject{X: 10, Y: 20}
	ast.Builtin.Define(ast.Symbol("foo"), ast.Bind(&foo))

	log.SetPrefix("")
	log.SetFlags(0)

	if len(os.Args) > 1 {
		path := os.Args[1]
		prog := ast.Parse(path, "(load-file \""+path+"\")")
		result := prog.Eval(ast.Global)
		log.Print(dump(result))
	} else {
		repl()
	}
}

func repl() {
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

	prog := ast.Parse("stdin", line)
	if prog == nil {
		panic("parse returned nil")
	}
	result := prog.Eval(ast.Global)
	log.Println(dump(result))
}
