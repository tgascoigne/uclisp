package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/tgascoigne/uclisp"
)

func main() {
	data, err := ioutil.ReadFile("boot.lbc")
	if err != nil {
		panic(err)
	}

	control, err := uclisp.Read(string(data))
	if err != nil {
		fmt.Printf("Error reading boot: %v\n", err)
		os.Exit(1)
	}

	vm := uclisp.NewVM()
	defer vm.Dump()

	for _, sexpr := range control {
		vm.Eval(vm.Compile(sexpr))
	}

	scanner := bufio.NewScanner(os.Stdin)
	fmt.Printf("> ")
	for scanner.Scan() {
		line := scanner.Text()
		expr, err := uclisp.Read(line)
		if err != nil {
			fmt.Printf("read error: %v\n", err)
			continue
		}

		result := vm.Eval(vm.Compile(expr[0]))
		fmt.Printf("%v\n", result)
		fmt.Printf("> ")
	}
}
