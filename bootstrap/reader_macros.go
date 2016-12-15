package bootstrap

import (
	"fmt"

	"github.com/tgascoigne/uclisp/vm"
)

func init() {
	readMacros[vm.Symbol("'")] = genWrapperMacro(vm.Symbol("quote"))
	readMacros[vm.Symbol("`")] = genWrapperMacro(vm.Symbol("backquote"))
	readMacros[vm.Symbol(",")] = genWrapperMacro(vm.Symbol("unquote"))
	readMacros[vm.Symbol(",@")] = genWrapperMacro(vm.Symbol("splice"))

	for op := vm.OpNOP; op < vm.OpEND; op++ {
		opStr := fmt.Sprintf("%v", op)
		opStr = opStr[2:] // trim off 'Op' prefix
		readMacros[vm.Symbol("$"+opStr)] = genOpcodeMacro(op)
	}

	readMacros[vm.Symbol("nil")] = nilMacro
}

func genWrapperMacro(wrapSym vm.Symbol) readMacroFunc {
	return func(r *Reader) (vm.Elem, error) {
		target, err := r.ReadElem()
		if err != nil {
			return nil, err
		}

		return vm.List(wrapSym, target), nil
	}
}

func genOpcodeMacro(opcode vm.Op) readMacroFunc {
	return func(*Reader) (vm.Elem, error) {
		return opcode, nil
	}
}

func nilMacro(*Reader) (vm.Elem, error) {
	return vm.Nil, nil
}
