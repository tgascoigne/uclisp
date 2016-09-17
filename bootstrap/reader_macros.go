package bootstrap

import "github.com/tgascoigne/uclisp/vm"

func init() {
	readMacros[vm.Symbol("'")] = genWrapperMacro(vm.Symbol("quote"))
	readMacros[vm.Symbol("`")] = genWrapperMacro(vm.Symbol("backquote"))
	readMacros[vm.Symbol(",")] = genWrapperMacro(vm.Symbol("unquote"))
	readMacros[vm.Symbol(",@")] = genWrapperMacro(vm.Symbol("splice"))
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
