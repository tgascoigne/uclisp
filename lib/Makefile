boot.lisp: std.lisp compiler.lisp # repl.lisp
	cat $^ > $@

boot.fasl: boot.lisp
	sbcl --script compile-file.lisp < $< > $@
