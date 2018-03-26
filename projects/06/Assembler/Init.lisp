(format t "now loading Library module~%")
(load "library.lisp")
(format t "success~%")

(format t "now loading Parser module~%")
(load "Module/Parser.lisp")
(format t "success~%")

(format t "now loading Code module~%")
(load "Module/Code.lisp")
(format t "success~%")

(format t "now loading SymbolTable module~%")
(load "Module/SymbolTable.lisp")
(format t "success~%")

(format t "now loading InstructionTable module~%")
(load "Module/InstructionTable.lisp")
(format t "success~%")

(format t "now loading RomTable module~%")
(load "Module/RomTable.lisp")
(format t "success~%")

(format t "#############################~%")
(format t "now creating jump-mnemonic-table~%")
(make-jump-mnemonic-table)
(format t "#############################~%")

(format t "#############################~%")
(format t "now creating dest-mnemonic-table~%")
(make-dest-mnemonic-table)
(format t "#############################~%")

(format t "#############################~%")
(format t "now creating comp-mnemonic-table~%")
(make-comp-mnemonic-table)
(format t "#############################~%")

(format t "#############################~%")
(format t "now creating symbol-table~%")
(format t "you can rewrite this table by defining new symbol~%")
(make-symbol-table)
(format t "#############################~%")

(format t "#############################~%")
(format t "now creating rom-table~%")
(make-rom-table)
(format t "#############################~%")
