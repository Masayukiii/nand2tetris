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
