(format t "now loading quicklisp~%")
(let ((quicklisp-init "quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(format t "success~%")

(format t "now loading cl-ppcre for regular expression~%")
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

(format t "success~%")

(defun loading (file)
  (format t "now loading ~a~%" file)
  (load file)
  (format t "success~%")
)

(loading "JackTokenizer.lisp")
(loading "CompilationEngine.lisp")
