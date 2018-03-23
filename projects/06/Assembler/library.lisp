(format t "now loading quicklisp~%")
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(format t "success~%")


(format t "now loading cl-ppcre for regular expression~%")
(ql:quickload :cl-ppcre)
(format t "success~%")
