(load "Init.lisp")
(defvar *file* (car *args*))

(in-package :cl-user)
(defpackage vmtranslator.main
  (:use :cl)
  (:import-from :vmtranslator.parser :parse)
  (:import-from :vmtranslator.coderwriter :code)
)
(in-package :vmtranslator.main)

(defun getLines (file)
  (let (( stream (open file :direction :input)))
    (reverse (readLine stream nil))
  )
)

(defun readLine (stream lst)
  (let
    ((line (read-line stream nil :EOF)))
    (cond
      ((eql line :EOF)(close stream) lst)
      ( t  (readLine stream (cons  line lst)))
    )
  )
)

(defun writeAsm (lst)
  (let
    ((outStream (open (concatenate 'string *file* ".asm") :direction :output)))
    (writeLine outStream lst)
  )
)

(defun writeLine (stream lst)
  (cond
    ((eq lst nil)(close stream))
    (t (format stream "~a~%" (car lst))(writeLine stream (cdr lst)))
  )
)

(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (loop for a in lst append (flatten a)))
  )
)

(defvar *file* cl-user::*file*)

(defvar *lists* (getLines *file*))

(writeAsm (flatten (code (parse *lists*))))
