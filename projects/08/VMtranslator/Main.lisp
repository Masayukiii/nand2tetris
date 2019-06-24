(load "Init.lisp")
(defvar *name* (car *args*))

(in-package :cl-user)
(defpackage vmtranslator.main
  (:use :cl)
  (:import-from :vmtranslator.parser :parse)
  (:import-from :vmtranslator.coderwriter :convertAssmble)
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
    ((outStream (open (concatenate 'string *name* ".asm") :direction :output)))
    (writeLine outStream lst)
  )
)

(defun writeLine (stream lst)
  (cond
    ((eq lst nil)(close stream))
    (t (format stream "~a~%" (car lst))(writeLine stream (cdr lst)))
  )
)

(defvar *name* cl-user::*name*)

(defvar *target-directory* (directory (format nil "C:/Users/sekai/.ghq/github.com/Masayukiii/nand2tetris/projects/08/FunctionCalls/~a/*.vm" *name*)))

(defun flatten (lst acum)
  (cond
    ((null lst) acum)
    (t (flatten (cdr lst) (append acum (car lst))))
    )
)

(defun get-vm-name (path)
    (svref (nth-value 1 (ppcre:scan-to-strings "(.+).vm" (file-namestring path))) 0)
  )

(defparameter vm-names (mapcar #'get-vm-name *target-directory*))
(defparameter lines    (mapcar #'getLines *target-directory*))

(defparameter vm-info (mapcar #'cons vm-names lines))

(defun convert (lst)
   (flatten (convertAssmble (parse (cdr lst)) (car lst)) nil)
)

(writeAsm (flatten (mapcar #'convert vm-info) nil))
