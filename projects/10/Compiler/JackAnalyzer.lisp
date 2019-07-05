(load "Init.lisp")
(defvar *name* (car *args*))

(in-package :cl-user)
(defpackage jack-analyzer
  (:use :cl
        :jack-tokenizer
        :compilation-engine)
  (:import-from :cl-user *name*)
)
(in-package :jack-analyzer)

(defvar *target-directory* (directory (format nil "C:/Users/sekai/.ghq/github.com/Masayukiii/nand2tetris/projects/10/~a/*.jack" *name*)))
(defun get-jack-name (path)
  (svref (nth-value 1 (ppcre:scan-to-strings "(.+).jack" (file-namestring path))) 0))
(defvar *jack-file-names* (mapcar #'get-jack-name *target-directory*))

(defun read-file (file-name)
  (alexandria:read-file-into-string file-name))

(defun write-file (file-name content)
  (with-open-file (stream file-name
        :direction :output
        :if-does-not-exist :create)
    (format stream content)))

(defun write-lst-to-file (file-name lst)
  (let ((out-stream (open file-name :direction :output)))
    (write-lst out-stream lst)
  ))

(defun write-lst (stream lst)
  (cond
    ((eq lst nil)(close stream))
    (t (format stream "~a~%" (car lst))(write-lst stream (cdr lst)))
  )
)

(defun main (file-name)
 (let* (
   (content (read-file file-name))
   (tokens (jack-tokenizer::tokenizer content)))
     (write-lst-to-file (concatenate 'string file-name ".xml") tokens)
   ))

(main "hoge.jack")
