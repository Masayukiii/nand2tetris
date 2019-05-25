(load "Init.lisp")
(defvar *file-name* (car *args*))

(in-package :cl-user)
(defpackage :dom-parser
  (:use :cl))
(in-package :dom-parser)


(defvar *file-name* cl-user::*file-name*)


(defun read-file (file-name)
  (let ((stream (open file-name :direction :input))
        (content ""))
    (read-lines stream content)))


(defun read-lines (stream content)
  (let
    ((line (read-line stream nil :EOF)))
    (cond
      ((eql line :EOF)(close stream) content)
      ( t  (read-lines stream (concatenate 'string content line)))
    )
  )
)

(defun convert-to-list  (tags)
  (if (ppcre:scan "</[^>]*>" (car tags))
    (car tags)
    (cons (car tags) (convert-to-list (cdr tags)))
  )
)

(defun match-tags (content)
  (ppcre:all-matches-as-strings "<[^>]+>" content))

(defun main ()
  (format t "~a" (convert-to-list (match-tags (read-file *file-name*)))))


(main)
