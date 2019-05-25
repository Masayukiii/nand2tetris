(in-package :cl-user)
(defpackage vmtranslator.parser
  (:use :cl)
  (:export
    :parser-constructor
    :has-more-commands
    :advance
    :command-type
    :arg1
    :arg2))
(in-package :vmtranslator.parser)

(defun parser-constructor (file-name)
  (let ((stream (open file-name :direction :input)))
    (labels ((read-lines (lst)
        (let ((line (read-line stream nil :EOF)))
          (if (eql line :EOF) lst (read-lines (cons line lst))))))
    (defparameter *lines* (read-lines nil)))))

(defun has-more-commands ()
  (not (eq (car *lines*) nil)))

(defun advance ()
  (defparameter *line* (car *lines*))
  (defparameter *lines* (cdr *lines*)))

(defun command-type ()
  (cond
     ((ppcre:scan "push"     *line*) (string :C_PUSH))
     ((ppcre:scan "pop"      *line*) (string :C_POP ))
     ((ppcre:scan "label"    *line*) (string :C_LABEL ))
     ((ppcre:scan "goto"     *line*) (string :C_GOTO ))
     ((ppcre:scan "if-goto"  *line*) (string :C_IF ))
     ((ppcre:scan "function" *line*) (string :C_FUNCTION ))
     ((ppcre:scan "return"   *line*) (string :C_RETURN ))
     ((ppcre:scan "call"     *line*) (string :C_CALL ))
     (      t                        (string :C_ARITHMETIC ))))

(defun arg1 ()
  (let ((c-type (command-type)))
  (cond
    ((string= c-type (string :C_ARITHMETIC)) (cadr (ppcre:all-matches-as-strings "\\w+" *line*))))))

(defun arg2 ()
  (let ((c-type (command-type *line*)))
  (cond
    ((or (string= c-type (string :C_PUSH)) (string= c-type (string :C_POP)) (string= c-type (string :C_FUNCTION)) (string= c-type (string :C_CALL))))
    (parse-integer (caddr (ppcre:all-matches-as-strings "\\w+" *line*))))))
