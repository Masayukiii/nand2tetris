(in-package :cl-user)
(defpackage jack-tokenizer
  (:use :cl
        :ppcre)
)
(in-package :jack-tokenizer)

(defun tokenizer (content)
  (let*
    ((normalized-content (normalize-content content))
     (insufficient-tokens (cl-ppcre:split "\\s+" normalized-content))
     (normalized-tokens (normalize-tokens insufficient-tokens)))
     normalized-tokens))

(defun normalize-content (content)
  (replace-with-space (remove-comment content) *symbol-token*))

(defun normalize-tokens (tokens)
  (concatenate-string-token (remove-if (lambda (x) (equal x "")) tokens) nil))

(defun remove-comment (content)
  (ppcre:regex-replace-all "\/{2}.+" (ppcre:regex-replace-all "\/\\*.+\\*\/" content "") ""))

(defvar *symbol-token*
  (list "(" ")" "{" "}" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"))

(defvar *keyword-token*
  (list "class" "constructor" "function" "method" "field" "satic" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"))

(defun replace-sym (content sym)
  (ppcre:regex-replace-all (concatenate 'string "\\" sym) content (concatenate 'string " " sym " ")))

(defun replace-with-space (content lst)
  (cond
    ((null lst) content)
    (t ( replace-with-space (replace-sym content (car lst)) (cdr lst)) )))

(defun flatten (lst acum)
  (cond
    ((null lst) acum)
    (t (flatten (cdr lst) (append acum (car lst))))
    ))

(defun concatenate-string-token (tokens accum)
  (cond
    ((null tokens) (reverse accum))
    ((ppcre:scan "\"" (car tokens)) (concatenate-string-token (flatten (cdr (find-string-token tokens nil)) nil) (cons (car (find-string-token tokens nil)) accum)))
    (t (concatenate-string-token (cdr tokens) (cons (car tokens) accum) ))
    ))

(defun find-string-token (tokens accum)
  (cond
    ((null accum) (find-string-token (cdr tokens) (cons (car tokens) accum)))
    ((ppcre:scan "\"" (car tokens)) (list (concatenate-list (reverse (cons (car tokens) accum))) (cdr tokens)))
    (t (find-string-token (cdr tokens) (cons (car tokens) accum)))
  ))

(defun concatenate-list (lst)
  (cond
    ((null lst) "")
    (t (concatenate 'string (car lst) " " (concatenate-list (cdr lst))))))
