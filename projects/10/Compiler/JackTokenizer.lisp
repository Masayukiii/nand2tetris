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
     (normalized-tokens (normalize-tokens insufficient-tokens))
     (xml-tokens (convert-xml normalized-tokens)))
     xml-tokens))

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

(defun flatten-atom (lst accum)
  (cond
    ((null lst) (reverse accum))
    ((consp (car lst)) (flatten-atom (cdr lst) (reverse (flatten-atom (car lst) accum))))
    (t (flatten-atom (cdr lst) (cons (car lst) accum)))
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

(defun convert-xml (tokens)
  (let ((nested-tokens (list "<tokens>" (mapcar #'encode-symbol-for-xml (mapcar #'convert-xml-format tokens)) "</tokens>")))
    (flatten-atom nested-tokens nil)))

(defun convert-xml-format (token)
  (let ((token-type (terminal-type token)))
    (concatenate 'string "<" token-type ">" " " token " " "</" token-type ">")
  ))

(defun encode-symbol-for-xml (token)
  (cond
    ((equal token "<symbol> < </symbol>") "<symbol> &lt; </symbol>")
    ((equal token "<symbol> > </symbol>") "<symbol> &gt; </symbol>")
    ((equal token "<symbol> & </symbol>") "<symbol> &amp; </symbol>")
    (t token)
    ))

(defun terminal-type (token)
  (cond
    ((find-equal-char token *keyword-token*) "keyword")
    ((find-equal-char token *symbol-token*) "symbol")
    ((ppcre:scan "\\d+" token) "integerConstant")
    ((ppcre:scan "\"" token) "stringConstant")
    (t "identifier")
    )
)

(defun find-equal-char (char lst)
  (some (lambda (c) (equal c char)) lst)
)
