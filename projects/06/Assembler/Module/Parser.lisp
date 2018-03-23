
(defun parse (line)
  (declare (special line))
  (list commnadType dest comp jump)
)

(defun deleteWhitespaceAndComment (file)
  (format t "--------------------------now deleting whitespace and commetns----------------------------------~%")
  (setf swap-file (concatenate 'string file ".swp"))
  (setf stream (open file :direction :input))
  (setf swap-stream (open swap-file :direction :output))
  (loop
    (setf line (read-line stream nil :EOF))
    (cond ((eq line :EOF)(return t)(format t "finish delete whitespaces and comments")))
    (cond
      ((comment-or-whitespace-p line) (format t "...~%")) ;is this line comments or whitespace?
      (t (format swap-stream "~a~%" line))
    )
  )
  (format t "--------------------------finish deleting whitespace and comments-------------------------------~%")
  (close stream)
  (close swap-stream)
)

(defun comment-or-whitespace-p (line)
  (setf line (string line))
  (or (ppcre:scan "^//" line) (eq (length line) 0))
)

; While reach EOF, continuing parse.
;(defun hasMoreCommands ()
;)
;(defun advance ()
;)

(defun commandType ()
  (declare (special line))
)

(defun symbol ()
  (declare (special line))
)

(defun dest ()
  (declare (special line))
)

(defun comp ()
  (declare (special line))
)

(defun jump ()
  (declare (special line))
)
