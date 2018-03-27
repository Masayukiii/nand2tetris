(defun parse (line)
  (declare (special line))
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
      (t (setf line (ppcre:scan-to-strings "[^\\s]+" line))(format swap-stream "~a~%" line))
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
