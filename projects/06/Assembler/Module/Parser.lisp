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

(defun commandType ()
  (declare (special line))
  (cond
    ((ppcre:scan "^@" line)(string :A_COMMAND))
    ((or (ppcre:scan ";" line) (ppcre:scan "=" line))(string :C_COMMAND))
    (        t             (string :L_COMMAND))
  )
)

(defun symbol ()
  (declare (special line))
)

(defun dest ()
  (declare (special line))
  (setq dest-mnemonic (ppcre:scan-to-strings "^[^=]+" line))
  (gethash dest-mnemonic *dest-mnemonic-table*)
)

(defun comp ()
  (declare (special line))
  (cond
    ((ppcre:scan ";" line)
     (setq jump-comp-mnemonic (ppcre:scan-to-strings "^[^;]+" line))
     (gethash jump-comp-mnemonic *comp-mnemonic-table*)
    )

    ((ppcre:scan "=" line)
      (setq cal-comp-mnemonic (ppcre:scan-to-strings "[^=]+$" line))
      (gethash cal-comp-mnemonic *comp-mnemonic-table*)
    )
  )
)

(defun jump ()
  (declare (special line))
  (cond
    ((ppcre:scan ";" line)
     (setq jump-mnemonic (ppcre:scan-to-strings "[^;]+$" line))
     (gethash jump-mnemonic *jump-mnemonic-table*)
    )
    (t (string "000"))
  )
)

(defun convert_to_binary ()
  (declare (special line))
  (let*
    ((decimal (ppcre:scan-to-strings "[^@]+" line))
     (binary (format nil "~b" (parse-integer decimal)))
     (shortage-zero-count (- 16 (length binary))))
     (concatenate 'string (generate-zero-string shortage-zero-count nil) binary)
  )
)

(defun generate-zero-string (i y)
  (cond
    ((<= i 0) y)
    (t (generate-zero-string (1- i) (concatenate 'string y "0")))
  )
)
