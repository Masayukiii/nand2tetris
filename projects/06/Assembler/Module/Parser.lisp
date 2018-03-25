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
      (t (setf line (ppcre:scan-to-strings "[^\\s+]+" line))(format swap-stream "~a~%" line))
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

(defun dest ()
  (declare (special line))
  (cond
    ( (ppcre:scan "=" line) (setq dest-mnemonic (ppcre:scan-to-strings "^[^=]+" line)) (gethash dest-mnemonic *dest-mnemonic-table*))
    (t (string "000"))
  )

  ;if jump instrunction, return 000
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
      (format t "-------------------------------------")
      (format t "~a~%" cal-comp-mnemonic )
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

(defun address ()
  (declare (special line))
  (cond
    ((ppcre:scan "^@[0-9]+$" line)(convert_to_binary))
    (t (getAddress (ppcre:scan-to-strings "[^@]+" line)))
  )
)

(defun convert_to_binary ()
  (declare (special line))
  (let
    ((decimal (ppcre:scan-to-strings "[^@]+" line)))
    (generate_binary decimal)
  )
)

(defun generate_binary (i)
  (let*
    ((binary (format nil "~b" i))
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

(defun label-symbol ()
  (declare (special line))
  (setf label (ppcre:scan-to-strings "[^(^)]+" line))
  (addEntry label (generate_binary (1+ *line-count*)))
)
