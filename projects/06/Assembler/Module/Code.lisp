(defun code ()
  (declare (special line))
  (format t "------------------------~%")
  (format t "~a~%" (comp))
  (format t "~a~%" (dest))
  (format t "~a~%" (jump))
  (format t "------------------------~%")
  (cond
    ((string= (commandType) "A_COMMAND") (address))
    ((string= (commandType) "C_COMMAND") (concatenate 'string "111" (comp) (dest) (jump))))
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
    (t (string "000")))
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
  (format t "☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆~%")
  (format t "~a~%" line)
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
    ((binary (format nil "~b" (parse-integer i)))
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
