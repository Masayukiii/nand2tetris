(defparameter *line-count* 0)

(defun init-symbol (swp-file swp-swp-file)
  (setf input-stream  (open swp-file :direction :input))
  (setf output-stream (open swp-swp-file :direction :output))
  (loop
    (setf line (read-line input-stream nil :EOF))
    (if (eq line :EOF) (return t))
    (parse line)
    (cond
      ((string= (commandType) "L_COMMAND") (label-symbol))
      (t (format output-stream (concatenate 'string line "~%")))
    )
    (inc-line-count)
  )
  (close input-stream)
  (close output-stream)
)

(defun label-symbol ()
  (declare (special line))
  (setf label (ppcre:scan-to-strings "[^(^)]+" line))
  (addEntry label (generate_binary  (write-to-string *line-count*) ))
)

(defun inc-line-count ()
  (cond
    ((string= (commandType) "L_COMMAND") t)
    (t (defparameter *line-count* (1+ *line-count*)))
    )
  )


(defun addEntry (symbol address)
  (setf (gethash symbol *symbol-table*) address)
)

(defun getAddress (symbol)
  (format t "~a~%" (gethash symbol *symbol-table*))
  (setf symbol-value (gethash symbol *symbol-table*))
  (cond
    ((null symbol-value) (addVaribleSymbol symbol))
    (t symbol-value)
  )
)

(defun addVaribleSymbol (symbol)
  (setf rom-count (hash-table-count *rom-table*))
  (setf rom-address (generate_binary (write-to-string rom-count)))
  (setf (gethash symbol *rom-table*) rom-address)
  (setf (gethash symbol *symbol-table*) rom-address)
)
