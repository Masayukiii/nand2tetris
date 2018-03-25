(defun code ()
  (machine-language)
)

(defun machine-language ()
  (declare (special line))
  (format t "------------------------~%")
  (format t "~a~%" (comp))
  (format t "~a~%" (dest))
  (format t "~a~%" (jump))
  (format t "------------------------~%")

  (cond
    ((string= (commandType) "A_COMMAND") (address))
    ((string= (commandType) "C_COMMAND") (concatenate 'string "111" (comp) (dest) (jump)))
    ((string= (commandType) "L_COMMAND") (label-symbol))
    )
  )
