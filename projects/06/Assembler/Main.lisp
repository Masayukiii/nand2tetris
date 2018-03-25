(format t "now initializing~%")
(load "Init.lisp")
(format t "success~%")

(setf file (car *args*))

;which line reading now
(defparameter *line-count* 0)
(defun inc-line-count ()
  (cond
    ((string= (commandType) "L_COMMAND") t)
    (t (defparameter *line-count* (1+ *line-count*)))
    )
  )

;generate swp file not including whitespace and any comments.
(deleteWhitespaceAndComment file)

; config file path
(setf swp-file (concatenate 'string file ".swp"))
(setq file-name (ppcre:scan-to-strings "[^.]+" file))
(setf b-file (concatenate 'string file-name ".hack"))

; open stream
(setf input-stream (open swp-file :direction :input))
(setf output-stream (open b-file :direction :output))

; assembling
; TODO inplementation continue method, and increments line-count.
(loop
  (setf line (read-line input-stream nil :EOF))
  (cond((eq line :EOF) (return t)(format t "finished assembling~%")))
  (format t " now assembind ~a~%" line)
  (parse line)
  (setf binary (code))
  (format t "~a" binary)
  (format output-stream (concatenate 'string binary "~%")) ; if type of instruction is L, not write it. Only generating symbol info.
  (inc-line-count)
)

(close input-stream)
(close output-stream)
;(delete-file (probe-file swp-file))

(format t "~a" *symbol-table*)
