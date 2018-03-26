(format t "now initializing~%")
(load "Init.lisp")
(format t "success~%")

(setf file (car *args*))

;which line reading now


;generate swp file not including whitespace and any comments.
(deleteWhitespaceAndComment file)

; config file path
(setf swp-file (concatenate 'string file ".swp"))
(setf swp-swp-file (concatenate 'string swp-file ".swp"))
(setq file-name (ppcre:scan-to-strings "[^.]+" file))
(setf b-file (concatenate 'string file-name ".hack"))

(init-symbol swp-file swp-swp-file)

; open stream
(setf input-stream (open swp-swp-file :direction :input))
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
  (format output-stream (concatenate 'string binary "~%"))
)

(close input-stream)
(close output-stream)
(delete-file (probe-file swp-file))
(delete-file (probe-file swp-swp-file))
