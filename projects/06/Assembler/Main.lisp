(format t "now initializing~%")
(load "Init.lisp")
(format t "success~%")

(format t "please input fine name~%")
(setf file (string (read)))

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
(loop
  (setf line (read-line input-stream nil :EOF))
  (cond((eq line :EOF) (return t)(format t "finished assembling~%")))
  (format t " now assembind ~a~%" line)
;  (code (parse line))
;  (write)
)

(close input-stream)
(close output-stream)
