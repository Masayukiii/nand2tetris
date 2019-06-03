(in-package :cl-user)

(defpackage vmtranslator.coderwriter
  (:use :cl)
)

(in-package :vmtranslator.coderwriter)


(defun convertAssmble (lines)
  (mapcar #'convertAssmbleByInstructionType lines)
)

(defun convertAssmbleByInstructionType (line)
  (let*
    ((lst (ppcre:all-matches-as-strings "\\w+" line))
      (command (car lst))
      (segment (cadr lst))
      (index (caddr lst)))
      (cond
        ((string= (commandType line) :C_ARITHMETIC) (convertArithmeticAssmble line))
        ((string= (commandType line) :C_PUSH)       (convertPushAssmble segment index))
        ((string= (commandType line) :C_POP)        (convertPopAssmble segment index))
        ((string= (commandType line) :C_LABEL)      (convertLabelAssmble line))
        ((string= (commandType line) :C_GOTO)       (convertGotoAssmble line))
        ((string= (commandType line) :C_IF)         (convertIfAssmble line))
        ((string= (commandType line) :C_FUNCTION)   (convertFunctionAssmble line))
        ((string= (commandType line) :C_RETURN)     (convertReturnAssmble line))
        ((string= (commandType line) :C_CALL)       (convertCallAssmble line))
      )
  )
)

(defun commandType (line)
  (cond
    ((ppcre:scan "push"     line) (string :C_PUSH ))
    ((ppcre:scan "pop"      line) (string :C_POP ))
    ((ppcre:scan "label"    line) (string :C_LABEL ))
    ((ppcre:scan "if-"  line) (string :C_IF )) ; if include goto , meet below condition
    ((ppcre:scan "goto"     line) (string :C_GOTO ))
    ((ppcre:scan "function" line) (string :C_FUNCTION ))
    ((ppcre:scan "return"   line) (string :C_RETURN ))
    ((ppcre:scan "call"     line) (string :C_CALL ))
    (      t                      (string :C_ARITHMETIC ))
  )
)

(defun convertArithmeticAssmble (line)
  (let ((label1 (string (gensym))) (label2 (string (gensym))))
    (cond
      ((string= (arg1 line) "add")(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "M=M+D" "@R0" "M=M+1"))
      ((string= (arg1 line) "sub")(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "M=M-D" "@R0" "M=M+1"))
      ((string= (arg1 line) "neg")(list  "@R0" "M=M-1" "A=M" "M=-M" "@R0" "M=M+1"))
      ((string= (arg1 line) "eq" )(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "D=M-D"  (concatenate 'string "@" label1) "D;JEQ" "@R0" "A=M" "M=0" "@R0" "M=M+1" (concatenate 'string "@" label2) "0;JMP" (concatenate 'string "(" label1 ")") "@R0" "A=M" "M=-1" "@R0" "M=M+1" (concatenate 'string "(" label2 ")") ))
      ((string= (arg1 line) "gt" )(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "D=M-D"  (concatenate 'string "@" label1) "D;JGT" "@R0" "A=M" "M=0" "@R0" "M=M+1" (concatenate 'string "@" label2) "0;JMP" (concatenate 'string "(" label1 ")") "@R0" "A=M" "M=-1" "@R0" "M=M+1" (concatenate 'string "(" label2 ")") ))
      ((string= (arg1 line) "lt" )(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "D=M-D"  (concatenate 'string "@" label1) "D;JLT" "@R0" "A=M" "M=0" "@R0" "M=M+1" (concatenate 'string "@" label2) "0;JMP" (concatenate 'string "(" label1 ")") "@R0" "A=M" "M=-1" "@R0" "M=M+1" (concatenate 'string "(" label2 ")") ))
      ((string= (arg1 line) "and")(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "M=D&M" "@R0" "M=M+1"))
      ((string= (arg1 line) "or" )(list  "@R0" "M=M-1" "A=M" "D=M" "@R0" "M=M-1" "A=M" "M=D|M" "@R0" "M=M+1"))
      ((string= (arg1 line) "not")(list "@R0" "M=M-1" "A=M" "M=!M" "@R0" "M=M+1"))
    )
  )
)

(defun convertPushAssmble (segment index)
  (cond
   ((string= segment "constant") (list (concatenate 'string "@" index) "D=A" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "local")    (list "@R1" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "argument") (list "@R2" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "this")     (list "@R3" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "that")     (list "@R4" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "pointer")  (list "@3" "D=A" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "temp")     (list "@R5" "D=A" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "static")   (list (concatenate 'string "@" "StaticTest." index) "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
  )
)

(defun convertPopAssmble  (segment index)
  (let ((label1 (string (gensym))) (label2 (string (gensym))))
    (cond
      ((string= segment "local")    (list "@R1" "D=M" (concatenate 'string "@" index) "D=D+A" "@R1" "M=D" "@R0" "A=M-1" "D=M" "@R1" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R1" "M=M-D" "@R0" "M=M-1"))
      ((string= segment "argument") (list "@R2" "D=M" (concatenate 'string "@" index) "D=D+A" "@R2" "M=D" "@R0" "A=M-1" "D=M" "@R2" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R2" "M=M-D" "@R0" "M=M-1"))
      ((string= segment "this")     (list "@R3" "D=M" (concatenate 'string "@" index) "D=D+A" "@R3" "M=D" "@R0" "A=M-1" "D=M" "@R3" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R3" "M=M-D" "@R0" "M=M-1"))
      ((string= segment "pointer")  (list (concatenate 'string "@" index) "D=A" (concatenate 'string "@" label1) "D;JEQ" "@R0" "A=M-1" "D=M" "@R4" "M=D" (concatenate 'string "@" label2) "0;JMP" (concatenate 'string "(" label1 ")") "@R0" "A=M-1" "D=M" "@R3" "M=D" (concatenate 'string "(" label2 ")") "@R0" "M=M-1"  ))
      ((string= segment "temp")     (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" (write-to-string (+ (parse-integer index) 5))) "M=D"))
      ((string= segment "static")   (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" "StaticTest." index) "M=D"))
      ((string= segment "that")     (list "@R4" "D=M" (concatenate 'string "@" index) "D=D+A" "@R4" "M=D" "@R0" "A=M-1" "D=M" "@R4" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R4" "M=M-D" "@R0" "M=M-1"))
    )
  )
)

(defun convertLabelAssmble (line)
 (list (concatenate 'string "(" (arg1 line) ")"))
)

(defun convertGotoAssmble (line)
  (let ((label-var (arg1 line)))
    (format t "~a" (list (concatenate 'string "@" label-var) "A=M"))
    (list (concatenate 'string "@" label-var) "A=M")
  )
)

(defun convertIfAssmble (line)
  (let ((des (arg2 line)))
    (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" des) "D;JNE")
  )
)

(defun convertFunctionAssmble (line)
  (let ((function-name (arg2 line))
        (argument-count (arg3 line))
        (label1 (string (gensym)))
        (label2 (string (gensym))))
    (list (concatenate 'string "(" function-name ")") (concatenate 'string "@" argument-count) "D=A" (concatenate 'string "(" label1 ")") (concatenate 'string "@" label2) "D;JEQ" "@R0" "A=M" "M=0" "@R0" "M=M+1" "D=D-1" (concatenate 'string "@" label1) "0;JMP" (concatenate 'string "(" label2 ")"))
  )
)

(defun convertReturnAssmble (line)
  (let ((function-name (arg1 line))
        (argument-count (arg2 line)) ; ここの数字設定がおかしい
        (label1 (string (gensym)))
        (label2 (string (gensym))))
        (list
          ; TODO: obey last in, first out principle
          ; R13-R15 segment is free segment
          "@R0" "M=M-1" "D=M" "@R13" "M=D"
          "@R0" "A=M" "D=M" "@R2" "A=M" "M=D"
          "@R2" "D=M+1" "@R0" "M=D"
          "@1" "D=A" "@R13" "D=M-D" "A=D" "D=M" "@R4" "M=D"
          "@2" "D=A" "@R13" "D=M-D" "A=D" "D=M" "@R3" "M=D"
          "@3" "D=A" "@R13" "D=M-D" "A=D" "D=M" "@R2" "M=D"
          "@4" "D=A" "@R13" "D=M-D" "A=D" "D=M" "@R1" "M=D"
          "@5" "D=A" "@R13" "D=M-D" "A=D" "A=M"
        )
  )
)

(defun convertCallAssmble (line)
  (let ((function-name (arg1 line))
        (argument-count (arg2 line))
        (label1 (string (gensym)))
        (label2 (string (gensym))))
        (list
          (concatenate 'string "@" label1) "D=A" "@R0" "A=M" "M=D" "@R0" "M=M+1" ; この前に引数をpushしているから気にする必要はない
          "@R1" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"
          "@R2" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"
          "@R3" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"
          "@R4" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"
          "@R0" "D=M" (concatenate 'string "@" argument-count) "D=D-A" "@5" "D=D-A" "@R2" "M=D"
          "@R0" "D=M" "@R1" "M=D"
          (concatenate 'string "@" function-name) "0;JMP"
          (concatenate 'string "(" label1 ")")
        )
  )
)

; common function
(defun arg1 (line)
  (let ((c-type (commandType line)))
    (cond
      ((string= c-type (string :C_ARITHMETIC)) line)
      (t (GetArgument1 line))
    )
  )
)

(defun arg2 (line)
  (caddr (ppcre:all-matches-as-strings "\\w+" line))
)

(defun arg3 (line)
  (cadddr (ppcre:all-matches-as-strings "\\w+" line))
)

(defun GetArgument1 (line)
  (cadr (ppcre:all-matches-as-strings "\\w+" line))
)
