(in-package :cl-user)

(defpackage vmtranslator.coderwriter
  (:use :cl)
)

(in-package :vmtranslator.coderwriter)

(defun code (lineLists)
  (convertPushPopAssmble (convertArithmeticAssmble lineLists))
)

(defun convertArithmeticAssmble (lineLists)
  (mapcar #'writeArithmeticAssmble lineLists)
)

(defun writeArithmeticAssmble (line)
  (cond
    ((string= (commandType line) :C_ARITHMETIC)(arithmeticAssemblerList line))
    (t line)
  )
)

(defun convertPushPopAssmble (lineLists)
  (mapcar #'writePushPopAssmble lineLists)
)

(defun writePushPopAssmble (line)
  (cond
    ((atom line) (not (null line)) (unless (string= (commandType line) :C_ARITHMETIC)(writePushPop line)))
    (t line)
  )
)

(defun popOrPushCommnad? (line)
  (or (string= (commandType line) "C_POP") (string= (commandType line) "C_PUSH"))
)

(defun arithmeticAssemblerList (line)
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


(defun writePushPop (line)
  (let*  ((lst (ppcre:all-matches-as-strings "\\w+" line))
        (command (car lst))
        (segment (cadr lst))
        (index (caddr lst)))
    (cond
      ((string= command "push")(writePush segment index))
      ((string= command "pop" )(writePop segment index)))
  )
)

(defun writePush (segment index)
  (cond
   ((string= segment "constant") (list (concatenate 'string "@" index) "D=A" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "local")    (list "@R1" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "argument") (list "@R2" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "this")     (list "@R3" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "that")     (list "@R4" "D=M" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "pointer")  (list "@R3" "D=A" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "temp")     (list "@R5" "D=A" (concatenate 'string "@" index) "D=D+A" "A=D" "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
   ((string= segment "static")   (list (concatenate 'string "@" "StackTest." index) "D=M" "@R0" "A=M" "M=D" "@R0" "M=M+1"))
  )
)

(defun writePop  (segment index)
  (cond
    ((string= segment "local")    (list "@R1" "D=M" (concatenate 'string "@" index) "D=D+A" "@R1" "M=D" "@R0" "A=M-1" "D=M" "@R1" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R1" "M=M-D" "@R0" "M=M-1"))
    ((string= segment "argument") (list "@R2" "D=M" (concatenate 'string "@" index) "D=D+A" "@R2" "M=D" "@R0" "A=M-1" "D=M" "@R2" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R2" "M=M-D" "@R0" "M=M-1"))
    ((string= segment "this")     (list "@R3" "D=M" (concatenate 'string "@" index) "D=D+A" "@R3" "M=D" "@R0" "A=M-1" "D=M" "@R3" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R3" "M=M-D" "@R0" "M=M-1"))
    ((string= segment "that")     (list "@R4" "D=M" (concatenate 'string "@" index) "D=D+A" "@R4" "M=D" "@R0" "A=M-1" "D=M" "@R4" "A=M" "M=D" (concatenate 'string "@" index) "D=A" "@R4" "M=M-D" "@R0" "M=M-1"))
    ((string= segment "pointer")  (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" (write-to-string (+ (parse-integer index) 3))) "M=D"))
    ((string= segment "temp")     (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" (write-to-string (+ (parse-integer index) 5))) "M=D"))
    ((string= segment "static")   (list "@R0" "M=M-1" "A=M" "D=M" (concatenate 'string "@" "StackTest." index) "M=D"))
  )
)

(defun commandType (line)
  (cond
    ((ppcre:scan "push"     line) (string :C_PUSH ))
    ((ppcre:scan "pop"      line) (string :C_POP ))
    ((ppcre:scan "label"    line) (string :C_LABEL ))
    ((ppcre:scan "goto"     line) (string :C_GOTO ))
    ((ppcre:scan "if-goto"  line) (string :C_IF ))
    ((ppcre:scan "function" line) (string :C_FUNCTION ))
    ((ppcre:scan "return"   line) (string :C_RETURN ))
    ((ppcre:scan "call"     line) (string :C_CALL ))
    (      t                      (string :C_ARITHMETIC ))
  )
)

(defun arg1 (line)
  (let ((c-type (commandType line)))
    (cond
      ((string= c-type (string :C_ARITHMETIC)) line)
      (t (GetArgument1 line))
    )
  )
)

(defun GetArgument1 (line)
  (cadr (ppcre:all-matches-as-strings "\\w+" line))
)

(defun arg2 (line)
  (caddr (ppcre:all-matches-as-strings "\\w+" line))
)
