(defun make-jump-mnemonic-table ()
  (defvar *jump-mnemonic-table* (make-hash-table :test #'equal))
  (setf (gethash "null" *jump-mnemonic-table*) "000")
  (setf (gethash "JGT"  *jump-mnemonic-table*) "001")
  (setf (gethash "JEQ"  *jump-mnemonic-table*) "010")
  (setf (gethash "JGE"  *jump-mnemonic-table*) "011")
  (setf (gethash "JLT"  *jump-mnemonic-table*) "100")
  (setf (gethash "JNE"  *jump-mnemonic-table*) "101")
  (setf (gethash "JLE"  *jump-mnemonic-table*) "110")
  (setf (gethash "JMP"  *jump-mnemonic-table*) "111")
)

(defun make-dest-mnemonic-table ()
  (defvar *dest-mnemonic-table* (make-hash-table :test #'equal))
  (setf (gethash "null" *dest-mnemonic-table*) "000")
  (setf (gethash "M"    *dest-mnemonic-table*) "001")
  (setf (gethash "D"    *dest-mnemonic-table*) "010")
  (setf (gethash "MD"   *dest-mnemonic-table*) "011")
  (setf (gethash "A"    *dest-mnemonic-table*) "100")
  (setf (gethash "AM"   *dest-mnemonic-table*) "101")
  (setf (gethash "AD"   *dest-mnemonic-table*) "111")
  (setf (gethash "AMD"  *dest-mnemonic-table*) "111")
)

(defun make-comp-mnemonic-table ()
  (defvar *comp-mnemonic-table* (make-hash-table :test #'equal))
  (setf (gethash "0"    *comp-mnemonic-table*) "0101010")
  (setf (gethash "1"    *comp-mnemonic-table*) "0111111")
  (setf (gethash "-1"   *comp-mnemonic-table*) "0111010")
  (setf (gethash "D"    *comp-mnemonic-table*) "0001100")
  (setf (gethash "A"    *comp-mnemonic-table*) "0110000")
  (setf (gethash "!D"   *comp-mnemonic-table*) "0001101")
  (setf (gethash "!A"   *comp-mnemonic-table*) "0110001")
  (setf (gethash "-D"   *comp-mnemonic-table*) "0001111")
  (setf (gethash "-A"   *comp-mnemonic-table*) "0110011")
  (setf (gethash "D+1"  *comp-mnemonic-table*) "0011111")
  (setf (gethash "A+1"  *comp-mnemonic-table*) "0110111")
  (setf (gethash "D-1"  *comp-mnemonic-table*) "0001110")
  (setf (gethash "A-1"  *comp-mnemonic-table*) "0110010")
  (setf (gethash "D+A"  *comp-mnemonic-table*) "0000010")
  (setf (gethash "D-A"  *comp-mnemonic-table*) "0010011")
  (setf (gethash "A-D"  *comp-mnemonic-table*) "0000111")
  (setf (gethash "D&A"  *comp-mnemonic-table*) "0000000")
  (setf (gethash "D|A"  *comp-mnemonic-table*) "0010101")
  (setf (gethash "M"    *comp-mnemonic-table*) "1110000")
  (setf (gethash "!M"   *comp-mnemonic-table*) "1110001")
  (setf (gethash "-M"   *comp-mnemonic-table*) "1110011")
  (setf (gethash "M+1"  *comp-mnemonic-table*) "1110111")
  (setf (gethash "M-1"  *comp-mnemonic-table*) "1110010")
  (setf (gethash "D+M"  *comp-mnemonic-table*) "1000010")
  (setf (gethash "D-M"  *comp-mnemonic-table*) "1001011")
  (setf (gethash "M-D"  *comp-mnemonic-table*) "1000111")
  (setf (gethash "D&M"  *comp-mnemonic-table*) "1000000")
  (setf (gethash "D|M"  *comp-mnemonic-table*) "1010101")
)