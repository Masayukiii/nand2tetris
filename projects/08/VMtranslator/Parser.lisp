(in-package :cl-user)

(defpackage vmtranslator.parser
  (:use :cl)
)
(in-package :vmtranslator.parser)

(defun parse (lineLists)
  (remove-if #'includeCommentsOrWhiteLines? lineLists)
)

(defun includeCommentsOrWhiteLines? (x)
  (ppcre:scan "^//|^$" x)
)
