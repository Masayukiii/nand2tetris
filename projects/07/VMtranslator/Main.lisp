(load "Init.lisp")
(defvar *file* (car *args*))

(in-package :cl-user)
(defpackage vmtranslator.main
  (:use :cl)
  (:import-from :vmtranslator.parser :parse)
  (:import-from :vmtranslator.coderwriter :code)
)
(in-package :vmtranslator.main)


(defvar *file* cl-user::*file*)

(defvar *lists* (getLines *file*))

(writeAsm (flatten (code (parse *lists*))))
