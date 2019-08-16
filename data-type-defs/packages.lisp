(in-package :common-lisp-user)

(defpackage :cl-naive-data-type-defs
  (:use :cl :cl-naive-store)
  (:export
   :*example-type-defs*
   :getfx
   :getsfx
   :validate-sfx))
