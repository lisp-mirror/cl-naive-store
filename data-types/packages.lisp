(in-package :common-lisp-user)

(defpackage :cl-naive-data-types
  (:use :cl :cl-naive-store)
  (:export
   
   :*example-type-defs*
   :db-type-get-set
   :getfx
   :getsfx
   :validate-sfx
  
   ))
