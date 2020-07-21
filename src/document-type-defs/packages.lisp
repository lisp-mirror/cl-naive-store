(in-package :common-lisp-user)

(defpackage :cl-naive-document-type-defs
  (:use :cl :cl-getx :cl-naive-store)
  (:export
   :*example-type-defs*
   :getxe   
   :validate-xe
   :db-type-get-set))
