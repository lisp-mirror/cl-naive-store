(in-package :common-lisp-user)

(defpackage :cl-naive-store.document-type-defs
  (:use :cl :cl-getx :cl-naive-store.naive-core)
  (:export
   :*example-type-defs*
   :getxe
   :validate-xe
   :concrete-type-get-set

   :make-elements
   :definition-keys
   :implement-document-definition
   :implement-definitions-colllection))
