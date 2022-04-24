(in-package :common-lisp-user)

(defpackage :cl-naive-store.utils
  (:use :cl :cl-getx :cl-naive-store.naive-core)
  (:export

   :make-elements
   :definition-keys
   :implement-document-definition
   :implement-definitions-colllection))
