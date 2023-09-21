(in-package :common-lisp-user)

(defpackage :cl-naive-store.utils
  (:use :cl :cl-getx :cl-naive-store.naive-core)
  (:export

   ;;Used for sending docs over http.
   :doc-to-sexp
   :docs-to-sexps

   :make-elements
   :definition-keys
   :implement-document-definition
   :implement-definitions-colllection))
