(in-package :common-lisp-user)

(defpackage :cl-naive-indexed
  (:use :cl :cl-naive-store)
  (:export

   :indexed-collection-mixin   
   :uuid-index
   :key-value-index
   
   :hash
   :key-values
   :key-values-hash
   :index-lookup-values-hash
   :index-lookup-uuid
   :add-index
   :remove-index))
