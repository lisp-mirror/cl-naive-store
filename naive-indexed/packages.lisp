(in-package :common-lisp-user)

(defpackage :cl-naive-indexed
  (:use :cl :cl-naive-store)
  (:export

   :indexed-collection-mixin   
   :uuid-index
   :key-value-index
   
   :indexes
   
   :hash
   :key-values
   :index-values
   :index-lookup-values
   :index-lookup-uuid
   :add-index
   :remove-index))
