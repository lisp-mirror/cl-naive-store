(in-package :common-lisp-user)

(defpackage :cl-naive-indexed
  (:use :cl :cl-naive-store)
  (:export
   :*do-partial-indexing*
   :indexed-values-hashtables-mixin
    
   :indexed-collection-mixin   
   :uuid-index
   :key-value-index
   
   :indexes
   
   :hash

   :index-values
   :index-lookup-values
   :index-lookup-uuid
   :push-value-index
   :add-index
   :remove-index))
