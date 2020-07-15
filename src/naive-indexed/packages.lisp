(in-package :common-lisp-user)

(defpackage :cl-naive-indexed
  (:use :cl :cl-naive-store :cl-getx)
  (:export
   :*do-partial-indexing*
   :indexed-values-hashtables-mixin
    
   :indexed-collection-mixin   
   :uuid-index
   :key-value-index
   
   :indexes
   
   :hash
   
   :index-lookup-values   
   :index-lookup-hash
   :index-lookup
   :add-index
   
   :remove-index))

(defpackage :indexed-impl
  (:use :cl :cl-getx :cl-murmurhash :cl-naive-store :cl-naive-indexed)
  (:export
   
   :index-values
   :push-value-index
   :remove-value-index))
