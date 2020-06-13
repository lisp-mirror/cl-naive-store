(in-package :common-lisp-user)

(defpackage :cl-naive-indexed
  (:use :cl :cl-naive-store)
  (:export

   :*average-collection-size*
   :*average-value-index-size*
   :*do-partial-indexing*
   :*use-hashtable-for-value-indexing*
   :indexed-collection-mixin   
   :uuid-index
   :key-value-index
   
   :indexes
   
   :hash

   :index-values
   :index-lookup-values
   :index-lookup-uuid
   :add-index
   :remove-index))
