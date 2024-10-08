(in-package :common-lisp-user)

(defpackage :cl-naive-store.naive-indexed
  (:use :cl :cl-naive-store.naive-core :cl-getx)
  (:export
   :*do-partial-indexing*
   :indexed-collection-mixin
   :indexed-collection
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
  (:use :cl :cl-getx :cl-murmurhash :cl-naive-store.naive-core :cl-naive-store.naive-indexed)
  (:export

   :index-values
   :push-value-index
   :remove-value-index))
