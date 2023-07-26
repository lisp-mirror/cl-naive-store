(in-package :common-lisp-user)

(defpackage :cl-naive-store.naive-documents
  (:use
   :cl
   :cl-getx
   :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed
   :cl-naive-store.document-types
   #|:cl-naive-store.document-type-defs|#)
  (:export

   ;;Items
   :document-collection
   :document-store

   :document-collection
   :document
   :make-document
   :document-hash
   :document-document-type
   :document-elements
   :document-changes
   :document-versions
   :document-collection
   :document-store
   :document-deleted-p
   :document-p

   :document-values

   :document-of-type-p
   :getxo
   :getxn

   :find-equalp-document

   ;;####Export
   :documents-to-plist
   :document-list-to-csv
   :document-list-to-json))
