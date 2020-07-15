(in-package :common-lisp-user)

(defpackage :cl-naive-documents
  (:use :cl
	:cl-getx
	:cl-naive-store
	:cl-naive-indexed
	:cl-naive-document-types
	:cl-naive-document-type-defs)
  (:export

   ;;Items
   :document-collection
   :document-store
   
   :document
   :make-document
   :document-hash
   :document-type
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
