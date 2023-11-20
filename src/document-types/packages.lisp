(in-package :common-lisp-user)

(defpackage :cl-naive-store.document-types
  (:use :cl :cl-naive-store.naive-core :cl-getx)
  (:export

   :element
   :name
   :document-type
   :key-p
   :attributes

   :document-type
   :store
   ;;:name
   :element-class
   :label
   :elements

   :document-type-collection-mixin
   ;;:document-type

   :document-type-store-mixin
   :document-type-class
   :document-types

   :document-of-type-p
   :get-element
   :get-attribute)

  (:export ;; deprecated
   :add-document-type
   :get-document-type
   :get-document-type-from-def))

