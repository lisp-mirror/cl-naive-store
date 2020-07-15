(in-package :common-lisp-user)

(defpackage :cl-naive-document-types
  (:use :cl :cl-naive-store :cl-getx)
  (:export
   
   :element
   :name
   :type-def
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

   :get-document-type-from-def
   :get-document-type
   :add-document-type))

