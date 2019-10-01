(in-package :common-lisp-user)

(defpackage :cl-naive-data-types
  (:use :cl :cl-naive-store)
  (:export
   :field
   :name
   :type-def
   :key-p
   :attributes

   :data-type
   :store
   ;;:name
   :field-class
   :label
   :top-level-p
   :fields

   :data-type-collection-mixin
   ;;:data-type
   
   :data-type-store-mixin
   :data-type-class
   :data-types

   :get-data-type-from-def
   :get-data-type
   :add-data-type))
