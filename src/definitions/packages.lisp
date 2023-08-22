(in-package :common-lisp-user)

(defpackage :cl-naive-store.definitions
  (:use :cl :cl-getx)
  (:export
   :find-named-elements
   :find-named-element
   :get-referenced-types
   :get-collections-dependencies
   :add-collection
   :add-document-type
   :add-store
   :add-universe
   :remove-collection
   :remove-document-type
   :remove-store
   :remove-universe
   :create-multiverse))
