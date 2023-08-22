(in-package :common-lisp-user)

(defpackage :cl-naive-store.definitions
  (:use :cl :cl-getx :cl-naive-store.naive-core
   :cl-naive-store.document-types)
  (:export
   :find-named-definition-elements
   :find-named-definition-element
   :get-referenced-types
   :get-collections-dependencies
   :add-collection-definition
   :add-document-type-definition
   :add-store-definition
   :add-universe-definition
   :remove-collection-definition
   :remove-document-type-definition
   :remove-store-definition
   :remove-universe-definition
   :create-multiverse))
