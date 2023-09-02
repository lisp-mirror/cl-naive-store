(in-package :common-lisp-user)

(defpackage :cl-naive-store.definitions
  (:use :cl :cl-getx)
  (:export
   :find-named-elements
   :find-named-element
   :get-referenced-types
   :get-collections-dependencies
   :add-definition-element
   :remove-defintion-element
   :create-multiverse))
