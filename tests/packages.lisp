(in-package :common-lisp-user)

(defpackage :cl-naive-store-tests
  (:use :cl
	:cl-getx
	:cl-naive-store
	:cl-naive-indexed
	:cl-naive-document-types
	:cl-naive-document-type-defs
	:cl-naive-documents)
  (:export
   :*universe*

   :test-all
   :test-all-examples
   :test-all-simple
   :test-all-simple-documents
   :test-all-simple-indexed
   :test-delete
   :test-lazy-loading
   :test-simple
   :test-simple-duplicates
   
   :test-passed-p
   ))
