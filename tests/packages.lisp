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
   :test-simple
   :test-lazy-loading
   :test-delete
   :test-all
   :test-passed-p
   ))
