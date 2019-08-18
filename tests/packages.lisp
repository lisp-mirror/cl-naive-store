(in-package :common-lisp-user)

(defpackage :cl-naive-store-tests
  (:use :cl :cl-naive-store
	:cl-naive-indexed
	:cl-naive-data-types
	:cl-naive-data-type-defs
	:cl-naive-items)
  (:export
   :*universe*
   :test-simple
   :test-lazy-loading
   :test-delete
   :test-all
   :test-passed-p
   ))
