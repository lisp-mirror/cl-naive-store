(in-package :common-lisp-user)

(defpackage :cl-naive-store.tests
  (:use
   :cl
   :cl-getx
   :cl-naive-tests
   :cl-naive-store.naive-core)
  (:export
   :get-universe-class
   :get-store-class
   :*multiverse*
   :*universe*
   :*countries*

   :test-location
   :tear-down-test
   :setup-test
   :define-testsuite
   :run-test

   :test-all
   :test-all-examples
   :test-all-simple
   :test-all-simple-documents
   :test-all-simple-indexed
   :test-delete
   :test-lazy-loading
   :test-simple
   :test-simple-duplicates

   :test-passed-p))
