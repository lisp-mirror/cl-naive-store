(in-package :common-lisp-user)

(defpackage :cl-naive-store.tests
  (:use
   :cl
   :cl-getx
   :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed
   :cl-naive-store.document-types
   :cl-naive-store.naive-documents)
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

   :test-passed-p))
