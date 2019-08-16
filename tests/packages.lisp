(in-package :common-lisp-user)

(defpackage :cl-naive-store-tests
  (:use :cl :cl-naive-store :cl-naive-indexed)
  (:export
   :*universe*
   :test-simple
   :test-lazy-loading
   :test-delete
   :test-all
   :test-passed-p
   ))
