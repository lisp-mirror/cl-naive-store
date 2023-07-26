(in-package :common-lisp-user)

(defpackage :cl-naive-store.definitions
  (:use :cl :cl-getx :cl-naive-store.naive-core :cl-naive-store.document-types)
  (:export
   :create-multiverse))
