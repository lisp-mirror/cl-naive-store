(in-package :common-lisp-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cl-naive-store-api-packages*
    '(
      :cl-naive-store.definitions
      :cl-naive-store.document-types
      :cl-naive-store.naive-core
      :cl-naive-store.naive-documents
      :cl-naive-store.naive-indexed
      :cl-naive-merkle
      )
    "List of packages re-exported from :cl-naive-store.
Matches the :depends-on components in the :cl-naive-store system."))

(defpackage :cl-naive-store
  (:use :cl . #.*cl-naive-store-api-packages*)
  (:export  . #.(loop :for package :in *cl-naive-store-api-packages*
                      :append (let ((names '()))
                                (do-external-symbols (symbol package)
                                  (push (symbol-name symbol) names))
                                names))))

