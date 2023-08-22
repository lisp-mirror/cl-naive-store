(defsystem "cl-naive-store.definitions"
  :description "This is a naive, declaritive implementation of the entities that make up a naive store."
  :version "2023.7.22"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on (:cl-getx
               :cl-naive-ptrees
               :cl-naive-store.naive-core
               :cl-naive-store.document-types
               :cl-naive-store.naive-documents)
  :components
  ((:file "src/definitions/packages")
   (:file "src/definitions/definitions"
    :depends-on ("src/definitions/packages"))))
