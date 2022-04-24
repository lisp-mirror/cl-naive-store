(defsystem "cl-naive-store.document-type-defs"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2021.5.18"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on (:cl-naive-store.naive-core
               :cl-naive-store.document-types
               :cl-naive-store.document-types)
  :components
  ((:file "src/document-type-defs/packages")
   (:file "src/document-type-defs/document-type-defs"
    :depends-on ("src/document-type-defs/packages"))))
