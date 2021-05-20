(defsystem "cl-naive-store.document-types"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2021.5.18"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-core")
  :components
  ((:file "src/document-types/packages")
   (:file "src/document-types/document-types"
    :depends-on ("src/document-types/packages"))))

