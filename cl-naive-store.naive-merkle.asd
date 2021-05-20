(defsystem "cl-naive-store.naive-merkle"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2021.5.18"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-documents")
  :components
  ((:file "src/naive-merkle/package")
   (:file "src/naive-merkle/merkle" :depends-on ("src/naive-merkle/package"))))
