(defsystem "cl-naive-store"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2021.4.19"
  :author "Phil Marneweck"
  :licence "MIT"
  ;;TODO: add feature to conditional depend on UUID and cl-murmurhash...really?
  :depends-on ("cl-naive-store.naive-core"
               "cl-naive-store.document-types"
               "cl-naive-store.document-type-defs"
               "cl-naive-store.naive-documents"
               "cl-naive-store.naive-indexed"
               "cl-naive-store.naive-merkle")
  :components ()
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "cl-naive-store.test"))))
