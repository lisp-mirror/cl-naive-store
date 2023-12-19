(defsystem "cl-naive-store"
  :description "cl-naive-store is a log structured document store. Documents are
loaded in-memory to give facilitate fast querying. Depending on how
you use the store documents will be lazy loaded. indexed, and written
completely in Common Lisp."
  :version "2023.12.19"
  :author "Phil Marneweck"
  :licence "MIT"
  ;;TODO: add feature to conditional depend on UUID and cl-murmurhash...really?
  :depends-on ("cl-naive-store.naive-core"
               "cl-naive-store.document-types"
               "cl-naive-store.naive-documents"
               "cl-naive-store.naive-indexed"
               "cl-naive-store.naive-merkle")
  :components ((:file "src/packages"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "cl-naive-store.test"))))
