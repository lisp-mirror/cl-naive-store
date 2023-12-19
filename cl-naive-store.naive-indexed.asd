(defsystem "cl-naive-store.naive-indexed"
  :description "Adds indexing functionality to cl-naive-store.core."
  :version "2023.12.19"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-core")
  :components
  ((:file "src/naive-indexed/packages")
   (:file "src/naive-indexed/naive-indexed"
    :depends-on ("src/naive-indexed/packages"))
   (:file "src/naive-indexed/indexed-impl"
    :depends-on ("src/naive-indexed/naive-indexed"))
   (:file "src/naive-indexed/parse-document"
    :depends-on ("src/naive-indexed/naive-indexed"))
   (:file "src/naive-indexed/query"
    :depends-on ("src/naive-indexed/naive-indexed"))))

