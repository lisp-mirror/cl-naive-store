(defsystem "cl-naive-store.naive-documents"
  :description "Adds complex document functionality to cl-naive-store.core."
  :version "2023.12.19"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-core"
               "cl-naive-store.naive-indexed"
               "cl-naive-store.document-types")
  :components
  ((:file "src/naive-documents/packages")
   (:file "src/naive-documents/documents-impl/package"
    :depends-on ("src/naive-documents/packages"))
   (:file "src/naive-documents/naive-documents"
    :depends-on ("src/naive-documents/documents-impl/package"))
   (:file "src/naive-documents/documents-impl/parse-document"
    :depends-on ("src/naive-documents/naive-documents"))
   (:file "src/naive-documents/documents-impl/persist-document"
    :depends-on ("src/naive-documents/naive-documents"))
   (:file "src/naive-documents/document-types"
    :depends-on ("src/naive-documents/naive-documents"))
   (:file "src/naive-documents/documents"
    :depends-on ("src/naive-documents/packages"))
   (:file "src/naive-documents/indexed"
    :depends-on ("src/naive-documents/documents"))
   (:file "src/naive-documents/query"
    :depends-on ("src/naive-documents/indexed"))
   (:file "src/naive-documents/export"
    :depends-on ("src/naive-documents/indexed"))
   (:file "src/naive-documents/export-csv"
    :depends-on ("src/naive-documents/export"))
   (:file "src/naive-documents/export-json"
    :depends-on ("src/naive-documents/export"))))
