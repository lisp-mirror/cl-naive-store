(defsystem "cl-naive-store.utils"
  :description "Utilities to make working with cl-naive-store easier. Still very experimental and needs clean up."
  :version "2023.12.19"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-core"
               "cl-naive-store.naive-indexed"
               "cl-naive-store.naive-documents"
               "cl-naive-store.document-types")
  :components
  ((:file "src/utils/package")
   (:file "src/utils/utils"
    :depends-on ("src/utils/package"))))
