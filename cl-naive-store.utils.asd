(defsystem "cl-naive-store.utils"
  :description "Utilities to make working with cl-naive-store easier."
  :version "2022.3.6"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-core"
               "cl-naive-store.naive-indexed"
               "cl-naive-store.document-types")
  :components
  ((:file "src/utils/package")
   (:file "src/utils/utils"
    :depends-on ("src/utils/package"))))
