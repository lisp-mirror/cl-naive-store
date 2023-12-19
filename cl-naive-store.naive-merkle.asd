(defsystem "cl-naive-store.naive-merkle"
  :description "This needs to be striped out and moved to a branch."
  :version "2021.5.18"
  :author "Phil Marneweck"
  :licence "MIT"
  :depends-on ("cl-naive-store.naive-documents")
  :components
  ((:file "src/naive-merkle/package")
   (:file "src/naive-merkle/merkle" :depends-on ("src/naive-merkle/package"))))
