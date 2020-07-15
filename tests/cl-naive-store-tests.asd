(defsystem "cl-naive-store-tests"
  :description "Simple tests for cl-naive-store"
  :version "2020.7.11"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad"
	       "cl-getx"
	       "cl-naive-store"
	       "cl-naive-indexed"
	       "cl-naive-document-types"
	       "cl-naive-document-type-defs"
	       "cl-naive-documents")
  :components ((:file "packages")
	       (:file "tests" :depends-on ("packages"))))

