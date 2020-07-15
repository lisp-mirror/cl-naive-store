(defsystem "cl-naive-document-types"
  :description "Adds data types to cl-naive-store."
  :version "2020.7.16"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-getx")
  :components ((:file "packages")
	       (:file "document-types" :depends-on ("packages"))))
