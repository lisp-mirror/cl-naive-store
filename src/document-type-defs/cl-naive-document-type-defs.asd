(defsystem "cl-naive-document-type-defs"
  :description "Add on for cl-naive-store that supplies some predefined element data types and methods for getting, setting and validating them."
  :version "2020.7.16"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store")
  :components ((:file "packages")
	       (:file "document-type-defs" :depends-on ("packages"))))
