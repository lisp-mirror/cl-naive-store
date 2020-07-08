(defsystem "cl-naive-data-type-defs"
  :description "Add on for cl-naive-store that supplies some predefined field data types and methods for getting, setting and validating them."
  :version "2020.07.08"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store")
  :components ((:file "packages")
	       (:file "data-type-defs" :depends-on ("packages"))))
