(defsystem "cl-naive-data-types"
  :description "Adds data types to cl-naive-store."
  :version "2019.11.10"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store")
  :components ((:file "packages")
	       (:file "data-types" :depends-on ("packages"))))
