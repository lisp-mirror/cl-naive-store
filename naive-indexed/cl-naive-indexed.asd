(defsystem "cl-naive-indexed"
  :description "Adds simple indexing to cl-naive-store"
  :version "2019.9.14"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store")
  :components ((:file "packages")
	       (:file "naive-indexed" :depends-on ("packages"))))
