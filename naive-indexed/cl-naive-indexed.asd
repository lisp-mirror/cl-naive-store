(defsystem "cl-naive-indexed"
  :description "Adds simple indexing to cl-naive-store"
  :version "2020.07.08"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-murmurhash")
  :components ((:file "packages")
	       (:file "naive-indexed" :depends-on ("packages"))))
