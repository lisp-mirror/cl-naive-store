(defsystem "cl-naive-indexed"
  :description "Adds simple indexing to cl-naive-store"
  :version "2020.06.13"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-murmurhash")
  :components ((:file "packages")
	       (:file "avl-tree")
	       (:file "naive-indexed" :depends-on ("packages"))))
