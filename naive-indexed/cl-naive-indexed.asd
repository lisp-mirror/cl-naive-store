(defsystem "cl-naive-indexed"
  :description "Adds simple indexing to cl-naive-store"
  :version "2019.11.10"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-murmurhash")
  :components ((:file "packages")
	       (:file "avl-tree")
	       (:file "naive-indexed" :depends-on ("packages"))))
