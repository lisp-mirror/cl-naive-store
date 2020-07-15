(defsystem "cl-naive-indexed"
  :description "Adds simple indexing to cl-naive-store"
  :version "2020.7.16"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-murmurhash" "cl-getx")
  :components ((:file "packages")
	       
	       (:file "naive-indexed" :depends-on ("packages"))
	       (:file "indexed-impl" :depends-on ("naive-indexed"))
	       (:file "parse-document" :depends-on ("naive-indexed"))
	       (:file "query" :depends-on ("naive-indexed"))
	       ))
