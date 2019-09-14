(defsystem "cl-naive-items"
  :description "An extension of cl-naive-store that uses a structure called item as the data objects."
  :version "2019.9.14"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-naive-indexed" "cl-naive-data-types" "cl-naive-data-type-defs")
  :components ((:file "packages")
	       (:file "common" :depends-on ("packages"))
	       
               (:file "naive-items" :depends-on ("common"))
	       (:file "data-types" :depends-on ("naive-items"))
	       (:file "objects" :depends-on ("data-types"))
	       (:file "indexed" :depends-on ("objects"))
	       (:file "parse" :depends-on ("indexed"))
	       (:file "persist" :depends-on ("indexed"))
	       (:file "query" :depends-on ("indexed"))
	       (:file "export" :depends-on ("indexed"))
	       (:file "export-csv" :depends-on ("export"))
	       (:file "export-json" :depends-on ("export"))))
