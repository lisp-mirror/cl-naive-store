(defsystem "cl-naive-items"
  :description "An extension of cl-naive-store that uses a structure called item as the data objects."
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-naive-store" "cl-naive-data-types")
  :components ((:file "packages")
	       (:file "common" :depends-on ("packages"))
	       
               (:file "naive-items" :depends-on ("common"))
	       (:file "data-types" :depends-on ("naive-items"))
	       (:file "parse" :depends-on ("naive-items"))
	       (:file "persist" :depends-on ("naive-items"))
	       (:file "query" :depends-on ("naive-items"))
	       (:file "export" :depends-on ("naive-items"))
	       (:file "export-csv" :depends-on ("export"))
	       (:file "export-json" :depends-on ("export"))))
