(defsystem "cl-naive-store"
  :description "A naive db that perists data as proper lists."
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("split-sequence" "uuid")
  :components ((:file "packages")
	       (:file "common" :depends-on ("packages"))
               (:file "naive-store" :depends-on ("common" "packages"))
	       (:file "export" :depends-on ("naive-store"))
	       (:file "export-csv" :depends-on ("export"))
	       (:file "export-json" :depends-on ("export"))
	       (:file "data-type-fields" :depends-on ("naive-store"))))

