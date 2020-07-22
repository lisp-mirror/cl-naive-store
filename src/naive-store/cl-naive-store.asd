(defsystem "cl-naive-store"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2020.7.16"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad" "split-sequence" "uuid" "local-time" "cl-getx")
  :components ((:file "packages")
	       (:file "naive-impl/package")
	       (:file "naive-impl/common" :depends-on ("naive-impl/package"))
	       (:file "naive-impl/write-object" :depends-on ("naive-impl/common"))
	       (:file "naive-impl/files" :depends-on ("naive-impl/write-object"))
	       (:file "naive-impl/logs" :depends-on ("naive-impl/files"))               
	       (:file "naive-core" :depends-on ("naive-impl/logs"))
	       (:file "documents" :depends-on ("naive-core"))
	       (:file "blob" :depends-on ("documents"))
	       (:file "naive-impl/naive-core" :depends-on ("naive-impl/common"))
	       (:file "naive-impl/parse-document" :depends-on ("naive-impl/files"))
	       (:file "naive-impl/persist-document" :depends-on ("naive-impl/files"))
	       (:file "load" :depends-on ("naive-impl/parse-document"))
	       (:file "query" :depends-on ("naive-core"))              
	       (:file "maintenance" :depends-on ("naive-impl/persist-document"))


))

