(defsystem "cl-naive-store"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2019.11.10"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad" "split-sequence" "uuid" "local-time")
  :components ((:file "packages")
	       (:file "common" :depends-on ("packages"))
	       (:file "persist" :depends-on ("common"))
               (:file "naive-core" :depends-on ("persist"))
	       (:file "objects" :depends-on ("naive-core"))
	       (:file "blob" :depends-on ("naive-core"))
	       (:file "parse" :depends-on ("blob"))
	       (:file "load" :depends-on ("parse"))
	       (:file "query" :depends-on ("naive-core"))
	       (:file "maintenance" :depends-on ("naive-core"))))

