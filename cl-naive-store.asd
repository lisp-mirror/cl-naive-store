(defsystem "cl-naive-store"
  :description "A naive db that perists data as proper lists."
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad" "split-sequence" "uuid")
  :components ((:file "packages")
	       (:file "common" :depends-on ("packages"))
               (:file "naive-core" :depends-on ("common" "packages"))
	       (:file "blob" :depends-on ("naive-core"))
	       (:file "parse" :depends-on ("blob"))
	       (:file "load" :depends-on ("parse"))
	       (:file "query" :depends-on ("naive-core"))))

