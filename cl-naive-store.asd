(defsystem "cl-naive-store"
  :description "A naive db that perists data as proper lists."
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("split-sequence")
  :components ((:file "packages")
               (:file "naive-store" :depends-on ("packages"))
	       (:file "data-type-fields" :depends-on ("naive-store"))))

