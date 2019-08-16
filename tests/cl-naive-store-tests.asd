(defsystem "cl-naive-store-tests"
  :description "Simple tests for cl-naive-store"
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad" "cl-naive-store" "cl-naive-indexed")
  :components ((:file "packages")
	       (:file "tests" :depends-on ("packages"))))

