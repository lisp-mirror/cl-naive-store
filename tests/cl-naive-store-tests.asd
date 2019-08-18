(defsystem "cl-naive-store-tests"
  :description "Simple tests for cl-naive-store"
  :version "2019.08.19"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-fad" "cl-naive-store"
			"cl-naive-indexed"
			"cl-naive-data-types"
			"cl-naive-data-type-defs"
			"cl-naive-items")
  :components ((:file "packages")
	       (:file "tests" :depends-on ("packages"))))

