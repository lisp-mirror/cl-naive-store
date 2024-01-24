(defsystem "cl-naive-store.tests"
  ;; System attributes:
  :description "Tests the cl-naive-store system."
  :author      "Phil Marneweck"
  :licence     "MIT"
  ;; Component attributes:
  :version "2023.12.19"
  :depends-on ("alexandria"
               "cl-getx"
               "cl-naive-tests"
               "cl-naive-store")
  :components
  ((:file "tests/packages")
   (:file "tests/common" :depends-on ("tests/packages"))
   (:file "tests/test-impl" :depends-on ("tests/common"))
   (:file "tests/test-definitions" :depends-on ("tests/common"))
   (:file "tests/test-basic" :depends-on ("tests/common"))
   (:file "tests/test-basic-persisted" :depends-on ("tests/common"))
   (:file "tests/test-indexed" :depends-on ("tests/common"))
   (:file "tests/test-naive-documents" :depends-on ("tests/common"))
   (:file "tests/test-sharding" :depends-on ("tests/common"))
   (:file "tests/test-sharding-indexed" :depends-on ("tests/common"))))
