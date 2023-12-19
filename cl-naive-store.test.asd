(defsystem "cl-naive-store.test"
  ;; System attributes:
  :description "Tests the cl-naive-store system."
  :author      "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence     "MIT"
  ;; Component attributes:
  :version "2021.5.18"
  :depends-on ("alexandria"
               "cl-getx"
               "cl-naive-tests"
               "cl-naive-store")
  :components
  ((:file "tests/packages")
   (:file "tests/common" :depends-on ("tests/packages"))
   (:file "tests/test-definitions" :depends-on ("tests/common"))
   (:file "tests/test-basic" :depends-on ("tests/common"))
   (:file "tests/test-basic-persisted" :depends-on ("tests/common"))
   (:file "tests/test-indexed" :depends-on ("tests/common"))
   (:file "tests/test-naive-documents" :depends-on ("tests/common"))
   (:file "tests/test-sharding" :depends-on ("tests/common"))
   (:file "tests/test-sharding-indexed" :depends-on ("tests/common"))))
