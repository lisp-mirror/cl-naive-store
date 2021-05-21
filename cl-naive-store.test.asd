(defsystem "cl-naive-store.test"
  ;; System attributes:
  :description "Tests the cl-naive-store system."
  :author      "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence     "MIT"
  ;; Component attributes:
  :version "2021.5.18"
  :depends-on ("alexandria"
               "cl-naive-store")
  :components
  ((:file "tests/packages")
   (:file "tests/tests" :depends-on ("tests/packages")))
  #+asdf-unicode :encoding
  #+asdf-unicode :utf-8
  #+asdf3 :perform #+asdf3 (asdf:test-op
                            (operation system)
                            (uiop:symbol-call "CL-NAIVE-STORE.TESTS" "TEST-ALL"
					      #|monster-size|# 1000)))
