(defsystem "cl-naive-store.naive-core"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2021.4.19"
  :author "Phil Marneweck"
  :licence "MIT"
  ;;TODO: add feature to conditional depend on UUID and cl-murmurhash...really?
  :depends-on ("cl-fad"
               "split-sequence" "uuid" "local-time"
               "cl-getx" "cl-murmurhash" "ironclad"
               "lparallel" "cl-cpus"
	       "bordeaux-threads")
  :components
  ((:file "src/naive-task-pool/package")
   (:file "src/naive-task-pool/task-pool"               :depends-on ("src/naive-task-pool/package"))


   (:file "src/naive-store/packages"                   	:depends-on ())
 ;  (:file "src/naive-store/threads/packages"         	:depends-on ())
 ;;  (:file "src/naive-store/threads/monitor"            	:depends-on ("src/naive-store/threads/packages"))
  ;; (:file "src/naive-store/io-server/packages"         	:depends-on ("src/naive-store/threads/packages"
								     "src/naive-store/naive-impl/package"))
  ;; (:file "src/naive-store/io-server/io-server"        	:depends-on ("src/naive-store/io-server/packages"
								     "src/naive-store/threads/monitor"
								     "src/naive-store/naive-core"
								     "src/naive-store/naive-impl/files"))

   (:file "src/naive-store/naive-impl/package"          :depends-on ("src/naive-store/packages"))
   (:file "src/naive-store/naive-impl/common"           :depends-on ("src/naive-store/naive-impl/package"
                                                                     "src/naive-task-pool/task-pool"))
   (:file "src/naive-store/naive-impl/files"            :depends-on ("src/naive-store/naive-impl/common"))
   (:file "src/naive-store/naive-impl/logs"             :depends-on ("src/naive-store/naive-impl/files"))
   (:file "src/naive-store/naive-core"                  :depends-on ("src/naive-store/naive-impl/logs"
								     "src/naive-store/io-server/packages"))
   (:file "src/naive-store/documents"                   :depends-on ("src/naive-store/naive-core"))
   (:file "src/naive-store/blob"                        :depends-on ("src/naive-store/documents"))
   (:file "src/naive-store/naive-impl/naive-core"       :depends-on ("src/naive-store/naive-impl/common"))
   (:file "src/naive-store/naive-impl/parse-document"   :depends-on ("src/naive-store/naive-impl/files"))
   (:file "src/naive-store/naive-impl/persist-document" :depends-on ("src/naive-store/naive-impl/files"))
   (:file "src/naive-store/load"                        :depends-on ("src/naive-store/naive-impl/parse-document"))
   (:file "src/naive-store/query"                       :depends-on ("src/naive-store/naive-core"))
   (:file "src/naive-store/maintenance"                 :depends-on ("src/naive-store/naive-impl/persist-document")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "cl-naive-store.test"))))
