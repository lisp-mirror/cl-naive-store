(defsystem "cl-naive-store"
  :description "This is a naive, persisted, in memory (lazy loading) data store for Common Lisp."
  :version "2020.8.12"
  :author "Phil Marneweck"
  :licence "MIT"
  ;;TODO: add feature to conditional depend on UUID and cl-murmurhash...really?
  :depends-on ("cl-fad" "split-sequence" "uuid" "local-time" "cl-getx" "cl-murmurhash")
  :components
  ((:module "naive-store"
	    :pathname "../naive-store/"
                        
            :components ( (:file "packages")
			  (:file "naive-impl/package")
			  (:file "naive-impl/common" :depends-on ("naive-impl/package"))
			  (:file "naive-impl/files" :depends-on ("naive-impl/common"))
			  (:file "naive-impl/logs" :depends-on ("naive-impl/files"))               
			  (:file "naive-core" :depends-on ("naive-impl/logs"))
			  (:file "documents" :depends-on ("naive-core"))
			  (:file "blob" :depends-on ("documents"))
			  (:file "naive-impl/naive-core" :depends-on ("naive-impl/common"))
			  (:file "naive-impl/parse-document" :depends-on ("naive-impl/files"))
			  (:file "naive-impl/persist-document" :depends-on ("naive-impl/files"))
			  (:file "load" :depends-on ("naive-impl/parse-document"))
			  (:file "query" :depends-on ("naive-core"))              
			  (:file "maintenance" :depends-on ("naive-impl/persist-document"))))
   
   ;;Having to use depreciated #+ reader and not :feature or :if-feature because
   ;;asdf requires "all the component classes to defined" ...see Conditional Code in
   ;;best practices documentation.
   #+(or :naive-indexed
	 :naive-documents
	 (and (not :naive-store)
	      (not :naive-indexed)
	      (not :naive-document-types)
	      (not :naive-document-type-defs)
	      (not :naive-documents)))
   (:module "naive-indexed"
	    :pathname "../naive-indexed/"
	    :depends-on ("naive-store")
	    :components ((:file "packages")	       
			 (:file "naive-indexed" :depends-on ("packages"))
			 (:file "indexed-impl" :depends-on ("naive-indexed"))
			 (:file "parse-document" :depends-on ("naive-indexed"))
			 (:file "query" :depends-on ("naive-indexed"))))

   #+(or :naive-document-types
	 :naive-documents
	 (and (not :naive-store)
	      (not :naive-indexed)
	      (not :naive-document-types)
	      (not :naive-document-type-defs)
	      (not :naive-documents)))		
   (:module "document-types"
	    :pathname "../document-types/"
	    :depends-on ("naive-store")
	    :components ((:file "packages")
			 (:file "document-types"
				:depends-on ("packages"))))
   
   #+(or :naive-document-type-defs
	 :naive-documents
	 (and (not :naive-store)
	      (not :naive-indexed)
	      (not :naive-document-types)
	      (not :naive-document-type-defs)
	      (not :naive-documents)))
   (:module "document-type-defs"
	    :pathname "../document-type-defs/"
	    :depends-on ("naive-store")
	    :components ((:file "packages")
			 (:file "document-type-defs"
				:depends-on ("packages"))))
   #+(or :naive-documents
	 (and (not :naive-store)
	      (not :naive-indexed)
	      (not :naive-document-types)
	      (not :naive-document-type-defs)
	      (not :naive-documents)))
   (:module "naive-documents"
	    :pathname "../naive-documents/"
	    :depends-on ("naive-store"
			 "naive-indexed"
			 "document-types"
			 "document-type-defs")
	    :components ((:file "packages" )
			 (:file "documents-impl/package"
				:depends-on ("packages"))
			 (:file "naive-documents" :depends-on ("documents-impl/package"))
			 (:file "documents-impl/parse-document"	:depends-on ("naive-documents"))
			 (:file "documents-impl/persist-document" :depends-on ("naive-documents"))
			 (:file "../naive-documents/document-types" :depends-on ("naive-documents"))
					
			 (:file "documents" :depends-on ("packages"))
			 (:file "indexed" :depends-on ("documents"))
			 (:file "query" :depends-on ("indexed"))
			 (:file "export" :depends-on ("indexed"))
			 (:file "export-csv" :depends-on ("export"))
			 (:file "export-json" :depends-on ("export"))))

   #+:naive-store-tests
   (:module "naive-store-tests"
	    :pathname "../../tests/"
	    :depends-on ("naive-documents")
	    :components ((:file "packages")
			 (:file "tests"
				:depends-on ("packages"))))))



