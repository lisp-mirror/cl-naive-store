(defsystem "cl-naive-documents"
  :description "An extension of cl-naive-store that uses a structure called document as the data documents."
  :version "2020.7.16"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("cl-murmurhash"
	       "cl-getx"
	       "cl-naive-store"
	       "cl-naive-indexed"
	       "cl-naive-document-types"
	       "cl-naive-document-type-defs")
  :components ((:file "packages")
	       (:file "documents-impl/package" :depends-on ("packages"))
	       (:file "naive-documents" :depends-on ("documents-impl/package"))
	       (:file "documents-impl/parse-document" :depends-on ("naive-documents"))
	       (:file "documents-impl/persist-document" :depends-on ("naive-documents"))
	       (:file "document-types" :depends-on ("naive-documents"))
	       (:file "documents" :depends-on ("document-types"))
	       (:file "indexed" :depends-on ("documents"))
	       (:file "query" :depends-on ("indexed"))
	       (:file "export" :depends-on ("indexed"))
	       (:file "export-csv" :depends-on ("export"))
	       (:file "export-json" :depends-on ("export"))))
