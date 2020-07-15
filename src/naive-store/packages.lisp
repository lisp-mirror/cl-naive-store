(in-package :common-lisp-user)

(defpackage :cl-naive-store
  (:use :cl :cl-getx)
  (:export

   ;;####common.lisp
   :frmt ;;Exposing naive-impl:frmt
   :empty-p ;;Exposing naive-impl:empty-p
   
   :persist
   :persist-document
   
   ;;####persist.lisp
   :persist
   
   ;;####naive-core.lisp
   
   ;;##classes
   :universe
   :collection
   :store
   :name
   :location
   :documents
   :keys
   
   ;;:store
   :universe
   ;;:name
   :collection-class
   :collections
   ;;:location
   
   ;;:universe
   :stores
   ;;:location
   :store-class

   ;;##methods
   :key-values
   :existing-document
   :add-document
   :document-values
   :remove-document
   :document-values
   :deleted-p
   :delete-document
   :get-store
   :get-document-type
   :get-collection
   :find-collection-definitions
   :get-store-from-def
   :get-collection-from-def
   
   
   ;;adding
   :add-store
   :add-collection
   :add-document-type
   
   ;;:persist
   :persist-collection-def
   :persist-collection

   ;;;loading
   :ensure-location
   :collection-container-loaded-p
   :load-data   
   :data-loaded-p
   
   ;;####blob.lisp
   :blob
   :make-blob
   :blob-file-type
   :blob-file-ext
   :blob-location
   :blob-raw
   :blob-parent-hash
   :blob-parent-key
   :blob-string-value
   
   :blob-p
   
   :blob-ref-p
   ;;:blob-ref-values
   :read-blob
   :write-blob

   ;;####load.lisp
   :load-store
   :load-stores
   :sanitize-data-file

   ;;####query.lisp
   :naive-reduce
   :query-data
   :query-document

   
  
   ))
