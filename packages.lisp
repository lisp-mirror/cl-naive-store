(in-package :common-lisp-user)

(defpackage :cl-naive-store
  (:use :cl)
  (:export

   ;;####common.lisp
   :frmt
   :empty-p
   :trim-whitespace
   :plist-to-values
   :plist-to-value-pairs
   :read-file-to-string
   :write-object
   :write-to-file
   :write-list-to-file
   :persist
   :persist-object
   :dig
   :getx
   :digx
   :exists-p
   :handle-duplicates-p
   :push-replace
   
   ;;####naive-core.lisp
   
   ;;##classes
   :universe
   :collection
   :store
  :name
   :location
   :data-objects
   :handle-duplicates
   :loaded-p

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
   :add-data-object
   :object-values
   :remove-data-object
   :object-values
   :deleted-p
   :delete-data-object
   :get-store
   :get-data-type
   :get-collection
   :find-collection-definitions
   :must-handle-duplicates

   ;;:persist
   :add-store
   :add-collection
   :add-data-type
   
   
   ;;####blob.lisp
   :blob
   :make-blob
   :blob-file-type
   :blob-file-ext
   :blob-location
   :blob-raw
   :blob-string-value
   :blob-p
   
   :blob-ref-p
   ;;:blob-ref-values
   :read-blob
   :write-blob

   ;;####parse.lisp
   :load-object-reference-collection
   :parse-object-deleted-p
   :parse-object-p
   :parse-reference-object-p
   :parse-top-level-data-object
   :parse-reference-data-object
   :parse-child-data-object
   :parse-data-object
  
   ;;####load.lisp
   :load-data
   :load-store
   :load-stores
   :sanitize-data-file


   ;;####query.lisp
   :naive-reduce
   :query-data
   :query-data-object
   
  
   ))
