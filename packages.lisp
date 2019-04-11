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
   :dig
   :getx
   :digx
   :exists-p

   ;;####naive-core.lisp
   
   ;;##classes
   :field
   :name
   :type-def
   :key-p
   :attributes

      
   :data-type
   :store
   ;;:name
   :field-class
   :label
   :top-level-p
   :fields  


   :collection
   :store
   ;;:name
   ;;:data-type
   :location
   :data-objects
   :uuid-index
   :key-value-index
   :loaded-p

   ;;:store
   :universe
   ;;:name
   :collection-class
   :data-type-class
   :data-types
   :collections
   ;;:location
   
   ;;:universe
   :stores
   ;;:location
   :store-class

   ;;##methods
   :add-data-object
   :remove-data-object
   :deleted-p
   :delete-data-object
   :hash
   :key-values
   :key-values-hash
   :index-lookup-values-hash
   :index-lookup-uuid
   :add-index
   :remove-index
   :get-store
   :get-data-type
   :get-collection
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
