(in-package :common-lisp-user)

(defpackage :cl-naive-store
  (:use :cl)
  (:export
   
   ;;Classes
   :field
   :name
   :type-def
   :attributes
   
   :bucket
   :collection
   :key-values
   :items
   :location
   
   :collection
   :store
   :name
   :bucket-keys
   :data-type
   :location
   :buckets
   :index
   
   :data-type
   :store
   :name
   :label
   :top-level-p
   :fields
   
   :store
   :universe
   :name
   :data-types
   :collections
   :location
   
   :universe
   :stores
   :location
   
   :item
   :make-item
   :item-hash
   :item-bucket-key
   :item-values
   :changes
   :versions
   :peristed-p
   
   :getx
  
   :write-to-file
   :write-list-to-file   
   :persist
   :add-store
   ;;:persist-collection-def
   :persist-collection
   :add-data-type
   :add-collection

   :get-store
   :get-data-type
   :get-collection
   :get-bucket
   :get-collection-bucket
   :get-key-values
   ;;get-bucket-key-val-location
   :index-keys
   ;;:add-index
   :lookup-index
   ;;:remove-from-index
  
   :persist-item
   
   :load-store-data-types
   :load-store-collections
   :load-collection-items
   
   :load-store
   :fetch-items
   :fetch-item
   
   
   ))
