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

   :parse-to-plist
   
   :item
   :make-item
   :item-hash
   :item-bucket-key
   :item-values
   :item-changes
   :item-versions
   :item-collection
   :item-data-type
   :item-store
   :item-peristed-p
   

   :dig
   :digx
   :exists-p
   :getx
   :naive-dig
   :set-naive-dig
   :getfx
   :getsfx
   :validate-sfx
   
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


   :find-item-by-value

   :match-item
   :match-replace-item
   
   :find-equalp-item
   :find-in-item-list 
   :find-items-in-item-list

   ))
