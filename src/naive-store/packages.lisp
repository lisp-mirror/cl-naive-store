(in-package :common-lisp-user)

(defpackage :cl-naive-store.naive-core
  (:use :cl :cl-getx)
  (:export

   ;;####common.lisp
   :frmt ;;Exposing naive-impl:frmt
   :empty-p ;;Exposing naive-impl:empty-p
   :*disable-parallel-p* ;;Exposing naive-impl:*disable-parallel-p*
   :do-sequence ;;Exposing naive-impl:do-sequence
   :gethash-safe
   :remhash-safe

   :persist
   :persist-document

   ;;####persist.lisp
   :persist

   ;;####naive-core.lisp

   ;;##classes
   :shard
   :universe
   :multiverse
   :collection
   :store
   :name
   :location
   :documents
   :keys

   ;;shard
   :mac
   :short-mac
   :status
   :shards
   :match-shard
   :get-shard
   :make-shard
   :document-shard-mac
   :lock

   ;;:store
   ;;:universe
   ;;:name
   :collection-class
   :collections
   ;;:location

   ;;:universe
   :stores
   ;;:location
   :store-class

   :multiverse
   :universes
   :universe-class

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

   :query-multiverse
   :get-multiverse-element
   :add-multiverse-element
   :remove-multiverse-element
   :get-definitions

   :get-definition

   :add-definition-element
   :remove-definition-element
   :get-definition-element
   :find-named-element
   :definition-body
   :instance-from-definition
   :load-from-definition
   :instance-from-definition-file
   :load-from-definition-file
   :instances-from-definitions
   :load-from-definitions
   :get-store-from-def
   :get-collection-from-def

   ;;adding
   :add-store
   :add-collection
   :add-document-type

   ;;Removing
   :clear-collection
   :remove-collection
   :clear-documents

   ;;:persist
   :persist-collection-def
   :persist-collection

   ;;;loading
   :ensure-location
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
   :query-document))

