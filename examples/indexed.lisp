;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core
                             :cl-naive-store.naive-indexed))
(in-package :naive-examples)

;;Create a class that inherits form indexed-collection-mixin and collection.
(defclass indexded-collection (indexed-collection-mixin collection)
  ())

(defparameter *multiverse*
  (make-instance
   'multiverse
   :location "~/multiverse/" ;Setting the location on disk.
   :universe-class 'universe))

;;Create a universe
(defparameter *universe*
  (make-instance
   'universe
   :multiverse *multiverse*
   :location "~/multiverse/universe/" ;Setting the location on disk.
   :store-class 'store))

(let* (;;Create a store and add it to the universe
       (store (add-multiverse-element *universe*
                                      (make-instance 'store
                                                     :name "simple-store"
                                                     :collection-class 'collection)
                                      :persist-p t))

       ;;Create a collection and add it to the store
       (collection
         (add-muLtiverse-element store
                                 (make-instance 'indexded-collection
                                                :name "simple-collection"
                                                ;;Specifying the key element, else its :key
                                                :keys '(:id)
                                                ;;Specifying the elements to set up indexes for.
                                                :indexes '((:name :surname)))
                                 :persist-p t))
       (results))

  ;;Add some documents to the collection

  (persist-document collection (list :name "Piet" :surname "Gieter" :id 123))
  (persist-document collection (list :name "Sannie" :surname "Gieter" :id 321))
  (persist-document collection (list :name "Koos" :surname "Van" :id 999))
  (persist-document collection (list :name "Frikkie" :surname "Frikkedel" :id 1001))
  (persist-document collection (list :name "Tannie" :surname "Frikkedel" :id 1001))

  ;;Lookup koos using index values and add it to results
  (push
   (index-lookup-values collection (list (list :name "Koos")
                                         (list :surname "Van")))
   results)

  ;;Lookup Frikkedel using index values and add it to results
  (push
   (index-lookup-values collection (list :surname "Frikkedel"))
   results)

  ;;Query the collection, query-data will load the data from file if the collection is empty,
  ;;and add it to the results
  (push (query-data collection :query (lambda (document)
                                        (<= (getx document :id) 900)))
        results)

  (reverse results))
