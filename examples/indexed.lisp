;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core
                             :cl-naive-store.naive-indexed))
(in-package :naive-examples)

;;Create a class that inherits form indexed-collection-mixin and collection.
(defclass indexded-collection (indexed-collection-mixin collection)
  ())

(let* (;;Create a multiverse.
       (multiverse (make-instance
                    'multiverse
                    :name "multiverse"
                    :location "~/multiverse/" ;Setting the location on disk.
                    :universe-class 'universe))
       ;;Create a universe and add it to the multiverse
       (universe (add-multiverse-element
                  multiverse
                  (make-instance
                   'universe
                   :name "universe"
                   :multiverse multiverse
                   :location "~/multiverse/universe/" ;Setting the location on disk.
                   :store-class 'store)))
       ;;Create a store and add it to the universe
       (store (add-multiverse-element
               universe
               (make-instance 'store
                              :name "simple-store"
                              :collection-class 'indexed-collection)))

       ;;Create a collection and add it to the store
       (collection (add-multiverse-element
                    store
                    (make-instance 'collection
                                   :name "simple-collection"
                                   ;;Specifying the key element, else its :key
                                   :keys '(:id))))
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
