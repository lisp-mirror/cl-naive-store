;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core))
(in-package :naive-examples)

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
                              :collection-class 'collection)))

       ;;Create a collection and add it to the store
       (collection (add-multiverse-element
                    store
                    (make-instance 'collection
                                   :name "simple-collection"
                                   ;;Specifying the key element, else its :key
                                   :keys '(:id)))))

  ;;Add some documents to the collection
  (persist-document collection (list :name "Piet" :surname "Gieter" :id 123))
  (persist-document collection (list :name "Sannie" :surname "Gieter" :id 321))
  (persist-document collection (list :name "Koos" :surname "Van" :id 999))

  ;;Clear the collection, ie unload documents from memory so we can
  ;;show that it has been persisted.
  (clear-collection collection)

  ;;Query the collection, query-data will load the data from file if
  ;;the collection is empty
  (query-data collection :query (lambda (document)
                                  (<= (getx document :id) 900))))

