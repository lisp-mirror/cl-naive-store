;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core
                             :cl-naive-store.naive-documents))
(in-package :naive-examples)

(defparameter *multiverse*
  (make-instance
   'multiverse
   :name "multiverse"
   :location "~/multiverse/" ;Setting the location on disk.
   :universe-class 'universe))

;;Create a universe
(defparameter *universe*
  (make-instance
   'universe
   :name "universe"
   :multiverse *multiverse*
   :location "~/multiverse/universe/" ;Setting the location on disk.
   :store-class 'store))

(add-multiverse-element *multiverse* *universe*)

(defparameter store (add-multiverse-element *universe*
                                            (make-instance 'document-store
                                                           :name "simple-store"
                                                           :collection-class 'collection)))

(defparameter collection (add-multiverse-element store
                                                 (make-instance 'document-collection
                                                                :name "simple-collection"
                                                                ;;Specifying the key element, else its :key
                                                                :keys '(:id))))

;;(require :sb-sprof)

(break "~S" collection)

(let* (;;Create a store and add it to the universe
       (storex)

       ;;Create a collection and add it to the store
       (collectionx))

  ;;Add some documents to the collection
  (time (dotimes (i 1000000)
          (persist-document collection (list :name "Piet" :surname "Gieter" :id i))))

  ;; (add-document collection (list :name "Sannie" :surname "Gieter" :id 321))
  ;; (add-document collection (list :name "Koos" :surname "Van" :id 999))

  ;;Duplicates are handled by default, so this will not cause a duplicate document
  ;;(add-document collection (list :name "Piet" :surname "Gieter" :id 123))

  (time (cl-naive-store.naive-core::clear-collection collection))
  #|
  (time (flamegraph:save-flame-graph ("~/temp/load.stack")
  (sb-sprof:with-profiling
  (:max-samples 10000
  :report :flat
  :loop nil)
  (load-data collection))))
  |#

  (time (load-data collection))

  ;;Query the collection
  (time
   (query-data collection :query (lambda (document)
                                   (>= (getx document :id) 999999)))))
