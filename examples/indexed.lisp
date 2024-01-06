(ignore-errors (delete-package :naive-examples))

;;Load to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core
                             :cl-naive-store.naive-indexed))
(in-package :naive-examples)

;;Required to correctly initialize lparallel:*kernel*.
(initialize)

;;Deleting existing example database
(cl-fad:delete-directory-and-files
 "~/multiverse/universe/simple-store"
 :if-does-not-exist :ignore)

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
                    (make-instance 'indexed-collection
                                   :name "simple-collection"
                                   ;;Specifying the key element, else its :key
                                   :keys '(:id)
                                   ;; Specifying the elements to set up indexes for.
                                   :indexes '((:name :surname)))))
       (results)
       ;;Add doc to collection
       (doc (persist-document collection (list :name "Piet" :surname "Gieter" :id 123))))

  ;;Load Collection if it was created before.
  ;; (load-data collection)

  ;;Add some documents to the collection

  (persist-document collection (list :name "Sannie" :surname "Gieter" :id 321))
  (persist-document collection (list :name "Koos" :surname "Van" :id 999))
  (persist-document collection (list :name "Frikkie" :surname "Frikkedel" :id 1001))
  (persist-document collection (list :name "Tannie" :surname "Frikkedel" :id 1002))

  ;;Look up piet by hash
  (push (list :desc "Looked up Piet using index-lookup-hash."
              :value (index-lookup-hash collection (getx doc :hash)))
        results)

  ;;Lookup koos using index values and add it to results
  (push (list :desc "Koos that we looked up using index-lookup-values and the index values of Koos and Van."
              :value
              (index-lookup-values collection (list (list :name "Koos")
                                                    (list :surname "Van"))))
        results)

  ;;Lookup Frikkedel using index values and add it to results
  (push
   (list
    :desc "A list of both Frikie and Tannie that we looked up using index-lookup-values and the surname. This is called a partial index lookup. You can enable or disable partial indexes."
    :value
    (index-lookup-values collection (list :surname "Frikkedel")))

   results)

  ;;Query the collection, query-data
  ;;will load the data from file if
  ;;the collection is empty, and add
  ;;it to the results
  (push
   (list
    :desc "Queried all id's <= 900 using query-data. The query will use indexes internally when possible."
    :value
    (query-data collection :query (lambda (document)
                                    (<= (getx document :id) 900))))
   results)

  (reverse results))
