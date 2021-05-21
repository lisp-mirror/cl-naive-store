;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-getx :cl-naive-store.naive-core))
(in-package :naive-examples)

;;Create a universe
(defparameter *universe* (make-instance
			  'universe
			  :store-class 'store))

(let* (;;Create a store and add it to the universe
       (store (add-store *universe*
			 (make-instance 'store
					:name "simple-store"
       					:collection-class 'collection)))

       ;;Create a collection and add it to the store
       (collection
	 (add-collection store
			 (make-instance 'collection
					:name "simple-collection"
					;;Specifying the key element, else its :key
					:keys '(:id)))))

  ;;Add some documents to the collection
  (add-document collection (list :name "Piet" :surname "Gieter" :id 123))
  (add-document collection (list :name "Sannie" :surname "Gieter" :id 321))
  (add-document collection (list :name "Koos" :surname "Van" :id 999))

  ;;Duplicates are handled by default, so this will not cause a duplicate document
  (add-document collection (list :name "Piet" :surname "Gieter" :id 123))

  ;;Query the collection
  (query-data collection :query (lambda (document)
				  (<= (getx document :id) 900))))
