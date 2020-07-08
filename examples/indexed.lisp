;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-naive-store :cl-naive-indexed))
(in-package :naive-examples)

;;Create a class that inherits form indexed-collection-mixin and collection.
(defclass indexded-collection (indexed-collection-mixin collection)
  ())

;;Create a universe
(defparameter *universe* (make-instance 
			  'universe
			  :location "~/data-universe/" ;Setting the location on disk.
			  :store-class 'store))


(let* (;;Create a store and add it to the universe
       (store (add-store *universe*
			 (make-instance 'store
					:name "simple-store"
       					:collection-class 'collection)))
       
       ;;Create a collection and add it to the store
       (collection 
	(add-collection store
			(make-instance 'indexded-collection
				       :name "simple-collection"
				       ;;Specifying the key field, else its :key
				       :keys '(:id)
				       ;;Specifying the fields to set up indexes for.
				       :indexes '((:name :surname)))))
       (results))
      
  ;;Add some objects to the collection
  
  
  (persist-object collection (list :name "Piet" :surname "Gieter" :id 123))
  (persist-object collection (list :name "Sannie" :surname "Gieter" :id 321))
  (persist-object collection (list :name "Koos" :surname "Van" :id 999))
  (persist-object collection (list :name "Frikkie" :surname "Frikkedel" :id 1001))
  (persist-object collection (list :name "Tannie" :surname "Frikkedel" :id 1001))

  ;;Lookup koos using index values and add it to results
  (push
   (index-lookup-values collection (list (list :name "Koos" )
					 (list :surname "Van")))
   results)

  ;;Lookup Frikkedel using index values and add it to results
  (push
   (index-lookup-values collection (list :surname "Frikkedel"))
   results)

  ;;Query the collection, query-data will load the data from file if the collection is empty,
  ;;and add it to the results
  (push (query-data collection :query (lambda (data-object)				    
					(<= (getx data-object :id) 900)))
	results)

  (reverse results))
