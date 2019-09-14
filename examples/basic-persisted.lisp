;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-naive-store))
(in-package :naive-examples)

;;Create a universe
(defparameter *universe* (make-instance 
			  'universe
			  :location "~/data-universe/"
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
				       :name "simple-collection"))))
      
  ;;Add some objects to the collection
  (persist-object collection (list :name "Piet" :surname "Gieter" :id 123))
  (persist-object collection (list :name "Sannie" :surname "Gieter" :id 321))

  ;;Clear the collection
  (setf (data-objects collection) nil)

  ;;Query the collection, query-data will load the data from file if the collection is empty
  (query-data collection :query (lambda (data-object)				    
				  (<= (getx data-object :id) 900))))

