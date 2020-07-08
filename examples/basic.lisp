;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-naive-store))
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
				       ;;Specifying the key field, else its :key
				       :keys '(:id)))))
      
  ;;Add some objects to the collection
  (add-data-object collection (list :name "Piet" :surname "Gieter" :id 123))
  (add-data-object collection (list :name "Sannie" :surname "Gieter" :id 321))
  (add-data-object collection (list :name "Koos" :surname "Van" :id 999))

  ;;Duplicates are handled by default, so this will not cause a duplicate object
  (add-data-object collection (list :name "Piet" :surname "Gieter" :id 123))
  
  ;;Query the collection
  (query-data collection :query (lambda (data-object)				    
				  (<= (getx data-object :id) 900))))
