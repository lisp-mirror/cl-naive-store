;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl :cl-naive-store
				  :cl-naive-indexed
				  :cl-naive-data-types
				  :cl-naive-data-type-defs
				  :cl-naive-items))
(in-package :naive-examples)

;;Create a data definition for an employee
;;It looks like a lot but dont panic its simple.
(defparameter *employee-data-type*
  '(:name "employee"
    :label "Employee"
    :top-level-p t
    :fields ((:name :emp-no
	      :label "Employee No"
	      :db-type :string
	      :key-p t
	      :attributes (:display t :editable t)) 
	     (:name :name
	      :label "Name"
	      :db-type :string
	      :attributes (:display t :editable t))
	     (:name :surname
	      :label "Surname"
	      :db-type :string
	      :attributes (:display t :editable t)))
    :documentation "This type represents a simple employee master."))

;;Create a universe
(defparameter *universe* (make-instance 
			  'universe
			  :location "~/data-universe/" ;Setting the location on disk.
			  :store-class 'store))


(let* (;;Create a store and add it to the universe
       (store (add-store *universe*
			 (make-instance 'item-store
					:name "simple-store"
       					:collection-class 'collection)))
       
       
       (collection)
       (fields)
       (data-type)
       (results))

  ;;initialize the data employee data definition.
  (dolist (field (getf *employee-data-type* :fields))
      (setf
       fields 
       (append fields 
	       (list (make-instance 
		      'field
		      :name (getf field :name)
		      :key-p (getf field :key-p)
		      :type-def (getf field :type-def)
		      :attributes (getf field :attributes))))))

  (setf data-type (add-data-type
		   store
		   (make-instance 
		    'data-type
		    :name (getf *employee-data-type* :name)
		    :label (getf *employee-data-type* :label)
		    :top-level-p
		    (getf *employee-data-type* :top-level-p)
		    :fields fields)))

  ;;Create a collection and add it to the store
  (setf collection (add-collection store
			(make-instance 'item-collection ;;using items collection.
				       :name "simple-collection"
				       :data-type data-type
				       ;;Specifying the fields to set up indexes for.
				       :indexes '(:name :surname))))
  ;;Add some objects to the collection
  (persist-object collection
   (make-item 
    :store (store collection)
    :collection collection
    :data-type "employee"		
    :values (list :name "Piet" :surname "Gieter" :emp-no 123)))

  (persist-object collection
   (make-item 
    :store (store collection)
    :collection collection
    :data-type "employee"		
    :values (list :name "Sannie" :surname "Gieter" :emp-no 321)))

  (persist-object collection
   (make-item 
    :store (store collection)
    :collection collection
    :data-type "employee"		
    :values (list :name "Koos" :surname "Van" :emp-no 999)))

  (persist-object collection
   (make-item 
    :store (store collection)
    :collection collection
    :data-type "employee"		
    :values (list :name "Frikkie" :surname "Frikkedel" :emp-no 1001)))

  (persist-object collection
   (make-item 
    :store (store collection)
    :collection collection
    :data-type "employee"		
    :values (list :name "Tannie" :surname "Frikkedel" :emp-no 1001)))
  
 
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
					(<= (getx data-object :emp-no) 900)))
	results)

  (reverse results))



(let ((sannie (first (index-lookup-values (get-collection
					   (get-store *universe* "simple-store")
					   "simple-collection")
					  (list (list :name "Sannie" )
						(list :surname "Gieter"))))))
 
  (setf (getx sannie :surname) "Potgieter")

  sannie)
