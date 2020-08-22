;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl
				  :cl-getx :cl-naive-store
				  :cl-naive-indexed
				  :cl-naive-document-types
				  :cl-naive-document-type-defs
				  :cl-naive-documents))
(in-package :naive-examples)

;;Create a data definition for an employee
;;It looks like a lot but dont panic its simple.
(defparameter *employee-document-type*
  '(:name "employee"
    :label "Employee"
    :elements ((:name :emp-no
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
			 (make-instance 'document-store
					:name "simple-store"
       					:collection-class 'collection)))
       (collection)
       (elements)
       (document-type)
       (results))

  ;;initialize the data employee data definition.
  (dolist (element (getf *employee-document-type* :elements))
      (setf
       elements 
       (append elements 
	       (list (make-instance 
		      'element
		      :name (getf element :name)
		      :key-p (getf element :key-p)
		      :type-def (getf element :type-def)
		      :attributes (getf element :attributes))))))

  (setf document-type (add-document-type
		   store
		   (make-instance 
		    'document-type
		    :name (getf *employee-document-type* :name)
		    :label (getf *employee-document-type* :label)                    
		    :elements elements)))

  ;;Create a collection and add it to the store
  (setf collection (add-collection store
			(make-instance 'document-collection ;;using documents collection.
				       :name "simple-collection"
				       :document-type document-type
				       ;;Not specifying the keys to show
				       ;;that they are retrieved from the document-type
				       ;;if if no key is set.
				       ;;:keys ...
				       ;;Specifying the elements to set up indexes for.
				       :indexes '((:name :surname)))))
  ;;Add some documents to the collection
  (persist-document collection
   (make-document 
    :store (store collection)
    :collection collection
    :type-def "employee"		
    :elements (list :name "Piet" :surname "Gieter" :emp-no 123)))

  (persist-document collection
   (make-document 
    :store (store collection)
    :collection collection
    :type-def "employee"		
    :elements (list :name "Sannie" :surname "Gieter" :emp-no 321)))

  (persist-document collection
   (make-document 
    :store (store collection)
    :collection collection
    :type-def "employee"		
    :elements (list :name "Koos" :surname "Van" :emp-no 999)))

  (persist-document collection
   (make-document 
    :store (store collection)
    :collection collection
    :type-def "employee"		
    :elements (list :name "Frikkie" :surname "Frikkedel" :emp-no 1001)))

  (persist-document collection
   (make-document 
    :store (store collection)
    :collection collection
    :type-def "employee"		
    :elements (list :name "Tannie" :surname "Frikkedel" :emp-no 1001)))
  
 
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
  (push (query-data collection :query (lambda (document)				    
					(<= (getx document :emp-no) 900)))
	results)

  (reverse results))



(let ((sannie (first (index-lookup-values (get-collection
					   (get-store *universe* "simple-store")
					   "simple-collection")
					  (list (list :name "Sannie" )
						(list :surname "Gieter"))))))
 
  (setf (getx sannie :surname) "Potgieter")
