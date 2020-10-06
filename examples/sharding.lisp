;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use :cl
				  :cl-getx :cl-naive-store
				  :cl-naive-indexed
				  :cl-naive-document-types
				  :cl-naive-document-type-defs
				  :cl-naive-documents))
(in-package :naive-examples)


;;A helper function
(defun random-from-list (list)
  (nth (random (length list)) list))


;;Create a data definition for an asset. Assets are linked to employees
;;It looks like a lot but dont panic its simple.
(defparameter *asset-document-type*
  '(:name "asset-register"
     :label "Asset Register"
     :elements ((:name :asset-no
		       :label "Asset No"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t)) 
		(:name :description
		       :label "Description"
		       :db-type :string
		       :attributes (:display t :editable t)))
     :documentation "This type represents a simple employee master."))


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
	       (:name :country
		:label "Country"
		:db-type :string
		:attributes (:display t :editable t))
	       (:name :name
		:label "Name"
		:db-type :string
		:attributes (:display t :editable t))
	       (:name :surname
		:label "Surname"
		:db-type :string
		:attributes (:display t :editable t))
	       (:name :asset
		       :label "Asset"
		       :db-type (:type :document
				       :complex-type :document
				       :elements )
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
       (employee-collection)
       (asset-collection)
       (employee-elements)
       (employee-document-type)
       (asset-elements)
       (asset-document-type)
       (results))

  ;;initialize the data employee data definition.
  (dolist (element (getf *employee-document-type* :elements))
    (setf
     employee-elements 
     (append employee-elements 
	     (list (make-instance 
		    'element
		    :name (getf element :name)
		    :key-p (getf element :key-p)
		    :type-def (getf element :type-def)
		    :attributes (getf element :attributes))))))

  (setf employee-document-type (add-document-type
				store
				(make-instance 
				 'document-type
				 :name (getf *employee-document-type* :name)
				 :label (getf *employee-document-type* :label)                    
				 :elements employee-elements)))

 ;;initialize the data asset data definition.
  (dolist (element (getf *asset-document-type* :elements))
    (setf
     asset-elements 
     (append asset-elements 
	     (list (make-instance 
		    'element
		    :name (getf element :name)
		    :key-p (getf element :key-p)
		    :type-def (getf element :type-def)
		    :attributes (getf element :attributes))))))

  (setf asset-document-type (add-document-type
				store
				(make-instance 
				 'document-type
				 :name (getf *asset-document-type* :name)
				 :label (getf *asset-document-type* :label)                    
				 :elements asset-elements)))

  

  ;;Create a collection and add it to the store
  (setf employee-collection
	(add-collection store
			(make-instance 'document-collection ;;using documents collection.
				       :name "simple-collection"
				       :document-type employee-document-type
				       :keys '(:emp-no)
				       :indexes '((:surname))
				       ;;Creating shards based on the country that the employee
				       ;;belongs to. It is a bad example you should not shard on
				       ;;any value that could change!!!!!
				       :shard-elements (list :country))))

  ;;Create a collection and add it to the store
  (setf asset-collection
	(add-collection store
			(make-instance 'document-collection
					 :name "asset-collection"
					 :document-type asset-document-type
					 :keys '(:asset-no))))

  
  ;;Add some documents to the collections
  (let ((emp-country '("Nigeria" "Us" "Canada" "Uk" "Estonia" "Zimbabwe" "Botswanna" "Mosambique"
		       "Argentina" "Mexico" "Chilly" "China" "Russia" "Germany"))        
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))


    ;;Try to load the data first, maybe it has been persisted before.    
    (load-data employee-collection)

    ;;If the data was peristed before and successfully loaded dont add it again.
    (unless (data-loaded-p employee-collection)
      
      ;;Adding documents without persisting will do a bulk persist later which is much faster.
      (print "Adding 200000 documents to collections")
      (time
       (dotimes (x 100000)
	 
	 (add-document employee-collection
		       (make-document 
			:store (store employee-collection)
			:collection employee-collection
			:type-def employee-document-type		
			:elements (list
				   :asset (add-document asset-collection
							(make-document 
							 :store (store asset-collection)
							 :collection asset-collection
							 :type-def asset-document-type		
							 :elements (list :description x :asset-no x)))
				   :country (random-from-list emp-country)                        
				   :surname (random-from-list emp-surnames)
				   :name (format nil "Slave No ~A" x)			   
				   :emp-no x)))
	 
	 )))


    (break "~A ~A" employee-collection asset-collection)
    (print "Persisting 100000 assets to collections")
    (time
     ;;Bulk Persist assets
     (persist asset-collection))

    (print "Persisting 100000 employees to collections")
    (time
     ;;Bulk Persist employees
     (persist employee-collection))

    (print "Doing a straight up query that touches each record.")
    (time
     (push (list :query-all
		 (length (query-data employee-collection :query
				     (let ((size 100000))
				       (lambda (document)
					 
					 (or (and
					      (>= (getx document :emp-no) 50)
					      (<= (getx document :emp-no) 100))
					     (and
					      (>= (getx document :emp-no) (/ size 2))
					      (<= (getx document :emp-no) (+ (/ size 2) 100)))
					     (and
					      (>= (getx document :emp-no) (- size 50))
					      (<= (getx document :emp-no) size))))))))
	   results))

    
    (print "Fetching an index set.")
    (time
     (push (list
	    :how-many-davises?
	    (length (query-data employee-collection                              
				:index-values (list :surname "Davis"))))
	   results))

    (print "Doing a query against an index set.")
    (time
     (push (list
	    :how-many-davises-in-india?
	    (length (query-data employee-collection
				:query (lambda (emp)
					 (string-equal (getx emp :country) "India"))
				:index-values (list :surname "Davis"))))
	   results)))

  (print results))



