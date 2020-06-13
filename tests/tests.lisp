(in-package :cl-naive-store-tests)

(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory )
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defparameter *universe* nil)

(defparameter *store-class* 'store)

(defparameter *location* (cl-fad:merge-pathnames-as-directory
			  (get-temp)
			  (make-pathname :directory '(:relative "data-universe"))))

(defparameter *object-type* :plist)

(defparameter *monster-size* 100000)

(defclass collection-indexed (indexed-collection-mixin collection)
  ())

(defparameter *collection-class* 'collection)

(defun data-type-def ()
  '(:name "employee"
       :label "Employee"
       :top-level-p t
       :fields ((:name :emp-no
		       :label "Employee No"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t)) 
		(:name :gender
		       :label "Gender"
		       :db-type (:type :string
				       :complex-type :value-list
				       :values ("Male"
						"Female"))
		       :attributes (:display t :editable t))
		(:name :race
		       :label "Race"
		       :db-type (:type :string
				       :complex-type :value-list
				       :values ("African"
						"White"
						"Indian"
						"Chinese"
						"Coloured"))
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

(defun init-data-type (store)
  (let ((data-type-def (data-type-def))
	(fields))

    
    (dolist (field (getf data-type-def :fields))
      (setf
       fields 
       (append fields 
	       (list (make-instance 
		      'field
		      :name (getf field :name)
		      :key-p (getf field :key-p)
		      :type-def (getf field :type-def)
		      :attributes (getf field :attributes))))))

    (add-data-type
     store
     (make-instance 
      'data-type
      :name (getf data-type-def :name)
      :label (getf data-type-def :label)
      :top-level-p
      (getf data-type-def :top-level-p)
      :fields fields))))

(defun setup-universe (&optional location)
  "Sets up the the simple universe that will contain the data. Use the add-* functions to add 
stores and collections to make sure that they are set up correctly.
A universe is made up out of stores (databases) which are made up in turn of collections (tables/buckets)
which contain the actual data. Each collection will have its own directory and file on disk if data is persisted."

  (setf *universe* (make-instance 
			  'universe
			  :location *location*
			  :store-class (if (equalp *object-type* 'item)
					   'item-store
					*store-class*)))
  (when location
    (setf (location *universe*) location))
  
  (let* ((store (add-store *universe*
			   (make-instance (if (equalp *object-type* 'item)
					      'item-store
					      *store-class*)
					  :name "simple-store"
					  :collection-class *collection-class*)))
	 (data-type (if (equalp *object-type* 'item)
			(init-data-type store))))
    
    (add-collection store
		    (if (equalp *object-type* 'item)
			(make-instance 'item-collection
				       :name "simple-collection"
				       :data-type data-type
				       :indexes '((:gender :race :surname)))
			(if (equalp *collection-class* 'collection-indexed)
			 (make-instance *collection-class*
					:name "simple-collection"
					:indexes '((:gender :race :surname)))
			 (make-instance *collection-class*
					:name "simple-collection"))
			))
    
    ))

(defun tare-down-universe ()
  "Deletes any peristed data from exmaples."
  (when *universe*
    (cl-fad:delete-directory-and-files (location *universe*) :if-does-not-exist :ignore)
    (setf *universe* nil)))

(defun random-from-list (list)
  (nth (random (length list)) list))

(defun populate-simple-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	(emp-race '("African" "White" "Indian" "Chinese" "Coloured"))
	(emp-gender '("Male" "Female"))
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))

    ;;Make sure that any previously persisted objects are already loaded from disk.
    (unless (loaded-p collection)
      (load-data collection))
    
    ;;Add data objects in the form of plists to the collection. naive-store expects plists
    ;;by default. 
    (dotimes (x size)
      (let* ((object (list 
		      :race (random-from-list emp-race)
		      :gender (random-from-list emp-gender)
		      :surname (random-from-list emp-surnames)
		      :name (format nil "Slave No ~A" x)			   
		      :emp-no x)))
	
	;;Persist adds the object to the collection for you but if you dont want to persist
	;;you need to use add-data-object directly.
	;;Persisting individual objects in a loop like this is slow, because the file is opened
	;;closed each time. Use (persist collection) if you are going to add lots of data, see
	;;populate-monster-data if you want to see how to use it.
	(if persist-p	  
	    (persist-object collection object)
	    (if (equalp *object-type* 'item)
		(add-data-object collection (make-item 
					     :store (store collection)
					     :collection collection
					     :data-type (if (stringp (data-type collection))
							    (item-data-type (data-type collection))
							    (name (data-type collection)))		
					     :values object))
		(add-data-object collection object)))))))

(defun query-simple-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    
    (query-data collection :query (lambda (data-object)				    
				    (<= (getx data-object :emp-no) 50)))))

(defun simple-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data objects and then queries 
the collection to retrieve the the 50 objects that have a :emp-no >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-simple-data persist-p :size 100)
  ;;Query the data in the universe
  (query-simple-data))

(defun test-simple ()
  (let ((test-results)
	(query-results (simple-example t)))
    
    (push (list :universe-directory-exists
		(probe-file
		 (cl-fad:merge-pathnames-as-directory
				     (get-temp)
				     (make-pathname :directory '(:relative "data-universe" "simple-store")))))
	  test-results)
   
    (push (list :collection-log-exists
		(probe-file
		 (cl-fad:merge-pathnames-as-file
		  (get-temp)
		  (make-pathname :directory '(:relative "data-universe" "simple-store" "simple-collection")
				 :name "simple-collection"
				 :type "log"))))
	  test-results)
    (let ((objects (data-objects
			    (get-collection (get-store *universe* "simple-store")
					    "simple-collection"))))
       (push (list :data-objects-count-100
		  (= (if (hash-table-p objects)
			 (hash-table-count objects)
			 (length objects))
		     100))
	    test-results))

    (push (list :query-result-count-51 (= (length query-results) 51))
	   test-results)
    (tare-down-universe)
    test-results))

(defun simple-example-lazy-loading ()
  "naive-store does lazy loading of data from disk when doing queries."
  ;;Clear any risidual 
  (tare-down-universe)
  
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-simple-data t)

  
  ;;Unload the collection (contains the data) if it is already loaded.
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (when (loaded-p collection)
      (setf collection (make-instance 'collection
				      :name "simple-collection"))))
   ;;Query the data in the universe
  (query-simple-data))

(defun test-lazy-loading ()
  (let ((test-results)
	(query-results (simple-example-lazy-loading)))
    
    (push (list :query-result-count-51 (= (length query-results) 51))
	   test-results)
    test-results))


(defun simple-example-delete ()
  "naive-store does lazy loading of data from disk when doing queries."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-simple-data t)

  
  (let ((results (query-simple-data))
	(collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    ;;Delete the top 51 objects
    (dolist (object results)
      (delete-data-object collection object))

    ;;Unload the collection (contains the data) if it is already loaded
    ;;to make sure the delete was persisted.
    (when (loaded-p collection)
      (setf collection (make-instance 'collection
				      :name "simple-collection"))))
  
  ;;Query the data in the universe for the top 51 that has been deleted.
  (query-simple-data))

(defun test-delete ()
  (let ((test-results)
	(query-results (simple-example-delete)))
    (push (list :query-result-nil (not query-results))
	  test-results)
    
    test-results))

(defun test-simple-duplicates ()
  (let ((test-results)
	(data))
    ;;Clear any risidual 
    (tare-down-universe)
    ;;Setup the data universe aka the objects that will contain the data
    (setup-universe)
    ;;Generate some data and put it in the universe objects
    (populate-simple-data t :size 100)
    

    ;;Query the data in the universe
     (setf data (query-simple-data))

    (let ((collection (get-collection (get-store *universe* "simple-store")
				      "simple-collection")))

      (dolist (object data)
	(if (equalp *object-type* 'item)
	    (persist-object collection (make-item 
					 :store (store collection)
					 :collection collection
					 :data-type (if (stringp (data-type collection))
							(item-data-type (data-type collection))
							(name (data-type collection)))		
					 :values (item-values object))
			    )
	    (persist-object collection 
			     (list 
			      :race (getf object :race)
			      :gender (getf object :gender)
			      :surname (getf object :surname)
			      :name (getf object :name)			   
			      :emp-no (getf object :emp-no)))))
      
      ;;Unload the collection (contains the data) if it is already loaded.
      (when (loaded-p collection)
	(setf collection (make-instance 'collection
					:name "simple-collection"))))
    ;;Query the data in the universe
    (setf test-results (query-simple-data) )

    (list (list :no-duplicates (= (length data)
				  (length test-results))))))

(defun test-all-simple ()
  (let ((results))
    (setf results (append results (test-simple)))
    (setf results (append results (test-simple-duplicates)))
    (setf results (append results (test-lazy-loading)))
    (setf results (append results (test-delete)))))

(defun test-all-simple-indexed ()
  (let ((*collection-class* 'collection-indexed))
    (test-all-simple)))

(defun test-all-simple-items ()
  (let ((*object-type* 'item))
    (test-all-simple)))


(defun query-monster-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    
    (query-data collection :query (lambda (data-object)				    
				    (or (and
					 (>= (getx data-object :emp-no) 50)
					 (<= (getx data-object :emp-no) 100))
					(and
					 (>= (getx data-object :emp-no) (/ *monster-size* 2))
					 (<= (getx data-object :emp-no) (+ (/ *monster-size* 2) 100)))
					(and
					 (>= (getx data-object :emp-no) (- *monster-size* 50))
					 (<= (getx data-object :emp-no) *monster-size*)))))))

(defun populate-monster-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	(emp-race '("African" "White" "Indian" "Chinese" "Coloured"))
	(emp-gender '("Male" "Female"))
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))

    ;;Make sure that any previously persisted objects are already loaded from disk.
    (unless (loaded-p collection)
        (load-data collection))

    ;;Add data objects in the form of plists to the collection. naive-store expects plists
    ;;by default.
    (format t "Add ~A objects to collection" size)
    (time
     (progn
       (dotimes (x size)
	 (let* ((object (list 
			   :race (random-from-list emp-race)
			   :gender (random-from-list emp-gender)
			   :surname (random-from-list emp-surnames)
			   :name (format nil "Slave No ~A" x)			   
			   :emp-no x)))
	   ;;Add objects to collcetion
	   (if (equalp *object-type* 'item)
	       (add-data-object collection (make-item 
					    :store (store collection)
					    :collection collection
					    :data-type (if (stringp (data-type collection))
							   (item-data-type (data-type collection))
							   (name (data-type collection)))		
					    :values object))
	       ;;To speed up loading of objects I am switching of duplicate handling.
	       (add-data-object collection object
				:handle-duplicates-p nil))))))

    (format t "Persist collection")
    (time
     (when persist-p	  
       (persist collection)))))


(defun monster-size-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data objects and then queries 
the collection to retrieve the the 50 objects that have a :emp-no >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-monster-data persist-p :size *monster-size*)

  (format t "Query moster collection")
  ;;Query the data in the universe
  (time
   (query-monster-data)))


(defun test-monster-size ()
  (let* ((test-results)
	 (query-results (monster-size-example t))
	 (query-length (length query-results)))
    
    (push (list :universe-directory-exists
		(probe-file
		 (cl-fad:merge-pathnames-as-directory
				     (get-temp)
				     (make-pathname :directory '(:relative "data-universe" "simple-store")))))
	  test-results)
   
    (push (list :collection-log-exists
		(probe-file
		 (cl-fad:merge-pathnames-as-file
		  (get-temp)
		  (make-pathname :directory '(:relative "data-universe" "simple-store" "simple-collection")
				 :name "simple-collection"
				 :type "log"))))
	  test-results)
    (let ((objects (data-objects
			    (get-collection (get-store *universe* "simple-store")
					    "simple-collection"))))       
       (push (list :data-objects-count-monster
		  (= (if (hash-table-p objects)
			 (hash-table-count objects)
			 (length objects))
		     *monster-size*))
	    test-results))

    (push (list :query-result-count-202 (= query-length 202))
	   test-results)
    (tare-down-universe)
    test-results))



(defun monster-example-lazy-loading ()
  "naive-store does lazy loading of data from disk when doing queries."

   ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-monster-data t :size *monster-size*)
  
  (when *universe*
    ;;Unload the collection (contains the data) if it is already loaded.
    (let ((collection (get-collection (get-store *universe* "simple-store")
				      "simple-collection")))
      ;;Clear the collection
      (when (loaded-p collection)
	(setf collection (make-instance 'collection
					:name "simple-collection")))))
  
  ;;Query the data in the universe
  (query-simple-data))

(defun test-monster-lazy-loading ()
  (let ((test-results)
	(query-results (monster-example-lazy-loading)))
 
    (push (list :lazy-query-result-count-51 (= (length query-results) 51))
	   test-results)
    test-results))

(defun test-all-monster ()
  (let ((*collection-class* 'collection))
    (let ((results))
      (setf results (append results (test-monster-size)))
      (setf results (append results (test-monster-lazy-loading)))
      (tare-down-universe)
      results)))

(defun test-all-monster-indexed ()
    (let ((*collection-class* 'collection-indexed))
      (let ((results))
	(setf results (append results (test-monster-size)))
	(setf results (append results (test-monster-lazy-loading)))
	(tare-down-universe)
	results)))

(defun test-all-monster-items ()
  (let ((*object-type* 'item))
    (let ((results))
	(setf results (append results (test-monster-size)))
	(setf results (append results (test-monster-lazy-loading)))
	;;(tare-down-universe)
	results)))

(defun key-values-lookup (values)
  "naive-index use index to get data objects"
  (unless *universe*
    (setup-universe))

  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (unless (data-objects collection)     
      (load-data collection))

    ;;Get objects from index values
    (index-lookup-values collection values)))

(defun indexed-query (index-values query)
   "naive-index use index to get and query values"
  (unless *universe*
    (setup-universe))

  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (unless (data-objects collection)     
      (load-data collection))

    (query-data collection
		:query query
		:index-values index-values)))

(defun indexed-reduce (index-values query function initial-value)
   "naive-index use index to get and query values"
  (unless *universe*
    (setup-universe))

  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (unless (data-objects collection)     
      (load-data collection))

    (naive-reduce collection
		  :index-values index-values
		  :query query		
		  :function function
		  :initial-value initial-value)))

(defun indexed-query-examples ()
  (let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed)
	(results))
    
    (cl-naive-store-tests::monster-size-example t)
    (format t "Key value Lookup surname Davis")
    (time (push (list
		 :how-many-davises?
		 (length (cl-naive-store-tests::key-values-lookup  (list :surname "Davis"))))
		results))
    (format t "Key value Lookup Indian males.")
    (time (push (list
		 :how-many-indian-males?
		 (length (cl-naive-store-tests::key-values-lookup
			  (list (list :race "Indian" )
				(list :gender "Male")))))
		results))
    
    (format t "Indexed Query Indian males with the surname Davis")
    (time (push (list
		 :how-many-indian-davises?
		 (length (cl-naive-store-tests::indexed-query 
			  (list (list :race "Indian") 	
				(list :gender "Male"))		
			  (lambda (object)		
			    (string-equal (cl-naive-store:getx object :surname) "Davis")))))
		results))
    
    (format t "Indexed Reduce, sum of emp-no's for Indian males with the surname Davis")
    (time (push (list
		 :sum-emp-no-indian-davise
		 (cl-naive-store-tests::indexed-reduce
		  (list (list :race "Indian") 	
			(list :gender "Male"))		
		  (lambda (object)		
		    (string-equal (cl-naive-store:getx object :surname) "Davis"))
		  (lambda (result object)
		    (incf result (cl-naive-store:getx object :emp-no)))
		  0))
	   results))
    results))


(defun test-all-monster-indexed-queries ()
  (let ((test-results))
    (dolist (result (indexed-query-examples))
      (push  (list (first result)
		   (if (and (second result)
			    (> (second result)
			       0))
		       t))
	     test-results))
    test-results))

(defun indexed-query-examples-items ()
  
  (let ((*object-type* 'item)
	(results))
    
    (cl-naive-store-tests::monster-size-example t)
    (format t "Key value Lookup surname Davis")
    (time (push (list
		 :how-many-davises?
		 (length (cl-naive-store-tests::key-values-lookup  (list :surname "Davis"))))
		results))
    (format t "Key value Lookup Indian males.")
    (time (push (list
		 :how-many-indian-males?
		 (length (cl-naive-store-tests::key-values-lookup
			  (list (list :race "Indian" )
				(list :gender "Male")))))
		results))
    
    (format t "Indexed Query Indian males with the surname Davis")
    (time (push (list
		 :how-many-indian-davises?
		 (length (cl-naive-store-tests::indexed-query 
			  (list (list :race "Indian") 	
				(list :gender "Male"))		
			  (lambda (object)		
			    (string-equal (cl-naive-store:getx object :surname) "Davis")))))
		results))
    
    (format t "Indexed Reduce, sum of emp-no's for Indian males with the surname Davis")
    (time (push (list
		 :sum-emp-no-indian-davise
		 (cl-naive-store-tests::indexed-reduce
		  (list (list :race "Indian") 	
			(list :gender "Male"))		
		  (lambda (object)		
		    (string-equal (cl-naive-store:getx object :surname) "Davis"))
		  (lambda (result object)
		    (incf result (cl-naive-store:getx object :emp-no)))
		  0))
	   results))
    results))

(defun test-all-monster-item-queries ()
  
  (let ((*object-type* 'item)
	(test-results))
    (dolist (result (indexed-query-examples-items))
      (push  (list (first result)
		   (if (and (second result)
			    (> (second result)
			       0))
		       t))
	     test-results))
    test-results))


(defun test-passed-p (results)
  (let ((passed-p t))
    (dolist (test results)
      (unless (second test)
	(setf passed-p nil)))
    passed-p))

#|
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-indexed))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-items))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed-queries))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-items))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-item-queries))


;;;IGNORE - Used to manually check stuff when writing tests
(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store::collection))
    (cl-naive-store-tests::test-simple-duplicates))

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
    (cl-naive-store-tests::test-simple-duplicates))


(let ((cl-naive-store-tests::*object-type* 'cl-naive-store-tests::item))
    (cl-naive-store-tests::test-simple-duplicates))


(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
(cl-naive-store-tests::test-simple))

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
(cl-naive-store-tests::monster-size-example t))

(cl-naive-store-tests::key-values-lookup  (list :surname "Davis"))

(cl-naive-store-tests::key-values-lookup  (list :race "African" :gender "Male" :surname "Davis" :name "Slave No 3" ))

|#


