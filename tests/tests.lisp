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

(defclass collection-indexed (indexed-collection-mixin collection)
  ())

(defparameter *collection-class* 'collection-indexed)

(defun setup-universe (&optional location)
  "Sets up the the simple universe that will contain the data. Use the add-* functions to add 
stores and collections to make sure that they are set up correctly.
A universe is made up out of stores (databases) which are made up in turn of collections (tables/buckets)
which contain the actual data. Each collection will have its own directory and file on disk if data is persisted."

  (setf *universe* (make-instance 
			  'universe
			  :location *location*
			  :store-class *store-class*))
  (when location
    (setf (location *universe*) location))
  
  (let* ((store (add-store *universe*
			   (make-instance *store-class*
					  :name "simple-store"
					  :collection-class *collection-class*))))
    (add-collection store
		    (make-instance *collection-class*
				   :name "simple-collection"))))

(defun tare-down-universe ()
  "Deletes any peristed data from exmaples."
  (when *universe*
    (cl-fad:delete-directory-and-files (location *universe*) :if-does-not-exist :ignore)
    (setf *universe* nil)))

(defun populate-simple-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))

    ;;Make sure that any previously persisted objects are already loaded from disk.
    (unless (loaded-p collection)
      (load-data collection))
    
    ;;Add data objects in the form of plists to the collection. naive-store expects plists
    ;;by default. 
    (dotimes (x size)
      (let* ((object (list :key x
			   :name (format nil "Slave No ~A" x))))
	;;Persist adds the object to the collection for you but if you dont want to persist
	;;you need to use add-data-object directly.
	;;Persisting individual objects in a loop like this is slow, because the file is opened
	;;closed each time. Use (persist collection) if you are going to add lots of data, see
	;;populate-monster-data if you want to see how to use it.
	(if persist-p	  
	  (persist-object collection object)
	  (add-data-object collection object))))))

(defun query-simple-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    
    (query-data collection :query (lambda (data-object)				    
				    (<= (getx data-object :key) 50)))))

(defun simple-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data objects and then queries 
the collection to retrieve the the 50 objects that have a :key >= 50.
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

(defun test-all ()
  (let ((results))
    (setf results (append results (test-simple)))
    (setf results (append results (test-lazy-loading)))
    (setf results (append results (test-delete)))))

(defun test-all-indexed ()
  (let ((*collection-class* 'collection-indexed))
    (test-all)))

(defun test-passed-p (results)
  (let ((passed-p t))
    (dolist (test results)
      (unless (second test)
	(setf passed-p nil)))
    passed-p))

(defun query-monster-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    
    (query-data collection :query (lambda (data-object)				    
				    (or (and
					 (>= (getx data-object :key) 50)
					 (<= (getx data-object :key) 100))
					(and
					 (>= (getx data-object :key) 500000)
					 (<= (getx data-object :key) 500100))
					(and
					 (>= (getx data-object :key) 999950)
					 (<= (getx data-object :key) 1000000)))))))

(defun populate-monster-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))

    ;;Make sure that any previously persisted objects are already loaded from disk.
    (unless (loaded-p collection)
        (load-data collection))

    ;;Add data objects in the form of plists to the collection. naive-store expects plists
    ;;by default.
    (format t "Add ~A objects to collection" size)
    (time
     (progn
       (dotimes (x size)
	 (let* ((object (list :key x
			      :name (format nil "Slave No ~A" x)
			      :random (random size))))
	   ;;Add objects to collcetion
	   (add-data-object collection object)))))

    (format t "Persist collection")
    (time
     (when persist-p	  
       (persist collection)))))

(defun monster-example-lazy-loading ()
  "naive-store does lazy loading of data from disk when doing queries."
  
  (when *universe*
    ;;Unload the collection (contains the data) if it is already loaded.
    (let ((collection (get-collection (get-store *universe* "simple-store")
				      "simple-collection")))
      ;;Clear the collection
      (when (loaded-p collection)
	(setf collection (make-instance 'collection
					:name "simple-collection")))))
  (unless *universe*
    (setup-universe))

  (get-collection (get-store *universe* "simple-store")
		  "simple-collection")
  ;;Query the data in the universe
  (query-simple-data))

(defun monster-size-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data objects and then queries 
the collection to retrieve the the 50 objects that have a :key >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-monster-data persist-p :size 1000000)

  (format t "Query moster collection")
  ;;Query the data in the universe
  (time
   (query-monster-data)))

#|

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store::collection))
  (time (cl-naive-store-tests::monster-size-example t)))

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
  (time (cl-naive-store-tests::monster-size-example t)))


|#
