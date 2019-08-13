(in-package :cl-naive-store)

(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory )
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defparameter *universe* (make-instance 
			  'universe
			  :location (cl-fad:merge-pathnames-as-directory
				     (get-temp)
				     (make-pathname :directory '(:relative" data-universe")))))

(defun setup-universe (&optional location)
  "Sets up the the simple universe that will contain the data. Use the add-* functions to add 
stores and collections to make sure that they are set up correctly.
A universe is made up out of stores (databases) which are make up in turn of collections (tables/buckets)
which contains the actual data. Each collection will have its own file on disk if data is persisted."

  (when location
    (setf (location *universe*) location))
  
  (let* ((store (add-store *universe*
			   (make-instance 'store
					  :name "simple-store"))))
    (add-collection store
		    (make-instance 'collection
				   :name "simple-collection"))))

(defun tare-down-universe ()
  "Deletes any peristed data from exmaples."
  (when *universe*
    (cl-fad:delete-directory-and-files (location *universe*) :if-does-not-exist :ignore)
    (setf *universe* (make-instance 
			  'universe
			  :location (cl-fad:merge-pathnames-as-directory
				     (get-temp)
				     (make-pathname :directory '(:relative "data-universe")))))))

(defun populate-simple-data (persist-p)
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))

    ;;Make sure that any previously persisted objects are already loaded from disk.
    (unless (loaded-p collection)
      (load-data collection))
    
    ;;Add a 100 data objects in the form of plists to the collection. naive-store expects plists
    ;;by default. 
    (dotimes (x 100)
      (let* ((object (list :emp-no x
			   :name (format nil "Slave No ~A" x))))
	;;naive-store 
	(when persist-p	  
	  (persist-object collection object)
	  (add-data-object collection object))))))

(defun query-simple-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    
    (query-data collection :query (lambda (data-object)
				    
				    (>= (getf data-object :emp-no) 50)))))

(defun simple-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data objects and then queries 
the collection to retrieve the the 50 objects that have a :emp-no >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the objects that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe objects
  (populate-simple-data persist-p)
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
    (push (list :data-objects-count-100
		(= (length (data-objects
			    (get-collection (get-store *universe* "simple-store")
					    "simple-collection")))
		   100))
	   test-results)
    (push (list :query-result-count-50 (= (length query-results) 50))
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
    
    (push (list :query-result-count-50 (= (length query-results) 50))
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
    ;;Delete the top 50 objects
    (dolist (object results)
      (delete-data-object collection object))

    ;;Unload the collection (contains the data) if it is already loaded
    (when (loaded-p collection)
      (setf collection (make-instance 'collection
				      :name "simple-collection"))))
  
  ;;Query the data in the universe for the top 50 that has been deleted.
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

(defun test-passed-p (results)
  (let ((passed-p t))
    (dolist (test results)
      (unless (second test)
	(setf passed-p nil)))
    passed-p))
