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

(defparameter *document-type* :plist)

(defparameter *monster-size* 100000)

(defparameter *use-shards* t)

(defclass collection-indexed (indexed-collection-mixin collection)
  ())

(defparameter *collection-class* 'collection)

(defun document-type-def-x ()
  '((:name "asset-register"
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
     :documentation "This type represents a simple employee master.")
    (:name "employee"
     :label "Employee"
     :elements ((:name :emp-no
		       :label "Employee No"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t)) 
		(:name :gender
		       :label "Gender"
		       :db-type (:type :string
				       :complex-type :value-list
				       :elements ("Male"
						  "Female"))
		       :attributes (:display t :editable t))
		(:name :race
		       :label "Race"
		       :db-type (:type :string
				       :complex-type :value-list
				       :elements ("African"
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
		       :attributes (:display t :editable t))
		(:name :asset
		       :label "Asset"
		       :db-type (:type :document
				       :complex-type :document
				       :elements )
		       :attributes (:display t :editable t)))
     :documentation "This type represents a simple employee master.")))

(defun init-document-type (store)
  (let ((document-type-defs (document-type-def-x))
	
	(types))

    (dolist (type-def document-type-defs)
      (let ((elements))
	(dolist (element (getf type-def :elements))
	  (setf
	   elements 
	   (append elements 
		   (list (make-instance 
			  'element
			  :name (getf element :name)
			  :key-p (getf element :key-p)
			  :type-def (getf element :type-def)
			  :attributes (getf element :attributes))))))

	(push (add-document-type
	       store
	       (make-instance 
		'document-type
		:name (getf type-def :name)
		:label (getf type-def :label)
		:elements elements))
	      types)))
    (reverse types)))

(defun setup-universe (&optional location)
  "Sets up the the simple universe that will contain the data. Use the add-* functions to add 
stores and collections to make sure that they are set up correctly.
A universe is made up out of stores (databases) which are made up in turn of collections (tables/buckets)
which contain the actual data. Each collection will have its own directory and file on disk if data is persisted."

  (setf *universe* (make-instance 
			  'universe
			  :location *location*
			  :store-class (if (equalp *document-type* 'document)
					   'document-store
					*store-class*)))
  (when location
    (setf (location *universe*) location))
  
  (let* ((store (add-store *universe*
			   (make-instance (if (equalp *document-type* 'document)
					      'document-store
					      *store-class*)
					  :name "simple-store"
					  :collection-class *collection-class*)))
	 (document-types (if (equalp *document-type* 'document)
			    (init-document-type store))))
    
    (add-collection store
		    (if (equalp *document-type* 'document)
			(make-instance 'document-collection
					 :name "asset-collection"
					 :document-type (first document-types)
					 :keys '(:asset-no))
			(if (equalp *collection-class* 'collection-indexed)
			    (make-instance *collection-class*
					 :name "asset-collection"
					 :keys '(:asset-no))
			    (make-instance *collection-class*
					 :name "asset-collection"
					 :keys '(:asset-no)))))

    (add-collection store
		    (if (equalp *document-type* 'document)
			(make-instance 'document-collection
					 :name "simple-collection"
					 :document-type (second document-types)
					 ;;not setting keys to make sure fallback to document-type
					 ;;is done, need to add tests for both
					 ;;:keys '(:emp-no)
					 :shard-elements (if *use-shards*
							     (list :race))
					 :indexes '((:gender :race :surname)))
			(if (equalp *collection-class* 'collection-indexed)
			    (make-instance *collection-class*
					     :name "simple-collection"
					     :keys '(:emp-no)
					     :indexes '((:gender :race :surname))
					     :shard-elements (if *use-shards*
							     (list :race)))
			    (make-instance *collection-class*
					     :name "simple-collection"
					     :keys '(:emp-no)
					     :shard-elements (if *use-shards*
							     (list :race))))))))

(defun tare-down-universe ()
  "Deletes any peristed data from exmaples."
;;  (break "?")
  (unless *universe*
    (cl-fad:delete-directory-and-files *location* :if-does-not-exist :ignore)
    )
  (when *universe*
    (cl-fad:delete-directory-and-files (location *universe*) :if-does-not-exist :ignore)    
    (setf *universe* nil)
    ))

(defun random-from-list (list)
  (nth (random (length list)) list))

(defun populate-simple-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	(asset-collection (get-collection (get-store *universe* "simple-store")
				    "asset-collection"))
	(emp-race '("African" "White" "Indian" "Chinese" "Coloured"))
	(emp-gender '("Male" "Female"))
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))

    ;;Make sure that any previously persisted documents are already loaded from disk.
    (unless (data-loaded-p collection)
      (load-data collection))
    
    ;;Add data documents in the form of plists to the collection. naive-store expects plists
    ;;by default. 
    (dotimes (x size)
      (let* ((asset (if (equalp *document-type* 'document)
			(persist-document
			 asset-collection
			 (make-document 
			  :store (store asset-collection)
			  :collection asset-collection
			  :type-def (if (stringp (document-type asset-collection))
					(document-type asset-collection)
					(name (document-type asset-collection)))		
			  :elements (list :description x :asset-no x)))
			(list :description x :asset-no x)))
	     (document (list
			:asset asset
			:race (random-from-list emp-race)
			:gender (random-from-list emp-gender)
			:surname (random-from-list emp-surnames)
			:name (format nil "Slave No ~A" x)			   
			:emp-no x)))

	;;Persist adds the document to the collection for you but if you dont want to persist
	;;you need to use add-document directly.
	;;Persisting individual documents in a loop like this is slow, because the file is opened
	;;closed each time. Use (persist collection) if you are going to add lots of data, see
	;;populate-monster-data if you want to see how to use it.
	(if persist-p
	    (progn
	      (persist-document asset-collection asset)
	      (persist-document collection document))
	    (if (equalp *document-type* 'document)
		(add-document collection
			      (make-document 
			       :store (store collection)
			       :collection collection
			       :type-def (if (stringp (document-type collection))
					     (document-type collection)
					     (name (document-type collection)))		
			       :elements document))
		(add-document collection document)))))))

(defun query-simple-data ()  
  (let* ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	 (result ))
    
    (setf result (query-data collection :query (lambda (document)				    
						 (<= (getx document :emp-no) 50))))
    result))

(defun query-ref-doc ()  
  (let* ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	(reference-doc
	 (query-document collection :query (lambda (document)				    
					     (= (getx document :emp-no) 1)))))
    (if (document-p reference-doc)
	(if (index-lookup-hash (get-collection (get-store *universe* "simple-store")
						 "asset-collection")
				 (hash (getx reference-doc :asset)))
	      reference-doc)
	reference-doc)))

(defun simple-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data documents and then queries 
the collection to retrieve the the 50 documents that have a :emp-no >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe documents
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

    (unless *use-shards*
      (push (list :collection-log-exists
		  (probe-file
		   (cl-fad:merge-pathnames-as-file
		    (get-temp)
		    (make-pathname :directory
				   '(:relative "data-universe" "simple-store" "simple-collection")
				   :name "simple-collection"
				   :type "log"))))
	    test-results))
    
    (let ((documents (documents
		      (get-collection (get-store *universe* "simple-store")
				      "simple-collection"))))
      (push (list :documents-count-100
		  (= (if (hash-table-p documents)
			 (hash-table-count documents)
			 (length documents))
		     100))
	    test-results)

      (push (list :reference-document-found
		  (getx (query-ref-doc) :asset)
		  (query-ref-doc))

       test-results))

    ;;(break "~A ~% ~A" query-results (length query-results))
    
    (push (list :query-result-count-51 (= (length query-results) 51))
	  test-results)
    
    ;;(tare-down-universe)

    test-results))

(defun simple-example-lazy-loading ()
  "naive-store does lazy loading of data from disk when doing queries."
  ;;Clear any risidual 
  (tare-down-universe)
  
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe documents
  (populate-simple-data t)

  
  ;;Unload the collection (contains the data) if it is already loaded.
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))

    
    (when (data-loaded-p collection)
      (clear-collection collection))

    

    ;;(break "lazy ~A" collection)
    
    ;;Query the data in the universe
    (query-simple-data)

    ;; (break "lazy 2 ~A" collection)
    )
  )

(defun test-lazy-loading ()
  (let ((test-results)
	(query-results (simple-example-lazy-loading)))

    
    (push (list :query-result-count-51 (= (length query-results) 51)
		:actual-count (length query-results))
	   test-results)
    test-results))


(defun simple-example-delete ()
  "naive-store does lazy loading of data from disk when doing queries."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe documents
  (populate-simple-data t)

  
  (let ((results (query-simple-data))
	(collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    ;;Delete the top 51 documents
    (dolist (document results)
      (delete-document collection document))


    
    
    ;;Unload the collection (contains the data) if it is already loaded
    ;;to make sure the delete was persisted.
    (when (data-loaded-p collection)      
      (clear-collection collection)))
  
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
    ;;Setup the data universe aka the documents that will contain the data
    (setup-universe)
    ;;Generate some data and put it in the universe documents
    (populate-simple-data t :size 100)
    

    ;;Query the data in the universe
     (setf data (query-simple-data))

     (let ((collection (get-collection (get-store *universe* "simple-store")
				       "simple-collection")))

       (dolist (document data)
	 (if (equalp *document-type* 'document)
	     (persist-document collection
			       (make-document 
				:store (store collection)
				:collection collection
				:type-def (if (stringp (document-type collection))
					  (document-type collection)
					  (name (document-type collection)))		
				:elements (document-elements document)))	    
	     (persist-document collection 
			     (list 
			      :race (getf document :race)
			      :gender (getf document :gender)
			      :surname (getf document :surname)
			      :name (getf document :name)			   
			      :emp-no (getf document :emp-no)))))
       
       ;;Unload the collection (contains the data) if it is already loaded.
       (when (data-loaded-p collection)
	 (setf (shards collection) nil)
	 ))
     
    ;;Query the data in the universe
    (setf test-results (query-simple-data) )

    
    (list (list :no-duplicates (= (length data)
				  (length test-results))
		:actual-count (list (length data) (length test-results))))))

(defun test-all-simple ()
  (let ((results))
    (setf results (append results (test-simple)))
    (setf results (append results (test-simple-duplicates)))
    (setf results (append results (test-lazy-loading)))
    (setf results (append results (test-delete)))
    ))

(defun test-all-simple-indexed ()
  (let ((*collection-class* 'collection-indexed))
    (test-all-simple)))

(defun test-all-simple-documents ()
  (let ((*document-type* 'document))
    (test-all-simple)))


(defun query-monster-data ()
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (query-data collection :query
		(let ((size *monster-size*))
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

(defun populate-monster-data (persist-p &key (size 100))
  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection"))
	(emp-race '("African" "White" "Indian" "Chinese" "Coloured"))
	(shards (make-hash-table :test 'equalp))
	(emp-gender '("Male" "Female"))
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))

    (when *use-shards*
      ;;Used to speed up adding documents.
 ;;     (setf (gethash "African" shards) (get-shard collection (naive-impl:make-mac (list "African"))))
 ;;     (setf (gethash "White" shards) (get-shard collection (naive-impl:make-mac (list "White"))))
 ;;     (setf (gethash "Indian" shards) (get-shard collection (naive-impl:make-mac (list "Indian"))))
 ;;     (setf (gethash "Chinese" shards) (get-shard collection (naive-impl:make-mac (list "Chinese"))))
      ;;     (setf (gethash "Coloured" shards) (get-shard collection (naive-impl:make-mac (list "Coloured"))))
      )

   ;; (break "? ~A" collection)
    ;;Make sure that any previously persisted documents are already loaded from disk.
    (unless (data-loaded-p collection)
        (load-data collection))

   ;; (break "??~A" collection)
    ;;Add data documents in the form of plists to the collection. naive-store expects plists
    ;;by default.
    (format t "Add ~A documents to collection" size)
    (time
     (progn
       (dotimes (x size)
	 (let* ((race (random-from-list emp-race))
		(document (list 
			   :race race
			   :gender (random-from-list emp-gender)
			   :surname (random-from-list emp-surnames)
			   :name (format nil "Slave No ~A" x)			   
			   :emp-no x)))

	   
	   
	   ;;Add documents to collcetion
	   (if (equalp *document-type* 'document)
	       (add-document collection
				  (make-document 
				   :store (store collection)
				   :collection collection
				   :type-def (if (stringp (document-type collection))
						      (document-type collection)
						      (name (document-type collection)))
				   
				   :elements document)
				  :shard (if *use-shards* (gethash race shards)))
	       ;;To speed up loading of documents I am switching of duplicate handling.
	       (add-document collection document
			     :handle-duplicates-p nil
			     :shard (if *use-shards* (gethash race shards))))))))

    
    (format t "Persist collection")
    (time
     (when persist-p
       (persist collection)))
    ))


(defun monster-size-example (persist-p)
  "This example sets up a store and populates a collection with a 100 data documents and then queries 
the collection to retrieve the the 50 documents that have a :emp-no >= 50.
Only peristed if persist-p is t."
  ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe documents
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
    (unless *use-shards*
      (push (list :collection-log-exists
		  (probe-file
		   (cl-fad:merge-pathnames-as-file
		    (get-temp)
		    (make-pathname :directory '(:relative "data-universe" "simple-store" "simple-collection")
				   :name "simple-collection"
				   :type "log"))))
	    test-results))
    
    (let ((documents (documents
		      (get-collection (get-store *universe* "simple-store")
				      "simple-collection"))))       
       (push (list :documents-count-monster
		  (= (if (hash-table-p documents)
			 (hash-table-count documents)
			 (length documents))
		     *monster-size*))
	    test-results))
    
    (push (list :query-result-count-202 (= query-length 202) query-length)
	  test-results)
    
    ;;(tare-down-universe)

    test-results))



(defun monster-example-lazy-loading ()
  "naive-store does lazy loading of data from disk when doing queries."

   ;;Clear any risidual 
  (tare-down-universe)
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)

  ;;Generate some data and put it in the universe documents
  (populate-monster-data t :size *monster-size*)

  (when *universe*
    ;;Unload the collection (contains the data) if it is already loaded.
    (let ((collection (get-collection (get-store *universe* "simple-store")
				      "simple-collection")))
      ;;Clear the collection
      (when (data-loaded-p collection)
	(clear-collection collection)
	)))

;;  (break "collection lazy ~A")
  ;;Query the data in the universe
  (query-simple-data))

(defun test-monster-lazy-loading ()
  (let ((test-results)
	(query-results (monster-example-lazy-loading)))
 
    (push (list :lazy-query-result-count-51 (= (length query-results) 51) (length query-results))
	   test-results)
    test-results))

(defun test-all-monster ()
  (let ((*collection-class* 'collection)
	;;have to trim down moster on list duplicate checking kills speed for loding db
	(*monster-size* 10000)) 
    (let ((results))
      (setf results (append results (test-monster-size)))
      (setf results (append results (test-monster-lazy-loading)))
     ;; (tare-down-universe)
      results)))

(defun test-all-monster-indexed ()
    (let ((*collection-class* 'collection-indexed))
      (let ((results))
	(setf results (append results (test-monster-size)))
	(setf results (append results (test-monster-lazy-loading)))
	;;(tare-down-universe)
	results)))

(defun test-all-monster-documents ()
  (let ((*document-type* 'document))
    (let ((results))
	(setf results (append results (test-monster-size)))
	(setf results (append results (test-monster-lazy-loading)))
	;;(tare-down-universe)
	results)))


(defun key-values-lookup (values)
  "naive-index use index to get data documents"
  (unless *universe*
    (setup-universe))

  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (unless (documents collection)     
      (load-data collection))

    ;;Get documents from index values
    (index-lookup-values collection values)))

(defun indexed-query (index-values query)
   "naive-index use index to get and query values"
  (unless *universe*
    (setup-universe))

  (let ((collection (get-collection (get-store *universe* "simple-store")
				    "simple-collection")))
    (unless (documents collection)     
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
    (unless (documents collection)     
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
			  (lambda (document)		
			    (string-equal (getx document :surname) "Davis")))))
		results))
    
    (format t "Indexed Reduce, sum of emp-no's for Indian males with the surname Davis")
    (time (push (list
		 :sum-emp-no-indian-davise
		 (cl-naive-store-tests::indexed-reduce
		  (list (list :race "Indian") 	
			(list :gender "Male"))		
		  (lambda (document)		
		    (string-equal (getx document :surname) "Davis"))
		  (lambda (result document)
		    (incf result (getx document :emp-no)))
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

(defun indexed-query-examples-documents ()
  
  (let ((*document-type* 'document)
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
			  (lambda (document)		
			    (string-equal (getx document :surname) "Davis")))))
		results))
    
    (format t "Indexed Reduce, sum of emp-no's for Indian males with the surname Davis")
    (time (push (list
		 :sum-emp-no-indian-davise
		 (cl-naive-store-tests::indexed-reduce
		  (list (list :race "Indian") 	
			(list :gender "Male"))		
		  (lambda (document)		
		    (string-equal (getx document :surname) "Davis"))
		  (lambda (result document)
		    (incf result (getx document :emp-no)))
		  0))
	   results))
    results))

(defun test-all-monster-document-queries ()
  
  (let ((*document-type* 'document)
	(test-results))
    (dolist (result (indexed-query-examples-documents))
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


(defun test-all (&optional (*monster-size* 100000))
  
  (tare-down-universe)
  (and
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-indexed))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-documents))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed-queries))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-documents))
    (cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-document-queries))))

#|


(push :naive-store-tests *features*)
(ql:quickload :cl-naive-store :verbose t)
(setf cl-naive-store-tests::*use-shards* t)

(cl-naive-store-tests::test-all 100000)



(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-indexed))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-simple-documents))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-indexed-queries))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-documents))
(cl-naive-store-tests::test-passed-p (cl-naive-store-tests::test-all-monster-document-queries))

;;(cl-naive-store-tests::test-monster-size)
;;(length (cl-naive-store-tests::query-monster-data))
;;(length (cl-naive-store-tests::query-simple-data))




;;;IGNORE - Used to manually check stuff when writing tests
(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store::collection))
    (cl-naive-store-tests::test-simple-duplicates))

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
    (cl-naive-store-tests::test-simple-duplicates))


(let ((cl-naive-store-tests::*document-type* 'cl-naive-store-tests::document))
    (cl-naive-store-tests::test-simple-duplicates))


(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
(cl-naive-store-tests::test-simple))

(let ((cl-naive-store-tests::*collection-class* 'cl-naive-store-tests::collection-indexed))
(cl-naive-store-tests::monster-size-example t))

(cl-naive-store-tests::key-values-lookup  (list :surname "Davis"))

(cl-naive-store-tests::key-values-lookup  (list :race "African" :gender "Male" :surname "Davis" :name "Slave No 3" ))

|#


