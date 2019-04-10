(in-package :cl-naive-store)

(defparameter *universe* nil)

(defun test-loose ()
  (let* ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(store (add-store universe
			  (make-instance 'store
					 :name "loose-stuff")))
	(collection (add-collection store 
			  (make-instance 
			   'collection 
			   :name "loose-items"
			   :data-type "hanging-loose"))))


    (add-data-type 
     store
     (make-instance
      'data-type
      :name "loose-one"
      :label "Loose One"
      :top-level-p t
      :fields
      (list
       (make-instance 'field 
		      :name :key-loose
		      :key-p t
		      :type-def (list :type :string))
       (make-instance 'field 
		      :name :loose-val
		      :key-p nil
		      :type-def (list :type :string)))))
    
    (add-data-type 
     store
     (make-instance
      'data-type
      :name "loose-two"
      :label "Loose Two"
      :top-level-p t
      :fields
      (list
       (make-instance 'field 
		      :name :key-loose-2
		      :key-p t
		      :type-def (list :type :string))
       (make-instance 'field 
		      :name :loose-val-2
		      :key-p nil
		      :type-def (list :type :item
				      :complex-type :item
				      :data-type "loose-one")))))
    
    (add-data-type 
     store
     (make-instance
      'data-type
      :name "hanging-loose"
      :label "Hanging Loose"
      :top-level-p t
      :fields
      (list
       (make-instance 'field 
		      :name :key-val
		      :key-p t
		      :type-def (list :type :string))
       (make-instance 'field 
		      :name :link-to-loose
		      :key-p nil
		      :type-def (list :type :item
				      :complex-type :item
				      :data-type "loose-one"))
       (make-instance 'field 
		      :name :link-loose-list
		      :key-p nil
		      :type-def (list :type :list
				      :complex-type :list-objects
				      :data-type "loose-two")))))

    (persist-item collection
		  (list
		   :key-val "1"
		   :link-loose (make-item
				:data-type "loose-one"
				:values (list
					 :key-loose "arst"
					 :loose-val "arst"
					 ))
		   :link-loose-list
		   (list (make-item
			  :data-type "loose-two"
			  :values (list
				   :key-loose-2 "qwfp"
				   :loose-val-2 (make-item
						 :data-type "loose-one"
						 :values (list
							  :key-loose "1234"
							  :loose-val "1234"
							  ))))

			 (make-item
			  :data-type "loose-two"
			  :values (list
				   :key-loose-2 "zxcv"
				   :loose-val-2 (make-item
						 :data-type "loose-one"
						 :values (list
							  :key-loose "5678"
							  :loose-val "5678")))))))


    (persist-item collection
		  (list
		   :key-val "2"
		   :link-loose (make-item
				:data-type "loose-one"
				:values (list
					 :key-loose "pglj"
					 :loose-val "pglj"))
		   :link-loose-list
		   (list (make-item
			  :data-type "loose-two"
			  :values (list
				   :key-loose-2 "qwfp"
				   :loose-val-2 (make-item
						 :data-type "loose-one"
						 :values (list
							  :key-loose "0000"
							  :loose-val "0000"))))

			 (make-item
			  :data-type "loose-two"
			  :values (list
				   :key-loose-2 "zxcv"
				   :loose-val-2 (make-item
						 :values (list
							  :key-loose "9999"
							  :loose-val "9999"
							  )))))))

    universe))


(defun test-load-loose ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (store (add-store universe (make-instance 'store :name "loose-stuff"))))
    
    (load-store store)
    store))


(defun test-fetch-loose ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (store (add-store universe (make-instance 'store :name "loose-stuff"))))
    
    (query-data store :collection-name "loose-items")))

(defun test-create-hierarchy ()
  (let ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(link-store)
	(link-one-col)
	(link-two-col)
	(item4)
	(item5)
	(item8)
	(item9))
    
    (setf link-store (add-store universe
				(make-instance 'store :name "link-stuff")))
    
    (add-data-type 
     link-store
     (make-instance
      'data-type
      :name "two-deap"
      :label "Two deap"
      :top-level-p t
      :fields
      (list
       (make-instance 'field 
		      :name :link-two-key
		      :key-p t
		      :type-def (list :type :string))
       (make-instance 'field 
		      :name :link-two-stuff
		      :key-p nil
		      :type-def (list :type :string))
       (make-instance 'field 
		      :name :link-one-link
		      :key-p nil
		      :type-def (list :type :item
				      :complex-type :collection
				      :item-type "one-deap"
						    
				      :collection "one-deaps"))
       (make-instance 'field 
		      :name :link-one-links
		      :key-p nil
		      :type-def (list :type :list
				      :complex-type :collection-objects
				      :data-type "one-deap"
						    
				      :collection "one-deaps")))))
    
    (add-data-type 
     link-store
     (make-instance
      'data-type
      :name "one-deap"
      :label "One Deap"
      :top-level-p t
      :fields (list
	       (make-instance 'field 
			      :name :link-one-key
			      :key-p t
			      :type-def (list :type :string))
	       (make-instance 'field 
			      :name :link-one-stuff
			      :key-p nil
			      :type-def (list :type :string))
	       (make-instance 'field 
			      :name :no-links
			      :key-p nil
			      :type-def (list :type :list
					      :complex-type :collection
					      :data-type "one-deap"
					      :store "link-stuff")))))  
    (setf link-one-col
	  (add-collection link-store 
			  (make-instance 
			   'collection 
			   :name "one-deaps"
			   :data-type "one-deap")))
    (setf link-two-col
	  (add-collection link-store 
			  (make-instance 
			   'collection 
			   :name "two-deaps"
			   :data-type "two-deap")))
    
  
    (persist-item link-one-col
		  (list :link-one-key "some-key-1"
			:link-one-stuff "arstarsarstarst"
			:no-links (list 
				   (make-item
				    :store link-store
				    :collection link-one-col
				    :values
				    (list :link-one-key "some-key-2" 
					  :link-one-stuff "bbbbbbbbbbbb"))
				   (make-item
				    :store link-store
				    :collection link-one-col
				    :values
				    (list :link-one-key "some-key-3" 
					  :link-one-stuff "cccccccccccciiii")))))
  
    (setf item4 (persist (make-item
			  :store link-store
			  :collection link-one-col
			  :values
			  (list :link-one-key "some-key-4"
				:link-one-stuff "uouyloehoenhoooo"))))

    (setf item8 (persist (make-item
			  :store link-store
			  :collection link-one-col
			  :values
			  (list :link-one-key "some-key-8"
				:link-one-stuff "qqqqqqqqqqqqqqqq"))))

    (setf item9 (persist (make-item
			  :store link-store
			  :collection link-one-col
			  :values
			  (list :link-one-key "some-key-9"
				:link-one-stuff "wwwwwwwwwwwwwwwwww"))))
   
    
    (setf item5
	  (persist-item link-one-col
			(list :link-one-key "some-key-5"
			      :link-one-stuff "eeeeeeeeeeeeee"
			      :no-links
			      (list 
			       (make-item
				:store link-store
				:collection link-one-col
				:values
				(list :link-one-key "some-key-6" 
				      :link-one-stuff "rrrrrrsssssssss"))
			       (make-item
				:store link-store
				:collection link-one-col
				:values
				(list :link-one-key "some-key-7" 
				      :link-one-stuff "dddddddddddddddd"))))))
    
    (persist (make-item
	      :store link-store
	      :collection link-one-col
	      :values
	      (list :link-two-key "some-other-key-1"
		    :link-two-stuff "ssssssssssss"
		    :link-one-link item4

		    :link-one-links	(list item5 item8))))
    
    (persist-item link-two-col
		  (list :link-two-key "some-other-key-2"
			:link-two-stuff "wwwwwwwwwwwww"
			:link-one-link item8		
			:link-one-links (list item4 item9)))
    
    (persist-item link-two-col 
		  (list :link-two-key "some-other-key-3"
			:link-two-stuff "MMMMMMMMMMMMMMM"))
    
    ;;testing change mech
 
    (persist-item link-one-col
		  (list :link-one-key "some-key-2" 
			:link-one-stuff "this is new changed values"))
    
    (persist-item link-one-col
		  (list :link-one-key "some-key-4" 
			:link-one-stuff "Setting up for delete 1"))
    (persist-item link-one-col
		  (list :link-one-key "some-key-4" 
			:link-one-stuff "Setting up for delete 2"))
    (persist-item link-one-col
		  (list :link-one-key "some-key-4" 
			:link-one-stuff "Setting up for delete 3"))
    (persist-item link-one-col
		  (list :link-one-key "some-key-4" 
			:link-one-stuff "Setting up for delete 4"))
    (persist-item link-one-col
		  (list 
		   :deleted% t
		   :link-one-key "some-key-4" 
		   :link-one-stuff "Setting up for delete 5"))
    
    universe))

(defun test-create-lots (stores-count collections-count
			 items-count bucket-keys-count)
  
  (let ((universe (make-instance 
		   'universe
		   :location "~/data-universe/")))
    
    (dotimes (i stores-count)
      (let ((store
	     (add-store universe
			(make-instance 'store :name 
				       (format nil "test-store-~A" i)))))
	(dotimes (j collections-count)
	  (let* ((fields
		  (list (make-instance 'field 
				       :name (intern 
					      (string-upcase
					       (format nil "col-~A-field-1" j))
					      :KEYWORD)
				       :key-p t
				       :type-def (list :type :string))
			(make-instance 'field 
				       :name (intern 
					      (string-upcase
					       (format nil "col-~A-field-2" j))
					      :KEYWORD)
				       :key-p t
				       :type-def (list :type :string))
			(make-instance 'field 
				       :name (intern 
					      (string-upcase
					       (format nil "col-~A-field-3" j))
					      :KEYWORD)
				       :key-p nil
				       :type-def (list :type :number))
			(make-instance 'field 
				       :name (intern 
					      (string-upcase
					       (format nil "col-~A-field-4" j))
					      :KEYWORD)
				       :key-p nil
				       :type-def (list :type :string))
			(make-instance 'field 
				       :name (intern 
					      (string-upcase
					       (format nil "col-~A-field-5" j))
					      :KEYWORD)
				       :key-p nil
				       :type-def (list :type :string))))
		 (collection)
		 (bucket-keys))

	    (if (> bucket-keys-count 2)
		(setf bucket-keys-count 2))
	    (dotimes (n bucket-keys-count)
	      (let ((key (intern 
			  (string-upcase
			   (format nil "col-~A-field-~A" j (+ n 1)))
			  :KEYWORD)))
		(push key bucket-keys)))
	    ;; (break "~A" bucket-keys)
	    
	    (add-data-type
	     store
	     (make-instance 'data-type 
			    :name (format nil "test-data-type-~A" j)
			    :top-level-p t
			    :fields fields))
	    
	    (setf collection (add-collection 
			      store 
			      (make-instance 
			       'collection 
			       :name (format nil "test-collection-~A" j)
			       :data-type (format nil "test-data-type-~A" j)
			       :bucket-keys (if bucket-keys
						(reverse bucket-keys)))))
	    (dotimes (k items-count)
	      (let ((item))
		(dolist (field fields)
		  (setf item 
			(append
			 item 
			 (list (name field) 
			       (if (equalp (getf (type-def field) :type) :number)
				   (random 10000)
				   (format nil "~A" (random 10)))))))
		(persist-item collection item)))))))
    universe))


(defun test-load-hierarchy ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (link-store (add-store universe (make-instance 'store :name "link-stuff"))))
    
    (load-store  link-store)
    link-store))


(defun test-fetch ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (link-store (add-store universe (make-instance 'store :name "link-stuff"))))
    
    (list
     
     (query-data link-store :collection-name "one-deaps")
     "x---------------------------------------------------------"
     (query-data link-store :collection-name "two-deaps")

     "xx---------------------------------------------------------"

     (query-data  link-store :collection-name "two-deaps" 
		  :query (lambda (item)
			  (equalp  (getf (item-values item) :link-two-stuff)
				   "MMMMMMMMMMMMMMM" )))
     "xxx---------------------------------------------------------"
     
     link-store)))

(defun test-fetch-col-sig ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (link-store (add-store universe (make-instance 'store :name "link-stuff")))
	 
	 (one-deaps (add-collection link-store 
				    (make-instance 
				     'collection 
				     :name "one-deaps"
				     :data-type "one-deap"))))
    
    (query-data one-deaps)))

(defun test-fetch-partial-hierarchy ()
  (let* ((universe (make-instance 
		    'universe
		    :location "~/data-universe/"))
	 (link-store (add-store universe (make-instance 'store :name "link-stuff"))))
    
    (list (query-data link-store :collection-name "two-deaps" 
		      :query (lambda (item)
			       (equalp  (getf (item-values item) :link-two-stuff)
					"MMMMMMMMMMMMMMM" )))
	  "---------------------------------------------------------"
	  link-store)))

(defun test-change-control ()
  (let ((item (make-item :values (list :blah "blah"))))
    (setf (getx item :blah) "shit")
    item))



