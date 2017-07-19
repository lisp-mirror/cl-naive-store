(in-package :cl-naive-store)

(defparameter *universe* nil)

(defun test-create-hierarcy ()
  (let ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(link-store)
	(link-one-col)
	(link-two-col))
    
    (setf link-store (add-store universe (make-instance 'store :name "link-stuff")))
    
    (add-data-type 
     link-store
     (make-instance 'data-type
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
						    :item-type "one-deap"
						    :store "link-stuff"
						    :collection "one-deaps"))
		     (make-instance 'field 
				    :name :link-one-links
				    :key-p nil
				    :type-def (list :type :list
						    :list-type :item
						    :data-type "one-deap"
						    :store "link-stuff"
						    :collection "one-deaps")))))
    
    (add-data-type 
     link-store
     (make-instance 'data-type
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
							    :list-type :item
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
		  (list :link-one-key "some-key-1" :link-one-stuff "arstarsarstarst"
			:no-links (list 
				   (list :link-one-key "some-key-2" 
					 :link-one-stuff "arstarsarstarst")
				   (list :link-one-key "some-key-3" 
					 :link-one-stuff "cccccccccccciiii"))))
    
    (persist-item link-one-col
		  (list :link-one-key "some-key-2" :link-one-stuff "uouyloehoenhoooo"))
    
    (persist-item link-one-col
		  (list :link-one-key "some-key-3" :link-one-stuff "cccccccccccciiii"
				:no-links (list 
				   (list :link-one-key "some-key-2" 
					 :link-one-stuff "uouyloehoenhoooo")
				   (list :link-one-key "some-key-1" 
					 :link-one-stuff "arstarsarstarst"))))
    
    (persist-item link-two-col
		  (list :link-two-key "some-other-key-1" :link-two-stuff "ssssssssssss"
			:link-one-link (list :link-one-key "some-key-3" 
					     :link-one-stuff "cccccccccccciiii")
			:link-one-links (list (list :link-one-key "some-key-3" 
						    :link-one-stuff "cccccccccccciiii")
					      (list :link-one-key "some-key-1" 
						    :link-one-stuff "arstarsarstarst"))))
    (persist-item link-two-col
		  (list :link-two-key "some-other-key-2" :link-two-stuff "wwwwwwwwwwwww"
			:link-one-link (list :link-one-key "some-key-2" 
					     :link-one-stuff "uouyloehoenhoooo")
			:link-one-links (list (list :link-one-key "some-key-3" 
						    :link-one-stuff "cccccccccccciiii")
					      (list :link-one-key "some-key-2" 
						    :link-one-stuff "uouyloehoenhoooo"))))
    (persist-item link-two-col 
		  (list :link-two-key "some-other-key-3" :link-two-stuff "MMMMMMMMMMMMMMM"
			:link-one-link (list :link-one-key "some-key-1" 
					     :link-one-stuff "arstarsarstarst")
			:link-one-links (list (list :link-one-key "some-key-2" 
						    :link-one-stuff "uouyloehoenhoooo")
					      (list :link-one-key "some-key-1" 
						    :link-one-stuff "arstarsarstarst"))))
    universe))

(defun test-create-lots (stores-count collections-count items-count bucket-keys-count)
  
  (let ((universe (make-instance 
		   'universe
		   :location "~/data-universe/")))
        
    (dotimes (i stores-count)
      (let ((store
	     (add-store universe (make-instance 'store :name 
						(format nil "test-store-~A" i)))))
	(dotimes (j collections-count)
	  (let* (
		 (fields (list (make-instance 'field 
					      :name (intern 
						     (string-upcase
						      (format nil "col-~A-field-1" j))
						     :KEYWORD)
					      :key-p t
					      :type-def (list :type :string)
					      )
			       (make-instance 'field 
					      :name (intern 
						     (string-upcase
						      (format nil "col-~A-field-2" j))
						     :KEYWORD)
					      :key-p t
					      :type-def (list :type :string)
					      )
			       (make-instance 'field 
					      :name (intern 
						     (string-upcase
						      (format nil "col-~A-field-3" j))
						     :KEYWORD)
					      :key-p nil
					      :type-def (list :type :number)
					      )
			       (make-instance 'field 
					      :name (intern 
						     (string-upcase
						      (format nil "col-~A-field-4" j))
						     :KEYWORD)
					      :key-p nil
					      :type-def (list :type :string)
					      )
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
	    
	    (add-data-type store (make-instance 'data-type 
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
			(append item 
				(list (name field) 
				      (if (equalp (getf (type-def field) :type) :number)
					  (random 10000)
					  (format nil "~A" (random 10)))))))
		(persist-item collection item)))))))
    (setf *universe* universe)))


(defun test-load-hierarchy ()
  (let* ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(link-store (add-store universe (make-instance 'store :name "link-stuff"))))
  
    (load-store link-store)
    link-store))


(defun test-fetch ()
  (let* ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(link-store (add-store universe (make-instance 'store :name "link-stuff"))))
  
    (list (fetch-items link-store :collection-name "one-deaps")
	  (fetch-items link-store :collection-name "two-deaps")
	  (fetch-items link-store :collection-name "two-deaps" 
		       :test (lambda (item)
			       (equalp  (getf (item-values item) :link-two-stuff)
					"MMMMMMMMMMMMMMM" )))
	  link-store
	  )))

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
    
    (fetch-items one-deaps)))

(defun test-fetch-partial-hierarchy ()
  (let* ((universe (make-instance 
		   'universe
		   :location "~/data-universe/"))
	(link-store (add-store universe (make-instance 'store :name "link-stuff"))))
  
    (list (fetch-items link-store :collection-name "two-deaps" 
		       :test (lambda (item)
			       (equalp  (getf (item-values item) :link-two-stuff)
					"MMMMMMMMMMMMMMM" )))
	  link-store)))
