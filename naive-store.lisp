(in-package :cl-naive-store)

(defclass field ()
  ((name :initarg :name
	 :accessor name
	 :initform nil)
   (type-def :initarg :type-def
	     :accessor type-def
	     :initform nil)
   (key-p :initarg :key-p
	  :accessor key-p
	  :initform nil)
   (attributes :initarg :attributes
	       :accessor attributes
	       :initform nil)))

(defclass bucket ()
  ((collection :initarg :collection
	  :accessor collection
	  :initform nil)
   (key-values :initarg :key-values
	       :accessor key-values
	       :initform nil)
   (items :initarg :items
	  :accessor items
	  :initform nil)
   (location :initarg :location
	     :accessor location
	     :initform nil)))

(defclass collection ()
  ((store :initarg :store
	  :accessor store
	  :initform nil)
   (name :initarg :name
	 :accessor name)
   (bucket-keys :initarg :bucket-keys
	       :accessor bucket-keys
	       :initform nil)
   (data-type :initarg :data-type
	       :accessor data-type
	       :initform nil)
   (location :initarg :location
	     :accessor location
	     :initform nil)
   (buckets :initarg :buckets
	    :accessor buckets
	    :initform nil)
   (index :initarg :index
	  :accessor index
	  :initform (make-hash-table :test 'equalp))))

(defclass data-type ()
  ((store :initarg :store
	  :accessor store
	  :initform nil)
   (name :initarg :name
	 :accessor name
	 :initform nil)
   (label :initarg :label
	 :accessor label
	 :initform nil)
   (top-level-p :initarg :top-level-p
		:accessor top-level-p
		:initform nil)  
   (fields :initarg :fields
	   :accessor fields
	   :initform nil)))

(defclass store ()
  ((universe :initarg :universe
	     :accessor universe
	     :initform nil)
   (name :initarg :name
	 :accessor name)
   (data-types :initarg :data-types
	       :accessor data-types
	       :initform nil)
   (collections :initarg :collections
		:accessor collections
		:initform nil)
   (location :initarg :location
	     :accessor location
	     :initform nil)))

(defclass universe ()
  ((stores :initarg :stores
	     :accessor stores
	     :initform nil)
   (location :initarg :location
	     :accessor location
	     :initform "~/data-universe/")))

(defstruct item
  hash
  bucket-key
  values
  changes
  persisted-p)

(defun write-to-file (file object &key (if-exists :append))
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
      (with-standard-io-syntax
	(if (equalp (type-of object) 'item)
	    (pprint (item-values object) out)
	    (pprint object out)))))

(defun write-list-to-file (file list &key (if-exists :append))
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
      (with-standard-io-syntax
	(dolist (object list)
	  (if (equalp (type-of object) 'item)
	      (pprint (item-values object) out)
	      (pprint object out))))))

(defgeneric persist (object &key &allow-other-keys))

(defmethod persist ((item item) &key file &allow-other-keys)
  (write-to-file file
		 item
		 :if-exists :append))

(defmethod persist ((list list) &key file (if-exists :append) &allow-other-keys)
  (write-list-to-file file
		      list
		      :if-exists if-exists))

(defmethod persist ((store store) &key &allow-other-keys)
  (write-to-file (format nil "~A~A.store" (location store) (name store))
		 (list :name (name store)
		       :location (location store)
		       ;;:data-types (data-types store)
		       )
		 :if-exists :supersede))

(defmethod add-store ((universe universe) (store store))
  (when (location store)
    (ensure-directories-exist (location store)))
  (unless (location store)
    (let ((location (format nil "~A~A/" 
			    (location universe) 
			    (name store))))
      (ensure-directories-exist location)
      (setf (location store) location)
      (setf (universe store) universe)
      (persist store)))
  (pushnew store (stores universe))
  store)

(defmethod persist ((data-type data-type) &key &allow-other-keys)
  (let ((fields))    
    (dolist (field (fields data-type))
      (setf fields (append fields (list (list
					 :name (name field)
					 :key-p (key-p field)
					 :type-def (type-def field)
					 :attributes (attributes field))))))
    
    (write-to-file (format nil "~A~A.type" (location (store data-type))
			   (name data-type))
		 (list 
		  :name (name data-type)
		  :label (label data-type)
		  :top-level-p t
		  :fields fields)
		 :if-exists :supersede)))

(defmethod add-data-type ((store store) (data-type data-type))
  (setf (store data-type) store)
  (pushnew data-type (data-types store))
  (persist data-type)
  data-type)

(defun perist-collection-def (collection)
  (write-to-file (format nil "~A~A.col" (location (store collection))
			   (name collection))
		   (list 
		    :name (name collection)
		    :location (location collection)
		    :data-type (data-type collection)
		    :bucket-keys (bucket-keys collection))
		   :if-exists :supersede))

(defun persist-collection (collection)
  (dolist (bucket (buckets collection))
    (persist (items bucket) :file (location bucket))))

(defmethod perist ((collection collection) &key def-only-p &allow-other-keys)
  (perist-collection-def collection)
  (unless def-only-p
    (persist-collection collection)))

(defmethod add-collection ((store store) (collection collection))
  (let ((location (location collection)))

    (when location
      (ensure-directories-exist location))

    (unless location 
      (setf location (format nil "~A~A" 
			     (location store) 
			     (name collection)))
      (ensure-directories-exist (format nil "~A/" location)))
    
    (setf (location collection) location)
    (pushnew collection (collections store))
    (setf (store collection) store)
    (perist collection :def-only-p t))
  collection)

(defmethod get-store ((universe universe) store-name)
  (dolist (store (stores universe))
    (when (string-equal store-name (name store))
      (return-from get-store store))))

(defmethod get-data-type ((store store) type-name)
  (dolist (data-type (data-types store))
    (when (string-equal type-name (name data-type))
      (return-from get-data-type data-type))))

(defmethod get-collection ((store store) collection-name)
  (dolist (collection (collections store))
    (when (string-equal collection-name (name collection))
      (return-from get-collection collection))))

(defmethod get-bucket ((store store) collection-name bucket-key)
  (dolist (collection (collections store))
    (when (string-equal collection-name (name collection))
      (dolist (bucket (buckets collection))
	(when (string-equal bucket-key (key-values bucket))
	  (return-from get-bucket bucket))))))

(defmethod get-collection-bucket ((collection collection) bucket-key)
  (dolist (bucket (buckets collection))
	(when (string-equal bucket-key (key-values bucket))
	  (return-from get-collection-bucket bucket))))

(defun get-key-values (keys item)
  (let ((val))
    (dolist (key keys)
      (push (list key (getf item key)) val))   
    (reverse val)))

(defun get-bucket-key-val-location (collection key-values)
  (let ((location (location collection)))    
    (dolist (key-val key-values)
      (setf location (format nil "~A/~A" location (second key-val))))
    (format nil "~A/~A.log" location (name collection))))

(defun get-bucket-from-keys (collection key-values)
  (dolist (bucket (buckets collection))
    (when (equalp key-values (key-values bucket))
      (return-from get-bucket-from-keys bucket))))


(defun lookup-or-create-item (collection bucket item-values)
  (let ((item (lookup-index collection item-values)))
	(if item
	    (setf (item-values item) item-values)
	    (let ((hash (sxhash (index-keys collection item-values))))	  
	      (setf item (make-item :hash hash 
				    :bucket-key (key-values bucket) 
				    :values item-values))
	      (setf (gethash hash (index collection)) item)
	      (push item (items bucket))))	
	item))

(defvar *bucket-keys*)

(defun load-hash-val (collection field val)
  (when val
    (let* ((bucket-key (getf val :bucket-key))
	   (bucket (gethash bucket-key *bucket-keys*)))
      
      (if (not bucket)
	  (if bucket-key
	      (let ((store (if (getf (type-def field) :store)
			       (get-store 
				(universe (store collection))
				(getf (type-def field) :store))
			       (store collection)))
		    (collection (get-collection 
				 (store collection)
				 (getf (type-def field) :collection))))
		(unless collection
		  (setf collection (get-collection-from-def 
				    store 
				    (getf (type-def field) :collection))))
		  
		(let* ((bucket (get-bucket (store collection)
					   (getf (type-def field) :collection)
					   bucket-key)))
		  (unless bucket
		    (setf bucket (add-bucket collection 
					     (get-bucket-key-val-location 
					      collection
					      bucket-key)
					     bucket-key)))
		  (setf (gethash bucket-key *bucket-keys*) bucket)
		  (lookup-or-create-item collection bucket val))))

	  (lookup-or-create-item (collection bucket) bucket val)))))



(defun load-item-from-values (collection bucket item-values)
  (when item-values
    (let ((*bucket-keys* (make-hash-table :test 'equalp)))
      (dolist (field (fields (get-data-type (store collection)
					    (data-type collection))))
	
	(when (or (and (equalp (getf (type-def field) :type) :item)
		       (getf (type-def field) :collection))
		  (and (equalp (getf (type-def field) :type) :list)
		       (equalp (getf (type-def field) :list-type) :item)
		       (getf (type-def field) :collection)))
	  (let ((sub-val (getf item-values (name field))))
	    (when sub-val
	      (if (listp (first sub-val))
		  (dolist (val sub-val)
		    (load-hash-val collection field val))
		  (load-hash-val collection field sub-val)))))))
    
    (lookup-or-create-item collection bucket item-values)))

(defun load-items (collection bucket filename)
  (with-open-file (in filename :if-does-not-exist :create)
     (with-standard-io-syntax              
       (when in
	 (loop for line = (read in nil)
	    while line
	    do (load-item-from-values collection bucket line) )
	 (close in))))
  bucket)


(defmethod add-bucket ((collection collection) location key-values)  
  (let ((bucket (make-instance 'bucket 
			       :key-values key-values
			       :location location)))

    (ensure-directories-exist location)
    (setf bucket (load-items collection bucket location ))
    
    (setf (buckets collection) (append (buckets collection)
				       (list bucket)))   
    (setf (collection bucket) collection)
    bucket))

(defun change-in-item-p (item)
  (not (equalp (item-values item) (item-changes item))))

(defgeneric index-keys (fields-source item-values))

(defmethod index-keys ((collection collection) item-values)  
  (index-keys (get-data-type (store collection) 
			     (data-type collection))
	      item-values))

(defmethod index-keys ((data-type data-type) item-values)
  (index-keys (fields data-type) item-values))

(defmethod index-keys ((fields list) item-values)
  (let ((keys))
    (dolist (field fields)
      (when (key-p field)
	(push (getf item-values (name field)) keys)))
    (reverse keys)))

(defun lookup-index (collection item-values)
  (let ((keys (index-keys (get-data-type 
			   (store collection)
			   (data-type collection))
			  item-values)))    
    (gethash (sxhash keys) (index collection))))

(defun item-val-reference (collection val)
  (let ((item (lookup-index collection val))
	(hash (sxhash (index-keys collection val))))
    (unless item
      (setf item (persist-item collection val)))
    (list 
	   :store (name (store collection))
	   :collection (name collection)
	   :hash% hash
	   :bucket-key (item-bucket-key item))))

(defun coerce-val-to-item-ref (collection field val)
  (let ((sub-p (if (listp (first val))
		   (not (getf (first val) :hash%))
		   (not (getf val :hash%)))))
    
    (if sub-p
	(let* ((sub-store (if (getf (type-def field) :store)
			      (get-store (universe (store collection))
					 (getf (type-def field) :store))
			      (store collection)))
	       (sub-collection (get-collection 
				sub-store
				(getf (type-def field) :collection)))
	       (reference))

	  (when (listp (first val))
	    (dolist (list-val val)
	      (setf reference 
		    (append reference
			    (list (item-val-reference sub-collection list-val)))))
	    
	    )
	  (unless (listp (first val))
	    (setf reference (item-val-reference sub-collection val)))
	  
	  (list (name field)) reference)
	(list (name field)
	      val))))

(defun parse-item-values-tree (collection item-values)
  (let ((fields (fields (get-data-type (store collection) 
				       (data-type collection))))
	(paresed-item-values))
    (dolist (field fields)
      (if (or (equalp (getf (type-def field) :type) :item)
	      (and (equalp (getf (type-def field) :type) :list)
		   (equalp (getf (type-def field) :list-type) :item)
		   (getf (type-def field) :collection)
		   ))
	  (setf paresed-item-values 
		(append
		 paresed-item-values
		 (list (name field)
		       (coerce-val-to-item-ref collection
					       field
					       (getf item-values (name field))))))
	
	  (setf paresed-item-values 
		(append
		 paresed-item-values
		 (list (name field)
			     (getf item-values (name field)))))))
    paresed-item-values))

(defun parse-and-perist (collection bucket bucket-location item values)
  (let ((parsed-values (parse-item-values-tree collection values)))
    
    (setf (item-values item) parsed-values)
    (persist item :file bucket-location)
    (setf (item-bucket-key item) (key-values bucket))
    (setf (item-changes item) nil)
    (setf (item-persisted-p item) t)
    
    ))

(defun add-item* (collection bucket bucket-location item)  
  
  (when (equalp (type-of item) 'item)
    (when (change-in-item-p item)
      (setf (item-persisted-p item) nil)
      (unless (equalp (sxhash (index-keys collection (item-values item)))
		      (sxhash (index-keys collection (item-changes item))))
	;;TODO: implement cascading changes
	(error (format nil "Cant change key values~%~A~%~A" 
		       (item-values item)
		       (item-changes item))))
      (setf item (parse-and-perist collection bucket bucket-location 
				   item (item-changes item))))
    
    (unless (change-in-item-p item)
      (unless (item-persisted-p item)	
	(unless (lookup-index collection (item-values item))
	  (let ((hash (sxhash (index-keys collection (item-values item)))))
	    
	    (setf item (parse-and-perist collection bucket bucket-location 
					 item (item-values item)))
	    (pushnew item (items bucket))
	    (setf (gethash hash (index collection)) item)	    
	    item)))))
  
  (unless (equalp (type-of item) 'item)      
    (let ((existing-item (lookup-index collection item))
	  (parsed-values (parse-item-values-tree collection item)))
      
      (when existing-item
	(unless (equalp (item-values existing-item) parsed-values)
	  (setf (item-values existing-item) parsed-values)
	  (persist existing-item :file bucket-location)))
      
      (unless existing-item
	(let* ((hash (sxhash (index-keys collection parsed-values)))
	      (item-item (make-item :hash hash
				    :values parsed-values
				    :persisted-p nil)))
	  
	  (pushnew item-item (items bucket))
	 
	  (setf (gethash hash (index collection))
		item-item)
	  (setf (item-persisted-p item-item) nil)	  
	  (persist item-item :file bucket-location)
	  (setf (item-bucket-key item-item) (key-values bucket))
	  (setf (item-persisted-p item-item) t)
	  item-item)))))

(defmethod persist-item ((collection collection) item)
  (when (bucket-keys collection)      
    (let* ((key-values (get-key-values (bucket-keys collection) 
				       (if (equalp (type-of item) 'item)
					   (item-values item)
					   item)))
	   (bucket (get-bucket-from-keys collection key-values))
	   (bucket-location (get-bucket-key-val-location
			     collection
			     key-values)))
      (unless bucket
	(setf bucket (add-bucket collection bucket-location key-values)))
            
      (add-item* collection bucket bucket-location item)))
  
  (unless (bucket-keys collection)
    (let ((bucket (first (buckets collection)))
	  (bucket-location (format nil "~A/~A.log" 
				   (location collection)
				   (name collection))))      
      (unless bucket
	(setf bucket (add-bucket collection bucket-location nil)))
      
      (add-item* collection bucket bucket-location item))))

(defun get-collection-from-def (store collection-name)
  (let ((filename (format nil "~A~A.col" (location store) collection-name))
	(collection))    
    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf collection (read in nil))
	  (close in))))
    (when collection
      (make-instance 'collection
		     :store store
		     :name (getf collection :name)
		     :bucket-keys (getf collection :bucket-keys)
		     :location (getf collection :location)
		     :data-type (getf collection :data-type)))))


(defun load-store-data-types (store)
  (let ((files (directory (format nil "~A**/*.type" (location store))))
		  (type-contents))
	      (dolist (file files)
		(with-open-file (in file :if-does-not-exist :create)
		  (with-standard-io-syntax              
		    (when in
		      (setf type-contents (read in nil))
		      (close in))))
		
		(let ((fields))
		  (dolist (field (getf type-contents :fields))
		    (setf fields 
			  (append fields 
				  (list (make-instance 'field
						       :name (getf field :name)
						       :key-p (getf field :key-p)
						       :type-def (getf field :type-def))))))
		  
		  (add-data-type 
		   store 
		   (make-instance 'data-type
				  :name (getf type-contents :name)
				  :label (getf type-contents :label)
				  :top-level-p (getf type-contents :top-level-p)
				  :fields fields))))))

(defun load-store-collections (store)
  (let ((files (directory (format nil "~A**/*.col" (location store)))))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	(when file-contents
	  (add-collection 
	     store 
	     (make-instance 'collection
			    :name (getf file-contents :name)
			    :location (getf file-contents :location)
			    :data-type (getf file-contents :data-type))))))))



(defun load-collection-items (collection)  
  (let ((files (directory (format nil "~A/**/*.log" (location collection))))
	(data-type (get-data-type (store collection)
				  (data-type collection)))
	(buckets))
    
    (unless data-type
      (load-store-data-types (store collection)))
    
    (dolist (file files)
      (let ((bucket-key)
	    (final-bucket-key)
	    (stop))
	
	(dolist (key (reverse (directory file)))
	  (when (equalp key (name collection))
	    (setf stop t))
	  (unless stop
	    (push key bucket-key)))

	(when bucket-key
	  (dolist (key (bucket-keys collection))
	    (push (list key (pop bucket-key)) final-bucket-key))
	  (pushnew  
	   (add-bucket collection 
		       (namestring file) 
		       (reverse final-bucket-key))
	   buckets))
	
	(unless bucket-key
	  (add-bucket collection 
		      (namestring file) 
		      bucket-key))))
    buckets))

(defun load-store (store)
  (load-store-data-types store)
  (load-store-collections store)
  (dolist (collection (collections store))
    (load-collection-items collection)))

(defun get-buckets-for-fetch (store collection bucket-keys)
  (let ((buckets))
    (when bucket-keys
      (dolist (bucket-key bucket-keys)
	(let ((bucket (get-bucket store
				  (name collection)
				  bucket-key)))
	  (unless bucket
	    (let ((file-path (location collection)))
	      (dolist (key bucket-key)
		(setf file-path (format nil "~A/~A" file-path key)))
	      (setf bucket (add-bucket collection 
				       (format nil "~A/~A.log" file-path (name collection)) 
				       bucket-key))))
	  (pushnew bucket buckets))))
    
    (unless bucket-keys
      (setf buckets (buckets collection)))
    
    ;;If no buckets are found try to load all of them from file
    (unless (and bucket-keys buckets)
      (setf buckets (load-collection-items collection)))
    buckets))

(defgeneric fetch-items (object &key test test-args
				  bucket-keys return-type
				  &allow-other-keys))

(defmethod fetch-items ((store store) &key collection-name
					test test-args
					bucket-keys (return-type 'list)
					)
  (fetch-store-items* store collection-name :test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type return-type))

(defmethod fetch-items ((collection collection) 
			&key test test-args
			  bucket-keys (return-type 'list)
					)
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type return-type))

(defun fetch-items* (store collection
		    &key test test-args 
		       bucket-keys (return-type 'list))
  (let ((items))    
    (when collection
      (let ((buckets (get-buckets-for-fetch store collection bucket-keys))) 
	(when buckets
	  (dolist (bucket buckets)
	    (setf items
		  (append
		   items
		   (if test
			(map return-type
			     (lambda (item)
			       (and (apply test item test-args) item))
			     (items bucket))
			(items bucket))))))))
    (remove-if #'not items)))

(defun fetch-store-items* (store collection-name 
		    &key test test-args 
		       bucket-keys (return-type 'list))
  (let ((collection (get-collection store collection-name)))
    
    (unless collection
      (setf collection (get-collection-from-def 
			store
			collection-name))
      (add-collection store collection))
    
    (fetch-items* store collection 
		  :test test
		  :test-args test-args
		  :bucket-keys bucket-keys 
		  :return-type return-type)))



