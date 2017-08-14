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
	     :initform nil)
   (loaded-p :initarg :loaded-p
	  :accessor loaded-p
	  :initform nil)))

(defclass collection ()
  ((store :initarg :store
	  :accessor store
	  :initform nil)
   (name :initarg :name
	 :accessor name)
   (name-space :initarg :name-space
	       :accessor name-space
	       :initform nil)
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
  store
  collection
  data-type
  bucket
  hash
  bucket-key
  values
  changes
  versions
  deleted-p
  persisted-p)

(defgeneric get-store (universe storn-name))

(defmethod get-store ((universe universe) store-name)  
  (dolist (store (stores universe))
    (when (string-equal store-name (name store))
      (return-from get-store store))))

(defgeneric get-data-type (store type-name))

(defmethod get-data-type ((store store) type-name)
  (dolist (data-type (data-types store))
    (when (string-equal type-name (name data-type))
      (return-from get-data-type data-type))))

(defgeneric get-collection (store collection-name))

(defmethod get-collection ((store store) collection-name)
   (dolist (collection (collections store))
     (when (string-equal collection-name (name collection))
       (return-from get-collection collection))))

(defgeneric get-bucket (collection bucket-key))

(defmethod get-bucket ((collection collection) bucket-key)
  (dolist (bucket (buckets collection))
    (when (equalp bucket-key (key-values bucket))
      (return-from get-bucket bucket))))

(defgeneric add-store (universe store))

(defmethod add-store ((universe universe) (store store))
  (unless (get-store universe (name store))
    (when (location store)
      (ensure-directories-exist (location store)))
    (unless (location store)
      (let ((location (format nil "~A~A/" 
			      (location universe) 
			      (name store))))
	(ensure-directories-exist location)
	(setf (location store) location)))
    
    (setf (universe store) universe)
    (persist store)
    (pushnew store (stores universe)))
  store)

(defun get-store* (universe name)
  (let ((store (get-store universe name)))
    (unless store
      (setf store (get-store-from-def universe name))
      (load-store-data-types store)
      (add-store universe store))
    store))

(defgeneric add-collection (object collection))

(defmethod add-collection ((store store) (collection collection))
  (unless (get-collection store (name collection))    
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
      (persist collection :def-only-p t)))
  collection)

(defun get-collection* (store name)
  
  (let ((collection (get-collection store name)))
    (unless collection
      (setf collection (get-collection-from-def store name))
      (add-collection store collection))
    collection))

(defun bucket-location (collection keys)
  (if (bucket-keys collection)
      (get-bucket-key-val-location
       collection
       keys)
      (format nil "~A/~A.log" 
	      (location collection)
	      (name collection))))

(defgeneric add-bucket (collection key-values &key ))

(defun get-bucket* (collection key &key  dont-load-items-p)
  (let ((bucket (get-bucket collection key)))    
    (unless bucket
      (setf bucket (add-bucket collection key 
			       :dont-load-items-p dont-load-items-p)))
    bucket))

(defgeneric add-data-type (store data-type))

(defmethod add-data-type ((store store) (data-type data-type))
  (unless (get-data-type store (name data-type))
    (setf (store data-type) store)
    (pushnew data-type (data-types store))
    (persist data-type))
  data-type)

(defun load-collection-items (collection)  
  (let ((files (directory (format nil "~A/**/*.log" (location collection))))
	(data-type (get-data-type (store collection)
				  (data-type collection))))
 
    (unless data-type
      (load-store-data-types (store collection)))
    
    (dolist (file files)     
      (load-items (universe (store collection)) file))))

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
			(list (make-instance
			       'field
			       :name (getf field :name)
			       :key-p (getf field :key-p)
			       :type-def (getf field :type-def)
			       :attributes (getf field :attributes))))))
	(add-data-type 
	 store 
	 (make-instance 'data-type
			:name (getf type-contents :name)
			:label (getf type-contents :label)
			:top-level-p (getf type-contents :top-level-p)
			:fields fields))))))

(defun load-store-collections (store with-items-p)  
  (let ((files (directory (format nil "~A**/*.col" (location store)))))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	
	(when file-contents
	  (let ((collection
		 (add-collection 
		  store 
		  (make-instance 'collection
				 :name (getf file-contents :name)
				 :location (getf file-contents :location)
				 :data-type (getf file-contents :data-type)))))
	    (when with-items-p
	      (load-collection-items collection))))))))

(defun load-stores (universe with-collections-p with-items-p)
  (let ((files (directory (format nil "~A**/*.store" (location universe)))))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	(when file-contents
	  (let ((store (add-store 
			universe
			(make-instance
			 'store
			 :name (getf file-contents :name)
			 :location (getf file-contents :location)))))
	    
	    (load-store-data-types store)
	    (when (or with-collections-p with-items-p)
	      (load-store-collections store with-items-p))))))))


(defun write-to-file (file object &key (if-exists :append))
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (if (equalp (type-of object) 'item)
	  (pprint 
	   (list
	    :store (name (item-store object))
	    :collection (name (item-collection object))
	    :data-type (item-data-type object)
	    :bucket-key (item-bucket-key object)
	    :hash (item-hash object)
	    :deleted-p (item-deleted-p object)
	    :values (item-values object))
	   out)
	  (pprint object out)))
    (close out)))

(defun write-list-to-file (file list &key (if-exists :append))
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (dolist (object list)
	(if (equalp (type-of object) 'item)
	    (pprint 
	     (list
	      :store (name (item-store object))
	      :collection (name (item-collection object))
	      :data-type (item-data-type object)
	      :bucket-key (item-bucket-key object)
	      :hash (item-hash object)
	      :deleted-p (item-deleted-p object)
	      :values (item-values object))
	     out)
	    (pprint object out)))
      (close out))))

(defgeneric persist (object &key &allow-other-keys))

(defmethod persist ((list list) &key file (if-exists :append)
				  &allow-other-keys)
  (write-list-to-file file
		      list
		      :if-exists if-exists))

(defmethod persist ((store store) &key &allow-other-keys)
  (write-to-file (format nil "~A~A.store" (location store) (name store))
		 (list :name (name store)
		       :location (location store))
		 :if-exists :supersede))

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

(defun persist-collection-def (collection)
  (write-to-file (format nil "~A~A.col" (location (store collection))
			 (name collection))
		 (list 
		  :name (name collection)
		  :name-space (name-space collection)
		  :location (location collection)
		  :data-type (data-type collection)
		  :bucket-keys (bucket-keys collection))
		 :if-exists :supersede))


(defun persist-collection (collection)
  (dolist (bucket (buckets collection))
    (persist (items bucket) :file (location bucket))))

(defmethod persist ((collection collection) &key def-only-p &allow-other-keys)
  (persist-collection-def collection)
  (unless def-only-p
    (persist-collection collection)))

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
	(if (equalp (type-of (getf item-values (name field))) 'item)
	    (push (item-hash (getf item-values (name field))) keys)
	    (push (getf item-values (name field)) keys))))
    (reverse keys)))

(defun remove-item (item)
  (flet ((rem-item (item-struct)
	   (equalp (item-hash item) (item-hash item-struct))))
    (remove-from-index item)
    (setf (items (item-bucket item))
	  (remove-if #'rem-item (items (item-bucket item))))
    item))

(defun add-index (item)
  (let* ((keys (index-keys (get-data-type 
			    (item-store item)
			    (data-type (item-collection item)))
			   (item-values item)))
	 (hash (sxhash keys)))
    (setf (item-hash item) hash)
    (setf (gethash hash (index (item-collection item))) item)))

(defun lookup-index (collection item-values)
  (let ((keys (index-keys (get-data-type 
			   (store collection)
			   (data-type collection))
			  item-values)))
    (gethash (sxhash keys) (index collection))))

(defun remove-from-index (item)
  (remhash (item-hash item) (index (item-collection item))))

(defun load-item-reference-bucket (universe item-ref &key dont-load-items-p)
  (let* ((store (get-store* universe (getf item-ref :store)))
	 (collection (get-collection* store (getf item-ref :collection)))
	 (bucket (get-bucket collection (getf item-ref :bucket-key))))
    
    (unless bucket
      (add-bucket collection (getf item-ref :bucket-key)
		  :dont-load-items-p dont-load-items-p)
      (setf bucket (get-bucket collection (getf item-ref :bucket-key))))
    
    bucket))

(defun find-key (value key)
  (find key value :test #'equalp))

(defun resolve-item-values (universe collection values)
  (let ((final)
	(value-pairs (parse-item values)))
    
    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))

	(if (and (not (atom val))
		 (listp val))	    
	    (if (find-key val :values)
		(if (dig val :values :reference%)
		    (setf final
			  (append final
				  (list key
					(resolve-item-reference
					 universe val
					 :dont-load-items-p
					 ;;Unless hierarchy of same object type
					 ;;load reference collection
					 (if (equalp (name collection)
						     (getf val :collection))
					     t
					     nil)))))
		    (setf final
			  (append final
				  (list key
					(make-item
					 :data-type (dig val :data-type)
					 :hash (dig val :hash)
					 :values
					 (resolve-item-values
					  universe
					  collection
					  (dig val :values)))))))
		(if (and (not (atom (first val)))
			 (listp (first val)))
		    (let ((children))
		      (dolist (it val)
			(if (and (not (atom it))
				 (listp it))
			    (if (find-key it :values)
				(if (dig it :values :reference%)
				    (setf children
					  (append
					   children
					   (list
					    (resolve-item-reference
					     universe it
					     :dont-load-items-p
					     ;;Unless hierarchy of same object
					     ;;load reference collection
					     (if (equalp (name collection)
						     (getf it :collection))
					     t
					     nil)))))
				    (setf children
					  (append
					   children
					   (list (make-item
						  :data-type (dig it :data-type)
						  :hash (dig it :hash)
						  :values
						  (resolve-item-values
						   universe
						   collection
						   (dig it :values)))))))
				(append children (list it)))))		  
		      (setf final (append final (list key children))))
		    (setf final (append final (list key val)))))
	    (setf final (append final (list key val)))


	    
	    )))
    final))

(defun resolve-item-reference (universe reference &key dont-load-items-p)
  (let ((bucket (load-item-reference-bucket
		 universe reference
		 ;;Don't load if loading file but if it is value references load
		 ;;those collections so that reference lookup works correct.
		 :dont-load-items-p dont-load-items-p))
	(final-item))
    
    (when bucket
      (let ((ref-item (gethash (dig reference :hash) 
			       (index (collection bucket)))))
	(when ref-item
	  (unless (getf reference :deleted-p)
	    

	    (unless (dig (getf reference :values) :reference%)
	      
	      (let ((resolved-values
		     (resolve-item-values universe (collection bucket)
					  (getf reference :values))))

		(unless (equalp (item-values ref-item) resolved-values)
		  (push  (item-values ref-item) (item-versions ref-item))		
		  (setf (item-values ref-item) resolved-values))))	  	    
	    (setf final-item ref-item))
	  
	  (when (getf reference :deleted-p)
	    (remove-item ref-item)
	    (setf final-item nil)))
	
	
	(unless ref-item
	  (unless (getf reference :deleted-p)
	    (setf final-item
		  (make-item
		   :store (store (collection bucket))
		   :collection (collection bucket)
		   :data-type (getf reference :data-type)
		   :bucket bucket
		   :bucket-key (getf reference :bucket-key)
		   :values (resolve-item-values universe
						(collection bucket)
						(getf reference :values))))

	    (when (getf (item-values final-item) :reference%)
	      (write-to-file "~/data-universe/error.log"
			     (list "Could not resolve ~S" reference)))
	    
	    (unless (getf (item-values final-item) :reference%)
	      (push final-item (items bucket))
	      (add-index final-item))))


	(if final-item
	    final-item
	    (unless (getf reference :deleted-p)
		(write-to-file "~/data-universe/error.log"
			       (list "Could not resolve ~S" reference))
		nil))))))

(defun load-items (universe filename )
  (with-open-file (in filename :if-does-not-exist :create)
    (with-standard-io-syntax              
      (when in
	(loop for line = (read in nil)
	   while line
	   do (resolve-item-reference
	       universe line
	       :dont-load-items-p t))
	(close in)))))

(defmethod add-bucket ((collection collection) key-values
		       &key dont-load-items-p)    
  (let* ((location (bucket-location collection key-values))
	 (bucket  (get-bucket collection key-values)))

    (unless bucket
      (setf bucket (make-instance 'bucket 
				  :key-values key-values
				  :location location))
      
      (ensure-directories-exist location)
      
      (setf (buckets collection) (append (buckets collection)
					 (list bucket)))  
      
      (setf (collection bucket) collection)
      (unless dont-load-items-p
	(unless (loaded-p bucket)
	  (load-items (universe (store collection)) location)
	  (setf (loaded-p bucket) t))))
    bucket))

(defgeneric persist-item (collection item))

(defmethod persist-item ((collection collection) item)

  (if (equalp (type-of item) 'item)
      (persist item :collection collection)
      (persist (make-item 
		:store (store collection)
		:collection collection
		:data-type (data-type collection)
		:values item))))

(defun check-location (item &key collection)
  (unless (and (item-store item) (item-collection item))
    (unless (or collection (item-collection item))
      (error (format nil "Dont know where to persist item ~S" item)))

    (let ((col (or collection (item-collection item))))
      (when col

	(setf (item-store item) (store col))
	(unless (store col)
	  (error
	   (format nil
		   "Dont know which store to use to persist item ~S" item)))
	(setf (item-collection item) col)
	(setf (item-bucket-key item) 
	      (get-key-values (bucket-keys col) 
			      (item-values item))))))
  
  (when (and (bucket-keys (item-collection item) )
	     (not (item-bucket-key item)))
    (setf (item-bucket-key item) 
	  (get-key-values (bucket-keys (item-collection item)) 
			  (item-values item))))
  
  (unless (item-bucket item)
    (if (bucket-keys (item-collection item))
	(setf (item-bucket item)
	      (get-bucket-from-keys (item-collection item)
				    (item-bucket-key item)))
	(setf (item-bucket item) (first (buckets (item-collection item)))))
    
    (unless (item-bucket item)
      (setf (item-bucket item) 
	    (add-bucket (item-collection item) 
			(item-bucket-key item)))))
  item)




(defun parse-item (item)
  (loop for (a b) on item by #'cddr 
     :collect (list a b)))

(defun set-hash (store item)
  (unless (item-hash item)
    (let* ((keys (index-keys (get-data-type 
			      (or (item-store item) store)
			      (item-data-type item))
			     (item-values item)))
	   (hash (sxhash keys)))
      (setf (item-hash item) hash)
      hash)))

(defun item-to-reference (store item)
  (if (equalp (type-of item) 'item)
      (if (item-collection item)
	  (list
	   :store (name (item-store item))
	   :collection (name (item-collection item))
	   :data-type (item-data-type item)
	   :bucket-key (item-bucket-key item)
	   :hash (item-hash item)
	   ;;	   :deleted-p (item-deleted-p item)
	   :values 
	   '(:reference% t))
	  (list
	     ;; :store (name (item-store item))
	     :data-type (item-data-type item)
	     :hash (or (item-hash item) (set-hash store item ))
	     :values (parse-to-references% store (or (item-changes item)
						     (item-values item)))))
      item))

(defun parse-to-references% (store values)
  (let ((final)
	(value-pairs (parse-item values)))
    
    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))

	(if (equalp (type-of val) 'item)
	    (setf final (append final (list key (item-to-reference store val))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (equalp (type-of (first val)) 'item)))
		(let ((children))
		  (dolist (it val)
		    (if (equalp (type-of it) 'item)
			(setf children
			      (append children 
				      (list (item-to-reference store it))))
			(setf children (append children (list it)))))
		  (setf final (append final (list key children))))
		(setf final (append final (list key val)))))))
    final))


(defun copy% (item)
  (let ((copy (copy-item item)))
    (setf (item-values copy) (copy-list (item-values item)))
    (setf (item-changes copy) (copy-list (item-changes item)))
    copy))

(defun parse-to-references (store item)
  (let ((ref-item (copy% item)))
    (setf (item-values ref-item)
	  (parse-to-references% store (item-values ref-item)))
    ref-item))

;;Used to mark change deep in the bowls of the beast.
(defvar *persist-p* nil)

(defun check-no-collection-val (store val)
  (unless (item-changes val)
    (setf (item-values val)
	  (check-item-values% store
	   (item-values val)))
    val)
  (when (item-changes val)
    (setf *persist-p* t)
    (setf (item-values val)
	  (check-item-values% store
	   (item-changes val)))
    (setf (item-changes val) nil)
    val)

  (unless (item-hash val)
    (set-hash store val))
  
  val)

(defun check-item-values% (store values)
  (let ((final)
	(value-pairs (parse-item values)))

    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))
	
	(if (equalp (type-of val) 'item)
	    (setf final (append final
				(list key
				      (if (item-collection val)
					  (persist val)
					  (check-no-collection-val store val)))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (equalp (type-of (first val)) 'item)))
		(let ((children))
		  (dolist (it val)
		    (if (equalp (type-of it) 'item)
			(setf children
			      (append children 
				      (list
				       (if (item-collection it)
					   (persist it)
					   (check-no-collection-val store it)))))
			
			(setf children (append children (list it)))))
		  (setf final (append final (list key children))))
		(setf final (append final (list key val)))))))     
    final))


(defun change-in-item-p (item)
  (not (equalp (item-values item) (item-changes item))))

(defun check-item-values (item allow-key-change-p)
  (let ((change-p (and (item-changes item) (change-in-item-p item)))
	(lookup-old (lookup-index (item-collection item) (item-values item)))
	(final-item))

    
    (when change-p
      (when lookup-old
	(let ((lookup-new (lookup-index (item-collection item)
					(item-changes item))))	  
	  (when lookup-new
	    (when (equalp (item-hash lookup-new) (item-hash lookup-old))
	      (unless (equalp (item-values lookup-new) (item-changes item))
		(setf (item-values lookup-old)
		      (check-item-values% (item-store item) (item-changes item)))
		(setf (item-changes item) nil)
		(setf final-item lookup-old)))
	    
	    (unless (equalp (item-hash lookup-new) (item-hash lookup-old))
	      (unless allow-key-change-p
		(error
		 (format
		  nil
		  "Cant change key values causes hash diff ~%~A~%~A~%~A~%~A" 
		  (item-hash lookup-old)
		  (item-hash item)
		  (item-values lookup-old)
		  (item-values item))))
	      
	      (when allow-key-change-p
		(push (item-values lookup-old) (item-versions lookup-old))
		(setf (item-values lookup-old)
		      (check-item-values% (item-store item)
					  (item-changes item)))
		(remove-item lookup-old)	      
		(push item (items (item-bucket lookup-old)))
		(add-index lookup-old)
		(setf final-item lookup-old))))
	  
	  (unless lookup-new
	    (setf (item-values lookup-old)
		  (check-item-values% (item-store item) (item-changes item)))
	    (setf (item-changes item) nil)
	    (setf final-item lookup-old))))
      
      (unless lookup-old
	(let ((lookup-new (lookup-index (item-collection item)
					(item-changes item))))
	  (when lookup-new
	    (unless (equalp (item-values lookup-new) (item-changes item))
		(setf (item-values lookup-new)
		      (check-item-values% (item-store item) (item-changes item)))
		(setf (item-changes lookup-new) nil)
		(setf final-item lookup-new)))
	  
	  (unless lookup-new
	     (setf (item-values item)
		    (check-item-values% (item-store item) (item-changes item)))
	     (setf (item-changes item) nil)	   
	     (push item (items (item-bucket item)))
	     (add-index item)
	     (setf final-item item)))))

    (unless change-p
      
      (when lookup-old
	(let ((wtf (check-item-values% (item-store item)
				       (or (item-changes item)
					   (item-values item)))))

	  (if (equalp (item-values lookup-old) (or (item-changes item)
						   (item-values item)))
	      (progn
		(when *persist-p*
		  (push (item-values lookup-old) (item-versions lookup-old))
		  (setf (item-values lookup-old) wtf)		
		  (setf (item-changes item) nil)
		  (setf final-item lookup-old))
		
		;;Don't save nothing changed
		(unless *persist-p*
		  (setf (item-changes lookup-old) nil)
		  (setf (item-changes item) nil)
		  (setf final-item nil)))
	      (progn
		(push (item-values lookup-old) (item-versions lookup-old))
		(setf (item-values lookup-old) wtf)		
		(setf (item-changes item) nil)
		(setf final-item lookup-old)))))
      
      (unless lookup-old
	 (setf (item-values item)
	       (check-item-values% (item-store item)
				   (or (item-changes item)
				       (item-values item))))
	 (setf (item-changes item) nil)	   
	 (push item (items (item-bucket item)))
	 (add-index item)
	 (setf final-item item)))
    
    (when final-item
      (setf (item-deleted-p final-item) (item-deleted-p item)))
    
    final-item))

(defmethod persist ((item item) &key collection file allow-key-change-p
				  &allow-other-keys)

  (let ((*persist-p* nil)
	(derived-file))
  
    (unless file
      ;;Resolve the location of the item
      (setf item (check-location item :collection collection))
      (setf derived-file (location (item-bucket item))))

    ;;Only persist if the item has changed
    (let ((changed-item (check-item-values item allow-key-change-p)))
      (when changed-item
	(setf item changed-item)

	;;Parse item to persistable format
	(let ((item-to-persist (parse-to-references (item-store item) item)))
	  (when item-to-persist
	    (setf (item-persisted-p item) nil)
	    (write-to-file (or file derived-file)
			   item-to-persist
			   :if-exists :append)
	    (setf (item-persisted-p item) t)))))
    item))

(defun get-store-from-def (universe store-name)
  (let ((filename (format nil "~A~A/~A.store" 
			  (location universe) store-name store-name))
	(store-def)
	(store))    
    
    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf store-def (read in nil))
	  (close in))))
    
    (when store-def
      (setf store
	    (make-instance 'store		     
			   :name (getf store-def :name)		    
			   :location (getf store-def :location)))
      (load-store-data-types store))
    store))

(defun get-collection-from-def (store collection-name)
  (let ((filename (format nil "~A~A.col" (location store) collection-name))
	(collection-def))    
    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf collection-def (read in nil))
	  (close in))))
    (when collection-def
      (make-instance 'collection
		     :store store
		     :name (getf collection-def :name)
		     :bucket-keys (getf collection-def :bucket-keys)
		     :location (getf collection-def :location)
		     :data-type (getf collection-def :data-type)))))

(defun load-collection (collection)
  (load-collection-items collection))


(defun load-store (store)
  (load-store-data-types store)
  (load-store-collections store t)
  (dolist (collection (collections store))
    (load-collection-items collection)))

(defun get-buckets-for-fetch (collection bucket-keys)
  (let ((buckets))
    (when bucket-keys
      (dolist (bucket-key bucket-keys)
	(let ((bucket (get-bucket collection
				  bucket-key)))
	  
	  (unless bucket
	    (let ((file-path (location collection)))
	      (dolist (key bucket-key)
		(setf file-path (format nil "~A/~A" file-path key)))
	      (setf bucket (add-bucket collection 				 
				       bucket-key))))
	  
	  (pushnew bucket buckets))))
    (unless bucket-keys
      (let ((bucket (get-bucket collection
				(name collection))))
	(unless bucket
	  (setf bucket
		(add-bucket collection nil)))
	(pushnew bucket buckets)))
    
    buckets))


(defun fetch-items* (store collection
		     &key test test-args 
		       bucket-keys (return-type 'list)
		       find-first-item-p)
  (let ((items)) 
    (unless (data-types store)
      (load-store-data-types store))
    (when collection
      (let ((buckets (get-buckets-for-fetch collection bucket-keys))) 
	
	(when buckets
	  (dolist (bucket buckets)
	   ;; (break "~A" bucket)
	    ;;last ditch attempt to load collection if not loaded
	    (unless (items bucket)
	      (load-collection-items collection))
	    
	    

	    (setf items
		  (append
		   items
		   (if test
		       (map return-type
			    (lambda (item)
			      (if find-first-item-p
				  (when (apply test item test-args)
				    (return-from fetch-items* item))
				  (and (apply test item test-args) item)))
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

(defgeneric fetch-items (object &key test test-args
				  bucket-keys return-type
				  &allow-other-keys))

(defmethod fetch-items ((store store) &key collection-name
					test test-args
					bucket-keys (return-type 'list))
  (fetch-store-items* store collection-name :test test
		      :test-args test-args
		      :bucket-keys bucket-keys 
		      :return-type return-type))

(defmethod fetch-items ((collection collection) 
			&key test test-args
			  bucket-keys (return-type 'list))
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type return-type))

(defgeneric fetch-item (collection &key test test-args bucket-keys))

(defmethod fetch-item ((collection collection) 
		       &key test test-args
			 bucket-keys)
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type nil	
		:find-first-item-p t))


(defun dig-down (place indicators)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))
    
    (if indicators
	(dig-down next-place indicators)
	next-place)))

(defun set-dig-down (place indicators value)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))
    (if indicators	
	(setf (getf place indicator) 
	      (set-dig-down next-place indicators value))
	(setf (getf place indicator) value))
    place))

(defun dig (place &rest indicators)
  (dig-down place indicators))

(defun (setf dig) (value place &rest indicators)
  (set-dig-down place indicators value))


(defgeneric getx (item field-name))

(defmethod getx ((item item) field-name)
  (getf (item-values item) field-name))

(defgeneric (setf getx) (value item field-name &key change-control-p))

(defmethod (setf getx) (value (item item) field-name &key (change-control-p t))
  (when change-control-p
    
    (unless (item-changes item)
      (setf (item-changes item) (copy-list (item-values item))))
    
    (setf (getf (item-changes item) field-name) value))
  (unless change-control-p
    (setf (getf (item-values item) field-name) value)))


(defun naive-dig (place indicators)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (equalp (type-of place) 'item)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (equalp (type-of val) 'item)
			 (item-values val)
			 val)))
    
    (if indicators
	(naive-dig next-place indicators)
	(if (equalp (type-of place) 'item)
	    (getx place indicator)
	    (getf place indicator)))))

(defun set-naive-dig (place indicators value)
  
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (equalp (type-of place) 'item)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (equalp (type-of val) 'item)
			 (if (item-changes val)
			     (item-changes val)
			     (setf (item-changes val)
				   (copy-list (item-values val))))
			 val)))
    (if indicators
	(if (equalp (type-of val) 'item)
	    (set-naive-dig next-place indicators value)
	    (setf (getf place indicator) 
		  (set-naive-dig next-place indicators value)))
	(if (equalp (type-of place) 'item)
	    (setf (getx place indicator) value)
	    (setf (getf place indicator) value)))
    place))

(defun digx (place &rest indicators)
  (naive-dig place indicators))

(defun (setf digx) (value place &rest indicators)
  (set-naive-dig place indicators value))
