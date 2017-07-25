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
      (perist collection :def-only-p t)))
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

(defgeneric add-bucket (collection key-values &key load-hash-items-p))

(defun get-bucket* (collection key &key load-hash-items-p dont-load-items-p)
  (let ((bucket (get-bucket collection key)))    
    (unless bucket
      (setf bucket (add-bucket collection key :load-hash-items-p load-hash-items-p
			       :dont-load-items-p dont-load-items-p)))
    bucket))

(defgeneric add-data-type (store data-type))

(defmethod add-data-type ((store store) (data-type data-type))
  (unless (get-data-type store (name data-type))
    (setf (store data-type) store)
    (pushnew data-type (data-types store))
    (persist data-type))
  data-type)


(defun load-collection-items (collection load-hash-items-p)  
  (let ((files (directory (format nil "~A/**/*.log" (location collection))))
	(data-type (get-data-type (store collection)
				  (data-type collection))))
    
    (unless data-type
      (load-store-data-types (store collection)))
    
    (dolist (file files)     
      (load-items (universe (store collection)) file 
		  :load-hash-items-p load-hash-items-p))))

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
		    	(load-collection-items collection nil))))))))

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
			(make-instance 'store
				       :name (getf file-contents :name)
				       :location (getf file-contents :location)))))
	    
	    (load-store-data-types store)
	    (when (or with-collections-p with-items-p)
	      (load-store-collections store with-items-p))))))))


(defun write-to-file (file object &key (if-exists :append))
  (when (equalp (type-of object) 'item)
#|
    (break "~S~%~A" (list
		 :store (name (item-store object))
		 :collection (name (item-collection object))
		 :bucket-key (item-bucket-key object)
		 :hash (item-hash object)
		 :deleted-p (item-deleted-p object)
		 :values (item-values object))
	   (check-structs object)
	   )
 |#

    )
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
	      :bucket-key (item-bucket-key object)
	      :hash (item-hash object)
	      :deleted-p (item-deleted-p object)
	      :values (item-values object))
	     out)
	    (pprint object out)))
      (close out)
      ))

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
		:bucket-key (item-bucket-key object)
		:hash (item-hash object)
		:deleted-p (item-deleted-p object)
		:values (item-values object))
	       out)
	      (pprint object out)))
	(close out)
	)))

(defgeneric persist (object &key &allow-other-keys))

(defmethod persist ((list list) &key file (if-exists :append) &allow-other-keys)
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

(defun perist-collection-def (collection)
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

(defmethod perist ((collection collection) &key def-only-p &allow-other-keys)
  (perist-collection-def collection)
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
	(push (getf item-values (name field)) keys)))
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
    (setf (gethash hash (index (item-collection item))) item)))

(defun lookup-index (collection item-values)
  (let ((keys (index-keys (get-data-type 
			   (store collection)
			   (data-type collection))
			  item-values)))    
    (gethash (sxhash keys) (index collection))))

(defun remove-from-index (item)
  (remhash (item-hash item) (index (item-collection item))))



(defun parse-item-ref (universe item-ref &key load-hash-items-p)
    
  (let* ((store (get-store* universe (getf item-ref :store)))
	 (collection (get-collection* store (getf item-ref :collection)))
	 (bucket (get-bucket collection (getf item-ref :bucket-key))))
    
    (unless bucket
      (load-collection collection nil)
      (setf bucket (get-bucket collection (getf item-ref :bucket-key))))
  
    bucket))

(defun parse-line-to-item (universe item-def)
  (let* ((store (get-store* universe (getf item-def :store)))
	 (collection (get-collection* store (getf item-def :collection)))
	 (item (make-item
		:store store
		:collection collection
		:bucket-key (getf item-def :bucket-key)
		:hash (getf item-def :hash)
		:deleted-p (getf item-def :deleted-p)
		:values (getf item-def :values))))
    item))

(defvar *bucket-keys* nil)

(defun load-item-from-def (universe item-def &key load-hash-items-p)  
  (when (and item-def (not (getf item-def :deleted-p)))

    
    (let* ((item (parse-line-to-item universe item-def))
	   (lookup-item (lookup-index (item-collection item) (item-values item))))
      (unless lookup-item	
	(when load-hash-items-p
	  (let ((*bucket-keys* (make-hash-table :test 'equalp)))
	    (dolist (field (fields (get-data-type (store (item-collection item))
						  (data-type (item-collection item)))))
	      (let ((val (getf (item-values item) (name field))))
		(if (listp (first val))
		    (dolist (list-val val)
		      (when (getf list-val :hash%)
			(unless (gethash (getf list-val :bucket-key) *bucket-keys*)
			  (setf (gethash (getf list-val :bucket-key) *bucket-keys*) t)
			  (parse-item-ref universe list-val 
					  :load-hash-items-p
					  load-hash-items-p))))
		    (when (getf val :hash%)
		      (unless (gethash (getf val :bucket-key) *bucket-keys*)
			(setf (gethash (getf val :bucket-key) *bucket-keys*) t)
			(parse-item-ref universe val 
					:load-hash-items-p
					load-hash-items-p))))))))
	(let ((bucket (get-bucket* (item-collection item)
				   (item-bucket-key item)
				   :dont-load-items-p t)))
	  (add-index item)
	  (push item (items bucket))))
      (when lookup-item
	(unless (equalp (item-values lookup-item) (item-values item))

	  (push (item-values lookup-item) (item-versions lookup-item))
	  (setf (item-values lookup-item) (copy-list (item-values item)))
	  )
	))))


(defun load-items (universe filename &key load-hash-items-p)
  (with-open-file (in filename :if-does-not-exist :create)
     (with-standard-io-syntax              
       (when in
	 (loop for line = (read in nil)
	    while line
	    do (load-item-from-def
		universe line
		:load-hash-items-p load-hash-items-p))
	 (close in)))))

(defmethod add-bucket ((collection collection) key-values
		       &key load-hash-items-p dont-load-items-p)    
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
	
	(load-items (universe (store collection)) location 
		    :load-hash-items-p load-hash-items-p)))
    
 
    bucket))

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
		       (not (getf val :hash%))))
	(final-val))
	
	(if sub-p
	    (let* ((sub-store (if (getf (type-def field) :store)
				  (get-store* (universe (store collection))
					      (getf (type-def field) :store))
				  (store collection)))
		   (sub-collection (or (get-collection*  
					sub-store
					(getf (type-def field) :collection))
				       collection)) ;;praying its a self ref
		   (reference))
	      
	      
	      (when sub-collection
		(when (listp (first val))
		  (dolist (list-val val)
		    (setf reference 
			  (append reference
				  (list (item-val-reference sub-collection list-val))))))
		(unless (listp (first val))
		  (setf reference (item-val-reference sub-collection val)))	 
		(setf final-val (list (name field) reference)))
	      
	      ;;Most likely a major fuck up what??
	      (unless sub-collection
		(warn (format nil "Cant find the field collection at this time ~A" field))
		(setf reference val))
	      
	      (setf final-val (list (name field)
			    reference)))
	    
	    (setf final-val (list (name field)
				  val)))))

(defun parse-item-values-tree (collection item-values new-values)
  (let ((fields (fields (get-data-type (store collection) 
				       (data-type collection))))
	(parsed-item-values (check-structs item-values)))
    
    (dolist (field fields)
      (let ((val (if (equalp (type-of (getf new-values (name field)))
			     'item)
		     (item-values (getf new-values (name field)))
		     (getf new-values (name field)))))
	(if (or (equalp (getf (type-def field) :type) :item)
		(equalp (getf (type-def field) :complex-type) :collection)
		(equalp (getf (type-def field) :complex-type) :collection-items)
		(equalp (getf (type-def field) :complex-type) :list-items)
		(equalp (getf (type-def field) :complex-type) :contained-item)
		(equalp (getf (type-def field) :complex-type) :colletion-contained-item))
	    
	    (let ((shit (coerce-val-to-item-ref collection
						 field
						 val))
		  
		  )
	      (break "~A" shit)
		  (setf (getf parsed-item-values (name field))
		    shit)
	      )
	    (setf (getf parsed-item-values (name field))
		    val))))

    parsed-item-values))

(defmethod persist-item ((collection collection) item)
  (if (equalp (type-of item) 'item)
      (persist item :collection collection)
      (persist (make-item 
		:store (store collection)
		:collection collection
		:store (name (store collection))
		:collection (name collection)
		:values item))))


(defun check-location (item &key collection)
  (unless (and (item-store item) (item-collection item))
    (unless (or collection (item-collection item))
      (error (format nil "Dont know where to persist item ~S" item)))

    (let ((col (or collection (item-collection item))))
      (when col

	(setf (item-store item) (store col))
	(unless (store col)
	  (error (format nil "Dont know which store to use to persist item ~S" item)))
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
	(setf (item-bucket item) (get-bucket-from-keys (item-collection item) 
						       (item-bucket-key item)))
	(setf (item-bucket item) (first (buckets (item-collection item)))))
    
    (unless (item-bucket item)
      
      (setf (item-bucket item) 
	    (add-bucket (item-collection item) 
			(item-bucket-key item)))))
  item)


(defun key-change-check (item)
  (let ((hash (sxhash (index-keys (item-collection item) 
				  (item-values item)))))
    
    (if (and (item-hash item) 
	     (not (equalp hash (item-hash item))))
	(error (format nil "Cant change key values causes hash diff ~%~A~%~A~%~A" 
		       (item-hash item)
		       hash
		       (item-values item)))
	(when (item-changes item)
	    (let ((new-hash (sxhash (index-keys (item-collection item) 
						(item-changes item)))))
	      (when (item-hash item)
		(unless (equalp new-hash (item-hash item))
		  (error (format nil "Cant change key values~%~A~%~A" 
				 (item-values item)
				 (item-changes item))))))))
    item))


(defun change-in-item-p (item)
  (not (equalp (item-values item) (item-changes item))))

(defun check-item (item)
  (when (key-change-check item)    
	(when (and (item-changes item) (change-in-item-p item)) 
	  (setf (item-values item)
		(parse-item-values-tree (item-collection item)
					(item-values item) (item-changes item)))
	  (setf (item-changes item) nil))
	(unless (item-changes item)
	  (setf (item-values item)
		(parse-item-values-tree (item-collection item)
					(item-values item) (item-values item)))))
  
  (let ((lookup-item (lookup-index (item-collection item) (item-values item))))
    ;;Dont save crap twice
    (when lookup-item
      (when (equalp (item-values lookup-item) (item-values item))
	(setf item nil)))
    
    (unless lookup-item
      (let ((bucket (get-bucket (item-collection item) (item-bucket-key item))))
	
	(push item (items bucket))
	(add-index item))))
  
  
  item)

(defun parse-item (item)
  (loop for (a b) on item by #'cddr ;;while b 
        :collect (list a b)))


(defun struct-to-val (val)
  (let ((final))
    (when (equalp (type-of val) 'item)
 ;;     (break "~A" val)
      (let ((it (or (item-changes val) 
		    (item-values val))))
	(setf final (check-structs it))))
    (unless (equalp (type-of val) 'item)
      (setf final val))
    ;;(break "wtf ~A" final)
    final))


(defun check-children (key val)
  (let ((final))
    (if val
	(if (and val (listp val))
	    (let (children)
	      (dolist (it val)
		(setf children (append children (list key (struct-to-val it)))))
	      (if children
		  (setf final (append final (list key children)))))
	    (setf final (append final (list key (struct-to-val val)))))
	(setf final (list key val)))
    final))

(defun check-structs (item)
  (let ((final))
    (dolist (pair (parse-item item))
     ;; (break "~A" pair)
      (let ((key (first pair))
	    (val (second pair)))
	  (setf final (append final (check-children key val))))) 
    (unless final
      (break "hoer ~S" item)
      )
     final))

(defun check-item-structs (item)
  (if (item-changes item)
      (setf (item-changes item) (check-structs (item-changes item)))
      (setf (item-values item) (check-structs (item-values item)))
      )
  item)

(defmethod persist ((item item) &key collection file &allow-other-keys)
  (let ((derived-file))
    (setf (item-persisted-p item) nil)
    (setf item (check-item-structs item))
    (unless file
      (setf item (check-location item :collection collection))
      (setf derived-file
	    (location (item-bucket item))))
    (let ((item-to-persist (check-item item)))
      
      (when item-to-persist
	;;(break "~A" item-to-persist)
	    (write-to-file (or file derived-file)
			   item-to-persist
			   :if-exists :append)
	    (setf (item-persisted-p item) t)))
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

(defun load-collection (collection load-hash-items-p)
  (load-collection-items collection load-hash-items-p))


(defun load-store (store load-hash-items-p)
  (load-store-data-types store)
  (load-store-collections store t)
  (dolist (collection (collections store))
    (load-collection-items collection load-hash-items-p)))

(defun get-buckets-for-fetch (collection bucket-keys load-hash-items-p)
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
					bucket-key
					:load-hash-items-p load-hash-items-p))))
	   
	   (pushnew bucket buckets))))
    (unless bucket-keys
      (let ((bucket (get-bucket collection
				(name collection))))
	(unless bucket
	  (setf bucket
		(add-bucket collection nil
			    :load-hash-items-p load-hash-items-p)))
	(pushnew bucket buckets)))
    
    buckets))


(defun fetch-items* (store collection
		    &key test test-args 
		      bucket-keys (return-type 'list)
		      load-hash-items-p
		      find-first-item-p)
  (let ((items)) 
    (unless (data-types store)
      (load-store-data-types store))
    (when collection
      (let ((buckets (get-buckets-for-fetch collection 
					    bucket-keys load-hash-items-p))) 
	
	(when buckets
	  (dolist (bucket buckets)
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
		      bucket-keys (return-type 'list)
		      load-hash-items-p)
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
		  :return-type return-type
		  :load-hash-items-p load-hash-items-p)))



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
			  bucket-keys (return-type 'list)
			  load-hash-items-p)
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type return-type
		:load-hash-items-p load-hash-items-p))

(defmethod fetch-item ((collection collection) 
			&key test test-args
			  bucket-keys 
			  load-hash-items-p)
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:bucket-keys bucket-keys 
		:return-type nil
		:load-hash-items-p load-hash-items-p
		:find-first-item-p t
		))

(defun find-refs (values)
  (let (refs)
    (dolist (val values)
      (when val
	(when (listp val)
	  (when (getf val :hash%)
	    (push val refs)))))
    refs))


(defun getx (item field-name &key resolve-universe)
  (let* ((val (getf (item-values item) field-name))
	 (final-val val))
    (when resolve-universe
      (let ((refs (find-refs (item-values item))))
	(dolist (ref refs)
	  (let ((bucket (parse-item-ref resolve-universe ref 
					:load-hash-items-p t)))
	  
	    (when bucket
	      (let ((ref-item (gethash (getf ref :hash%) 
				       (index (collection bucket)) )))
		(when ref-item
		  (setf final-val (item-values ref-item)))))))))
    final-val))

(defun (setf getx) (value item field-name &key (change-control-p t))
  (when change-control-p
      (unless (item-changes item)
	(setf (item-changes item) (copy-list (item-values item))))  
      (setf (getf (item-changes item) field-name) value))
  (unless change-control-p
    (setf (getf (item-values item) field-name) value)))
