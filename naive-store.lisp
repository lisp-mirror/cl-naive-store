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
	  :initform (make-hash-table :test 'equalp))
   (key-index :initarg :key-index
	  :accessor key-index
	  :initform (make-hash-table :test 'equalp))
   (filter :initarg :filter
	   :accessor filter
	   :initform nil)))

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

(defstruct blob
  file-type
  file-ext
  location
  raw)


(defun read-file-to-string (file)
  (let ((*read-eval* nil)
	(out (make-string-output-stream))
	(str ""))

    (with-open-file (in file
			:direction :input		     
			:if-does-not-exist nil)
      (when in
	(loop for line = (read-line in nil)
	   while line do (write-line line out) )
	(close in))
      (setf str (get-output-stream-string out))
      (close out))
    
    str))

(defun blob-string-value (blob)
  (if (blob-p blob)
      (if (empty-p (blob-raw blob))
	  (if (not (empty-p (blob-location blob)))
	      (read-file-to-string (blob-location blob)))
	  (blob-raw blob))
      blob))


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


(defun item-list-p (list)
  (and (listp list)
       (item-p (first list))))

(defun item-of-type-p (item data-type)
  (string-equal data-type (item-data-type item)))

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
      
      (when collection
	(add-collection store collection)
	)
      (unless collection
	  (error "Could not create collection ~A" name)))
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

(defun file-loaded (collection file)
  (let ((loaded-p))
    (dolist (bucket (buckets collection))
      (when (search (remove #\~ (location bucket)) (namestring file))
	(when (and (loaded-p bucket)
		   (equalp (file-write-date file)
			   (loaded-p bucket)))
	  (setf loaded-p t))))
    loaded-p))

(defun load-collection-items (collection)  
  (let ((files (directory (format nil "~A/**/*.log" (location collection))))
	(data-type (get-data-type (store collection)
				  (data-type collection))))
    
    (unless data-type
      (load-store-data-types (store collection)))

    (dolist (file files)
      (unless (file-loaded collection file)	
	(load-items (universe (store collection)) file)))))

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
				 :data-type (getf file-contents :data-type)
				 :filter (getf file-contents :filter)))))
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
      (if (item-p object)
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
	(if (item-p object)
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
  (unless data-type
    ;;Raising an error here because its problem with datatype specifications some where.
    (error "index-keys called with data-type = nil. If this happened on a save look for a mismatch between a collection and its data-type's destinations"))
  (index-keys (fields data-type) item-values))

(defmethod index-keys ((fields list) item-values)
  (let ((keys))
    (dolist (field fields)     
      (when (key-p field)
	(if (item-p (getf item-values (name field)))
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

(defun key-sxhash (store data-type values)
  (let* ((data-type-spec (get-data-type 
			    store
			    data-type))
	(keys (if data-type-spec
		  (index-keys data-type-spec
			      values)
		  (error "index-keys called with data-type = nil. If this happened on a save look for a mismatch between a collection and its data-type's destinations."))))
    
    (sxhash keys)))

(defun add-index (item)
  (let* ((hash (uuid:make-v4-uuid))
	 (hashx (key-sxhash (item-store item)
			    (item-data-type item)
			    (item-values item) )))

    (when (or (not (item-hash item))
	      (string-equal (format nil "~A" (item-hash item))
			    (format nil "~A" hashx)))
      
      (setf (item-hash item) hash))    
    
    (setf (gethash hashx (key-index (item-collection item))) item)
    (setf (gethash (item-hash item) (index (item-collection item))) item)))

(defun lookup-index (collection item-values)
  (gethash (key-sxhash (store collection)
		       (data-type collection)
		       item-values)
	     (key-index collection)))

(defun lookup-index-hash (collection hash)
  (gethash hash (index collection)))

(defun remove-from-index (item)
  (remhash (key-sxhash (item-store item)
			 (data-type (item-collection item))
			 (item-values item) )

	   (key-index (item-collection item)))
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

(defparameter *load-buckets* nil)

(defun item-ref-bucket (universe item-ref dont-load-items-p)
  (when (getf item-ref :store)
    (let* ((key
	    (format nil "~A~A~A"
		    (getf item-ref :store)
		    (getf item-ref :collection)
		    (getf item-ref :bucket-key)))
	   (bucket (gethash key *load-buckets*)))
      (unless bucket
	(setf bucket
	      (load-item-reference-bucket
	       universe item-ref
	       ;;Don't load if loading file but if it is value references load
	       ;;those collections so that reference lookup works correct.
	       :dont-load-items-p dont-load-items-p))
	(setf (gethash key *load-buckets*)
	      bucket))
      bucket)))


(defun find-key (value key)
  (find key value :test #'equalp))

(defun line-item-p (line)
  (and (listp line)
       (atom (first line))
       (symbolp (first line))
       (> (length line) 1)
       (find-key line :values)        
       (not (dig line :values :reference%))))

(defun line-ref-item-p (line)
  (and (listp line)
       (atom (first line))
       (symbolp (first line))
       (> (length line) 1)
       (find-key line :values)   
       (dig line :values :reference%)))


(defun blob-ref-p (object)
  (if (listp object)
      (equalp (first object) :blob%)))

(defun blob-ref-values (blob-ref)
  (let ((values (second blob-ref)))
    (if (listp values)
	values
	(list :location values :file-type :text :file-ext "blob"))))

(defun read-blob (blob-ref-values)
  (let ((*read-eval* nil)
	(out (make-string-output-stream))
	(str ""))

    (with-open-file (in (getf blob-ref-values :location)
			:direction :input		     
			:if-does-not-exist nil)
      (when in
	(loop for line = (read-line in nil)
	   while line do (write-line line out) )
	(close in))
      (setf str (get-output-stream-string out))
      (close out))
    
    (make-blob
     :file-type (getf blob-ref-values :file-type)
     :file-ext (getf blob-ref-values :file-ext)
     :location (getf blob-ref-values :location)
     :raw str)))

(defun parse-item-line (universe tree &key dont-load-items-p)
  (cond ((null tree)
	 nil)
	((line-item-p tree)
	 (let* ((final-item)
		(bucket (item-ref-bucket universe tree dont-load-items-p))
		(ref-values (dig tree :values))
		(resolved-values (and ref-values
				      (parse-item-line universe ref-values :dont-load-items-p dont-load-items-p)))
		(ref-item (and bucket (or (gethash (dig tree :hash) 
						   (index (collection bucket)))
					 
					  (gethash (dig tree :hash) 
						   (key-index (collection bucket)))))))
	   (cond (ref-item
		  (unless (getf tree :deleted-p)
		    (unless (equalp (item-values ref-item) resolved-values)
			(push  (item-values ref-item) (item-versions ref-item))
			(setf (item-values ref-item) resolved-values))
		    (setf final-item ref-item))
		    
		  (when (getf tree :deleted-p)
		    (remove-item ref-item)
		    (setf final-item nil)))
		 
		 ((not ref-item)
		  (unless (getf tree :deleted-p)
		    (when bucket
			(setf final-item
			      (make-item
			       :store (store (collection bucket))
			       :collection (collection bucket)
			       :data-type (dig tree :data-type)
			       :bucket bucket
			       :bucket-key (dig tree :bucket-key)
			       :hash (dig tree :hash)
			       :values resolved-values))

			(push final-item (items bucket))
			(add-index final-item)

			(unless (equalp (dig tree :hash) (item-hash final-item))
			  (write-to-file (format nil "~Ashash.err" (location universe))
					 (list
					  (location universe)
					  (location bucket)
					  (format nil " ~A"  (item-hash final-item))
					  (format nil " ~A" (dig tree :hash))))
			  
			  (setf (gethash (dig tree :hash)
					 (index (item-collection final-item)))
				final-item)))
		    
		    (unless bucket
			(setf final-item
			      (make-item
			    
			       :data-type (dig tree :data-type)
			       :bucket bucket
			       :bucket-key (dig tree :bucket-key)
			       :hash (dig tree :hash)
			       :values resolved-values))))
		  
		  (unless final-item
		    (unless (getf tree :deleted-p)
		      (write-to-file "~/data-universe/error.log"
				     (list "Could not resolve ~S" tree))
		      nil))))	   
	   final-item))
	((line-ref-item-p tree)
	 ;;Force load of bucket items because these are reference items
	 ;;the bucket will only be loaded once so dont worry.
	 (let* ((bucket (item-ref-bucket universe tree nil))
	       (ref-item (and bucket (or (gethash (dig tree :hash) 
						  (index (collection bucket)))
					  
					 (gethash (dig tree :hash) 
						  (key-index (collection bucket)))))))
	   (unless ref-item
	     (write-to-file  (format nil "~Aerror.err" (location universe))
				       (list "Could not resolve reference  ~S" tree)))

	   ref-item))
	((blob-ref-p tree)
	 (read-blob (blob-ref-values tree))
	 )
	((atom tree)
	 tree)
        ((consp tree)
	 (mapcar (lambda (child)
		     (parse-item-line universe child :dont-load-items-p dont-load-items-p))
		 tree))
        (t tree)))

(defun load-items (universe filename )
  (let ((*load-buckets* (make-hash-table :test 'equalp)))
    (declare (special *load-buckets*))
    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (loop for line = (read in nil)
	     while line
	     do (parse-item-line
		 universe line
		 :dont-load-items-p t))
	  (close in))))))

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
	(unless (and (loaded-p bucket)
		     (equalp (file-write-date location)
			     (loaded-p bucket)))
	  (load-items (universe (store collection)) location)
	  (setf (loaded-p bucket) (file-write-date location)))))
    bucket))

(defgeneric persist-item (collection item &key &allow-other-keys))

(defmethod persist-item ((collection collection) item &key allow-key-change-p)

  (if (item-p item) 
      (persist item :collection collection
		   :allow-key-change-p allow-key-change-p)
      (persist (make-item 
		:store (store collection)
		:collection collection
		:data-type (data-type collection)
		:values item)
	       :allow-key-change-p allow-key-change-p)))

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
  (plist-to-value-pairs (if (item-p item)
			   (item-values item)
			   item)))

(defun set-hash (store item)
  (declare (ignore store))
  (unless (item-hash item)
    (let* ((hash (uuid:make-v4-uuid)))
      (setf (item-hash item) hash)
      hash)))

(defun item-to-reference (store item location)
  (if (item-p item)
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
	     :values (parse-to-references% store
					   item
					   (or (item-changes item)
					       (item-values item))
					   location)))
      item))

(defun write-blob (file value)
  (let ((*read-eval* nil)
	(in (make-string-input-stream value)))
    
    (ensure-directories-exist file)
    
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (when in
	(loop for line = (read-line in nil)		 
	   while line do (write-line line out) )
	(close in))
    
      (close out))))

(defun parse-to-references% (store item values location)
  (let (
	(final)
	(value-pairs (parse-item values)))
    
    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))

	(if (item-p val)
	    (setf final (append final
				(list key (item-to-reference
					   store
					   val
					   location))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (item-p (first val))))
		(let ((children))
		  (dolist (it val)
		    (if (item-p it)
			(setf children
			      (append children 
				      (list (item-to-reference
					     store
					     it
					     location))))
			(setf children (append children (list it)))))
		  (setf final (append final (list key children))))
		(cond ((blob-p val)
		      
		       (let ((file (or (and (not (empty-p (blob-location val)))
					    (blob-location val))
				       (frmt "~A~A/~A.~A"
					     location
					     key
					     (item-hash item)
					     (blob-file-ext val)))))
			   ;;string-blob was converted to stream for persistence
			   ;;have to reset it to string-blob
			 (write-blob file (blob-raw val))
			 (setf final (append final (list key
							 (list :blob%
							       (list :file-type (blob-file-type val)
								     :file-type (blob-file-ext val)
								     :location file)))))))
			
			(t
			 (setf final (append final (list key val)))))))))
    final))


(defun copy% (item)
  (let ((copy (copy-item item)))
    (setf (item-values copy) (copy-list (item-values item)))
    (setf (item-changes copy) (copy-list (item-changes item)))
    copy))

(defun parse-to-references (store item location)
  (let ((ref-item (copy% item)))
    (setf (item-values ref-item)
	  (parse-to-references% store item (item-values ref-item) location))
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

(defun new-duplicate-item (items)
  (let ((new-duplicate-item ))
	    
    (dolist (item items)
      (when (and (item-p item) (not (item-hash item)))
	(setf new-duplicate-item item)))
    new-duplicate-item))

(defun remove-duplicate-items (store values)
  (let ((matching-hashes)

	(keyhash (make-hash-table :test 'equalp)))
    
    (dolist (item values)
      (when (item-p item)
	(let ((hash (key-sxhash store
				(item-data-type item)
				(or (item-changes item) (item-values item)))))
	  
	  (if (gethash hash keyhash)
	      (setf matching-hashes (push hash matching-hashes)))
	  (setf (gethash hash keyhash) (push item (gethash hash keyhash))))))

    (dolist (match matching-hashes)
      (let* ((items (gethash match keyhash))
	     (new-duplicate-item (new-duplicate-item items)))
	(when new-duplicate-item
	  (dolist (item items)
	    (when  (item-p item)
	      (unless (eq item new-duplicate-item)
		(setf (item-changes item) (item-changes new-duplicate-item))))))
	  (setf values (remove new-duplicate-item values))))
    values))

(defun check-item-values% (store values)
  (let ((final)
	(value-pairs (parse-item values)))

    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))
	
	(if (item-p val) 
	    (setf final (append final
				(list key
				      (if (item-collection val)
					  (persist val)
					  (check-no-collection-val store val)))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (item-p (first val))))
		(let ((children))
		  
		  (dolist (it (remove-duplicate-items store val))
		    
		    (if (item-p it)
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
	(lookup-old (or (lookup-index-hash (item-collection item)
					   (item-hash item))
			(lookup-index (item-collection item)
				      (item-values item))))
	(lookup-new (lookup-index (item-collection item)
				  (item-changes item)))
	(final-item))

    (when change-p
      (when lookup-old
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
	      (break "shit")
	      (add-index lookup-old)
	      (setf final-item lookup-old))))
	
	(unless lookup-new
	  (unless (equalp (index-keys  (item-collection lookup-old)
				       (item-values lookup-old))
			  (index-keys (item-collection lookup-old)
				      (item-changes lookup-old)))

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
	      (setf final-item lookup-old)))

	  (when (equalp (index-keys  (item-collection lookup-old)
				     (item-values lookup-old))
			(index-keys (item-collection lookup-old)
				    (item-changes lookup-old)))
	    (setf (item-values lookup-old)
		  (check-item-values% (item-store item) (item-changes item)))
	    (setf (item-changes item) nil)
	    (setf final-item lookup-old))))
      
      (unless lookup-old

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
	  (setf final-item item))))

    (unless change-p
      
      (when lookup-old

	(let ((wtf (check-item-values% (item-store item)
				       (or (item-changes item)
					   (item-values item)))))

	  (if (equalp (item-values lookup-old)
		      (or (item-changes item)
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

(defun parse-persist-item (file item)
  ;;Parse item to persistable format
  (let ((item-to-persist (parse-to-references (item-store item) item (directory-namestring file))))
    (when  item-to-persist
      (setf (item-persisted-p item) nil)	
      (write-to-file file
		     item-to-persist
		     :if-exists :append)
      (setf (item-persisted-p item) t))
    item))

(defmethod persist ((item item) &key collection file allow-key-change-p
				  new-file-p
				  &allow-other-keys)
  (let ((*persist-p* nil)
	(derived-file))

    (unless file
      ;;Resolve the location of the item
      (setf item (check-location item :collection collection))
      (setf derived-file (location (item-bucket item))))
    
    (let ((changed-item (if new-file-p
			    item
			    (check-item-values item allow-key-change-p))))

      (cond (changed-item
	     (setf item changed-item)
	     (parse-persist-item (or file derived-file)
				 item))
	    ((item-deleted-p item)
	     (parse-persist-item (or file derived-file)
				 item))
	    (t
	     item)))))

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
		     :data-type (getf collection-def :data-type)
		     :filter (getf collection-def :filter)))))

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
	    (unless (items bucket)
	      (load-collection-items collection))
	    
	    
	    (setf items
		  (append
		   items
		   (if test
		       (map return-type
			    (lambda (item)
			     
			      (when (filter collection))
			      (let ((filter-p (if (filter collection)
						  (eval
						   `(apply ,(filter collection)
							   (list ,item)))
						  t)))

				(when filter-p
				  (if find-first-item-p
				      (when (apply test item test-args)
					(return-from fetch-items* item))
				      (and (apply test item test-args) item)))))
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
      (when collection	
	(add-collection store collection))
      (unless collection
	  (error "Could not create collection ~A" collection-name)))

    
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

(defgeneric exists-p (item field-name))

(defmethod exists-p ((item item) field-name)
  (get-properties (item-values item) (list field-name)))

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
		  (if (item-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (item-p val)
			 (item-values val)
			 val)))
    
    (if indicators
	(naive-dig next-place indicators)
	(if (item-p place)
	    (getx place indicator)
	    (getf place indicator)))))

(defun set-naive-dig (place indicators value)
  
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (item-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (item-p val)
			 (if (item-changes val)
			     (item-changes val)
			     (setf (item-changes val)
				   (copy-list (item-values val))))
			 val)))
    (if indicators
	(if (item-p val)
	    (set-naive-dig next-place indicators value)
	    (setf (getf place indicator) 
		  (set-naive-dig next-place indicators value)))
	(if (item-p place)
	    (setf (getx place indicator) value)
	    (setf (getf place indicator) value)))
    place))

(defun digx (place &rest indicators)
  (naive-dig place indicators))

(defun (setf digx) (value place &rest indicators)
  (set-naive-dig place indicators value))

;;((:name arst :value arts) (:name ...))
(defun find-item-by-value (item-list field-values)
  (let ((exists nil))
    (dolist (item item-list)
      (dolist (field field-values)
	(if (equalp (getx item (getf field :name)) (getf field :value)) 
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(return-from find-item-by-value item)))
    exists))

(defun match-item (item item-list match-fields)
  (let ((exists nil))
    (dolist (list-item item-list)
      (dolist (field match-fields)
	(if (equalp (getx item field) (getx list-item field))	  
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(return-from match-item list-item)))))

(defun match-replace-item (item item-list match-fields)
  (let ((exists nil))
    (dolist (list-item item-list)
      (dolist (field match-fields)
	(if (equalp (getx item field) (getx list-item field))	  
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(remove list-item item-list)
	(push item item-list)))
   item-list))

(defun find-equalp-item (item item-list)
  (let ((exists nil))
    (dolist (list-item item-list)
      (when (equalp (item-values list-item) (item-values item))	
	(setf exists item-list)))
    exists))

(defun find-in-item-list (item-list test)
  (map nil
       (lambda (item)
	 (when (apply test (list item))
	       (return-from find-in-item-list item)))
       item-list))

(defun find-items-in-item-list (item-list test)
  (remove-if #'not  (map 'list
			 (lambda (item)
			   (apply test (list item)))
			 item-list)))
