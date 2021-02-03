(in-package :cl-naive-store)

(defclass shard ()
  ((mac :initarg :mac
	:accessor mac
	:initform nil
	:documentation "Mac to identify shard.")
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The file path to this shard is stored.")
   (documents :initarg :documents
	      :accessor documents
	      :initform (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)
	      :documentation "Documents belonging to shard.")   
   (status :initarg :status
	     :accessor status
	     :initform nil
	   :documentation "TODO")
   (lock :initarg :lock
	 :accessor lock
	 :initform (list :docs (bt:make-lock) :hash-index (bt:make-lock) :values-index (bt:make-lock))))
  
  (:documentation "Sharding is when you break the physical file that backs the collection into smaller files based on data elements of a document. An instance of a shard class is used to load the documents belonging to the shard into memory."))

(defclass collection ()
  ((store :initarg :store
	  :accessor store
	  :initform nil
	  :documentation "The store that this collection belongs to.")
   (name :initarg :name
	 :accessor name
	 :documentation "The collection name.")   
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The directory path to where files for this collection are stored.")
   (shards :initarg :shards
	  :accessor shards
	  :initform (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)
	  :type cl:vector
	  :documentation "A vector of shards. 

NOTES:

 Originally naive-store used lists but with the re-introduction of sharding, we chose to also introduce the use of lparrallel to speed up many functions and lparrallel has a preference for arrays.")

   (keys :initarg :keys
	 :accessor keys
	 :initform (list :key)
	 :documentation "Keys need to be set to handle duplicates, the default is :key if :key is not found in the document then duplicates will occur.

NOTES:

For collections that use cl-naive-document-type there is a fallback the document-type is checked for keys as well and the collection's keys will be set to the keys set in the document-type elements.")
   (shard-elements :initarg :shard-elements
	 :accessor shard-elements
	 :initform nil
	 :documentation "shard-elements is a list of document element keywords to use for sharding."))
  
  (:documentation "A collection of documents of a specific document-type."))

(defclass store ()
  ((universe :initarg :universe
	     :accessor universe
	     :initform nil
	     :documentation "The universe this store belongs to.")
   (name :initarg :name
	 :accessor name
	 :documentation "Store name.")
   (collection-class :initarg :collection-class
		:accessor collection-class
		:initform 'collection
		:allocation :class
		:documentation "The class that should be used to make collections.")
   (collections :initarg :collections
		:accessor collections
		:initform nil
		:documentation "List of collections represented by this store.")
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The directory path to the document-type files and collection files for this store.")
   )
  (:documentation "Document types and their associated collections are organized into groups called stores. 

NOTES:

collection-class and document-type-class is delcaritively specied here because they are dynamicly created when definition files are loaded. The alternative would be defmethod hell where the customizer of naive-store would have to implement a whole lot of methods that do exactly what the provided methods do just to be able to be type specific in other methods where it is actually needed. Alternatively meta classes could be used for element-class but that opens another can of worms."))
    
(defclass universe ()
  ((stores :initarg :stores
	   :accessor stores
	   :initform nil
	   :documentation "List of stores contained by this universe.")
   (store-class :initarg :store-class
		:accessor store-class
		:initform 'store
		:allocation :class
		:documentation "The class that should be used to make stores. 

NOTES:

store-class is delcaritively specied here because stores are dynamicly created when definition
files are loaded. (see store notes for more about this.).")
   (location :initarg :location
	     :accessor location
	     :initform (cl-fad:merge-pathnames-as-directory
			(user-homedir-pathname)
			(make-pathname :directory (list :relative "data-universe")))
	     :documentation "Directory path to stores.")
   (shards-cache% :initarg :shards-cache%
		  :accessor shards-cache%
		  :initform
		  #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized nil)
		  #+(not (or sbcl ecl)) (make-hash-table :test 'equalp )                  
		  :documentation "This was introduced to speedup finding a shard. It is only for internal use!")
   (shards-macs-cache% :initarg :shards-macs-cache%
		  :accessor shards-macs-cache%
		  :initform
		  #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized nil)
		  #+(not (or sbcl ecl)) (make-hash-table :test 'equalp )
		  :documentation "This was introduced to speedup finding a shard. Calulating macs is expensive. It is only for internal use!")
   )
  (:documentation "Stores are held by a universe to make up a database." ))

;;TODO: add parameter to select vector or list
(defmethod documents ((collection collection))
  (let ((documents))
    (do-sequence (shard (shards collection))
      (setf documents (concatenate 'list documents (documents shard))))
       documents))

(defun match-shard (filename shards)
  (dolist (mac shards)    
    (when (search (typecase mac
		    (shard
		     (mac mac))
		    (t mac)) (format nil "~A" filename) :test 'equal)
      (return-from match-shard (values mac filename)))))

(defgeneric get-shard (collection shard-mac &key &allow-other-keys)
  (:documentation "Get the shard object by its mac and creates a new shard if not found. Shard lookups are done so much that there is no choice but to cache them in a hashtable, but that hashtable needs to be thread safe so using safe functions to get and set."))

(defvar *shards-cache-lock* (bt:make-lock))

(defun get-shard-cache-safe% (collection shard-mac)
  (gethash-safe (frmt "~A-~A-~A"
		      (name (store collection))
		      (name collection)
		      (or shard-mac (name collection)))
		(shards-cache% (universe (store collection)))
			      :lock *shards-cache-lock*))

(defun set-shard-cache-safe% (collection shard-mac shard)
  (setf (gethash-safe (frmt "~A-~A-~A"
			    (name (store collection))
			    (name collection)
			    (or shard-mac (name collection)))
		      (shards-cache% (universe (store collection)))
		      :lock *shards-cache-lock*)
	shard))

(defmethod get-shard :around (collection shard-mac &key &allow-other-keys)
  (let ((shard (get-shard-cache-safe% collection shard-mac)))
    (if (not shard)
	(set-shard-cache-safe% collection shard-mac (call-next-method))
	shard)))

(defmethod get-shard (collection shard-mac &key &allow-other-keys)  
  (let ((shard (lparallel:pfind (or shard-mac (name collection))
				(shards collection)
				:test 'equal :key 'mac)))
    (unless shard
      (setf shard (make-instance 'shard
				 :mac shard-mac
				 :location
				 (cl-fad:merge-pathnames-as-file
				  (pathname (ensure-location collection))
				  (make-pathname
				   ;;:directory (list :relative (name collection))
				   :name shard-mac
				   :type "log")) ))
      (vector-push-extend shard (shards collection)))
    shard))

(defvar *shards-macs-cache-lock* (bt:make-lock))

(defun get-shard-mac-cache-safe% (collection value)
  (gethash-safe value (shards-macs-cache% (universe (store collection)))
			      :lock *shards-macs-cache-lock*))

(defun set-shard-mac-cache-safe% (collection value mac)
  (setf (gethash-safe value (shards-macs-cache% (universe (store collection)))
				    :lock *shards-macs-cache-lock*)
	mac))

(defun document-shard-mac (collection document)
  "Calculating a mac is expensive so caching shard value macs in a hashtable but that hashtable needs to be thread safe so using safe functions to get and set."
  (let ((value))
    ;;cant use lparallel the order of values are important.
    (dolist (element (shard-elements collection))      
      (push (list element (getx document element)) value))

    (if value
	(let ((mac (get-shard-mac-cache-safe% collection value)))
	  (unless mac
	    (setf mac (naive-impl:make-mac (reverse value)))
	    (set-shard-mac-cache-safe% collection value mac))
	  mac)
	(name collection))))

(defgeneric get-store (universe store-name)
  (:documentation "Returns a store if found in the universe."))

(defmethod get-store ((universe universe) store-name)
  (dolist (store (stores universe))
    (when (string-equal store-name (name store))
      (return-from get-store store))))

(defgeneric get-collection (store collection-name)
  (:documentation "Returns a collection document if found in the store."))

(defmethod get-collection ((store store) collection-name)
   (dolist (collection (collections store))
     (when (string-equal collection-name (name collection))
       (return-from get-collection collection))))

(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Writes various store structural objects to "))
  
(defmethod persist ((store store) &key &allow-other-keys)
  "Persists a store definition and not what it contains! Path to file is of this general format
/universe/store-name/store-name.store."
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location store))
    (make-pathname :name (name store)
		   :type "store"))
   (list :name (name store)
	 :location (location store))
  
   :if-exists :supersede))

(defgeneric persist-collection-def (colleciton)
  (:documentation "Persists a collection definition. Path to file is of this general format /universe/store-name/collection-name.col."))

(defmethod persist-collection-def ((collection collection))
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location (store collection)))
    (make-pathname :name (name collection)
		   :type "col"))
   (list 
    :name (name collection)
    :location (location collection))		 		 
   :if-exists :supersede))

;;TODO: handle hashtable with specialization
(defun persist-collection (collection)
  "Persists the documents in a collection in the order that they where added."

  (do-sequence (shard (shards collection) :parallel-p t)
    (naive-impl::with-open-file-lock
       (stream (location shard))

       (if (hash-table-p (documents shard))
	     
	     (maphash (lambda (key doc)
			(declare (ignore key))
			(persist-document collection doc :shard shard :file-stream stream))
		      (documents shard))
	     (do-sequence (doc (documents shard))
	       (persist-document collection doc :shard shard :file-stream stream))))))

(defmethod persist ((collection collection) &key &allow-other-keys)
  "Persists a collection definition and the documents in a collection. Path to file for data is this general format /universe/store-name/collection-name/collection-name.log."
  (persist-collection-def collection)
  (persist-collection collection))

(defgeneric add-store (universe store)
  (:documentation "Adds a store document to a universe."))

(defmethod add-store ((universe universe) (store store))
  (unless (get-store universe (name store))
    (when (location store)
      (ensure-directories-exist (pathname (location store))))
    (unless (location store)
      (let ((location
	     (cl-fad:merge-pathnames-as-directory
	       (pathname (location universe))
	       (make-pathname :directory (list :relative (name store))))))
	(ensure-directories-exist location)
	(setf (location store) (pathname location))))
    
    (setf (universe store) universe)
    (persist store)
    (pushnew store (stores universe)))
  store)

(defun get-store-from-def (universe store-name)
  "Tries to find a store definition file on disk and if it does it loads the store into the universe, but it does not load the collections!."
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location universe))
		   (make-pathname :directory (list :relative store-name)
				  :name store-name
				  :type "store")))
	(store-def)
	(store))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf store-def (read in nil))
	  (close in))))
    
    (when store-def
      (setf store
	    (make-instance (store-class universe)
			   :name (getx store-def :name)		    
			   :location (getx store-def :location))))
    store))


(defgeneric get-collection-from-def (store collection-name)
  (:documentation "Tries to find the collection definition file on disk and loads it into the store, but it does not load the collection's data."))

(defmethod get-collection-from-def ((store store) collection-name)
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location store))
		   (make-pathname :name collection-name
				  :type "col")))
	(collection-def))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax
	(when in
	  (setf collection-def (read in nil))
	  (close in))))

    (when collection-def
      (make-instance (collection-class store)
			 :store store
			 :name (getx collection-def :name)
			 :document-type (getx collection-def :document-type)
			 :location (getx collection-def :location)))))

(defgeneric add-collection (store collection)
  (:documentation "Adds a collection to a store."))

;;TODO: Deal with sharding
(defmethod add-collection ((store store) (collection collection))
  (unless (get-collection store (name collection))    
    (let ((location (location collection)))
      
      (when location
	(ensure-directories-exist (pathname location)))

      (unless location 
	(setf location
	      (cl-fad:merge-pathnames-as-file
	       (pathname (location store))
	       (make-pathname :directory (list :relative (name collection))
			      :name (name collection)
			      :type "log")))
        
	(ensure-directories-exist location))
      
      (setf (location collection) (pathname location))
      (pushnew collection (collections store))
      (setf (store collection) store)
      (persist-collection-def collection)))
  collection)

(defgeneric clear-collection (collection)
  (:documentation "Clears documents indexes etc from collection."))

(defmethod clear-collection (collection)
  (do-sequence (shard (shards collection))    
    (remhash (frmt "~A-~A-~A"
		      (name (store collection))
		      (name collection)
		      (or (mac shard) (name collection)))
	     (shards-cache% (universe (store collection))))
    (setf shard nil))  
  (setf (shards collection) (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)))

(defgeneric remove-collection (store collection)
  (:documentation "Removes a collection to a store."))

(defmethod remove-collection ((store store) (collection collection))
  (clear-collection collection)
  (setf (collections store) (remove collection (collections store))))

(defparameter *busy-loading* nil
  "Used to make sure loading does not go into a endless recursive loop.")

(defgeneric load-data (collection &key force-reload-p shard-macs &allow-other-keys)
  (:documentation "Loads the data documents of a collection from file or files if sharding is used. If the data is already loaded it wont reload it, if you want the data to be reloaded use force-reload-p.

shard-macs is a list of shard macs to indicate which shards should be used. If no shards are specified all shards will be loaded.

NOTES:

load-data could have been used to load universe or store as well but those have a wealth of other key word parameters to control exactly what to do that makes the load-data signature a nightmare to understand. So separate methods load-stores (for universe) and load-collections (for a store) exists for that."))

(defgeneric ensure-location (object)
  (:documentation "Tries to find or build path to cl-naive-store files."))

(defmethod ensure-location ((object universe))
  (if (not (empty-p (location object)))
      (location object)
      (error "Unverse location is not set.")))

(defmethod ensure-location ((object store))
  (if (not (empty-p (location object)))
      (location object)
      (if (not (universe object))
	  (error "Store universe not set, cant ensure location.")
	  (if (not (empty-p (ensure-location (universe object))))
	      (setf (location object)
		    (cl-fad:merge-pathnames-as-file
		     (pathname (location (universe object)))
		     (make-pathname :directory (list :relative (name object))
				    :name (name object)
				    :type "store")))))))

(defmethod ensure-location ((object collection))
  (if (and (not (empty-p (location object)))
	   (equalp (pathname-type (pathname (location object)))
		   "log"))      
      (location object)
      (if (not (store object))
	  (error "Collection store not set, cant ensure location.")
	  (if (not (empty-p (ensure-location (store object))))
	      (setf (location object)
		    (cl-fad:merge-pathnames-as-file
		     (pathname (location (store object)))
		     (make-pathname :directory
				    (list :relative (name object))
				    :name (name object)
				    :type "log")))))))

#|
;;TODO: Deal with shards
(defmethod load-data :around ((collection collection) &key force-reload-p shard-macs &allow-other-keys)
  "Explicitly stops execution of main methods if already loaded, unless forced."

  (if force-reload-p
      (call-next-method)
      (if (not shard-macs)
	  (unless (and collection
		   (shards collection) 
		   (elt (shards collection) 0)
		   (elt (elt (shards collection) 0) 0))
	    
	    (let ((*busy-loading* *busy-loading*))	  
		      (unless (string-equal *busy-loading* (name collection))	      
			(setf *busy-loading* (name collection))
			(call-next-method))
		      (setf *busy-loading* nil)))
	  
	
	  (let ((all-shards-p nil))
	    (do-sequence (mac shard-macs)
	      (let ((shard-found))	  
		(do-sequence (shard (shards collection))
		  (when (equalp (mac shard) mac)
		    (setf shard-found t)))
	  
		(if shard-found
		    (setf all-shards-p shard-found)
		    (let ((*busy-loading* *busy-loading*))	  
		      (unless (string-equal *busy-loading* (name collection))	      
			(setf *busy-loading* (name collection))
			(call-next-method))
		      (setf *busy-loading* nil)))))
	    all-shards-p))))

|#


;;TODO: data-loaded-p is not used internally any more since it is a waisted iteration to check and then load. Load data checks and loads at the same time if necessary. So should be remove data-loaded-p? Tests use it heavily so maybe not.

(defgeneric data-loaded-p (container &key *allow-other-keys)
  (:documentation "Checks if the data is loaded for the container, be it universe , store or collection.

NOTES: 

This physically checks each collection's underlying concrete data structure for data. This is done because a collection can be empty and still loaded, thus setting a status when loaded became confusing and could be missed by an over loading method.

If you change the underlying container for (shards collection) or the container for (docutments shard) you have to implement data-loaded-p. Your implementation is expected to physically check for document count > 0 and not some status set. Be smart about it you are not expected to return a count so dont waist time counting just check if there is at least one document in the container."))

;;TODO: Deal with shards.
(defmethod data-loaded-p ((collection collection) &key shard-macs &allow-other-keys)

  (let ((all-shards-p nil))
    (if (not shard-macs)      
	(if (and collection
		 (shards collection)
		 (> (fill-pointer (shards collection)) 0))
	    (do-sequence (shard-found (shards collection))
	      (if (or		       
		   (equalp (status shard-found) :loaded)
		   (> (length (documents shard-found)) 0))
		  (push shard-found all-shards-p)
		  (push nil all-shards-p))))
        
	(do-sequence (mac shard-macs)
	  (let ((shard-found (lparallel:pfind mac (shards collection) :test 'equal :key 'mac)))	  
	    (if shard-found
		(push shard-found all-shards-p)
		(push nil all-shards-p)))))
;;    (break "? ~A" (every (lambda (x) x) all-shards-p))
    (if all-shards-p
	(every (lambda (x) x) all-shards-p))))

(defmethod data-loaded-p ((store store) &key &allow-other-keys)
  (let ((loaded-p t))
    (dolist (collection (collections store))
      (unless (data-loaded-p collection)
	(return-from data-loaded-p nil)))
    loaded-p))

(defmethod data-loaded-p ((universe universe) &key &allow-other-keys)
  (let ((loaded-p t))
    (dolist (store (stores universe))
      (unless (data-loaded-p store)
	(return-from data-loaded-p nil)))
    loaded-p))
