(in-package :cl-naive-store)

(defclass collection ()
  ((store :initarg :store
	  :accessor store
	  :initform nil
	  :documentation "The store that this collection belongs to.")
   (name :initarg :name
	 :accessor name
	 :documentation "The collection name string.")   
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The directory path to where files for this collection is stored.")
   (documents :initarg :documents
	  :accessor documents
	  :initform nil
	  :documentation "The documents contained by this collection. By default naive-store uses a list.

NOTES:

 Testing on my system (sbcl- --dynamic-space-size 12000) doing string comparisons etc while querying data (ie high touch search) I only saw .7 seconds diff between an array of 10 mil (3.154) and a list of ten mil (3.851) plist data documents. The bulk of time is used to access a element in a data document and doing comparisons, so if you are desperate for speed look there. If you are using naive-store for more than 10 mil documents in a collection please let me know! ")

   (keys :initarg :keys
	 :accessor keys
	 :initform (list :key)
	 :documentation "Keys need to be set to handle duplicates, the default is :key if :key is not found in the document then duplicates will accur.

NOTES:

For collections that use cl-naive-document-type there is a fallback the document-type is checked for keys as well and the collection's keys will be set to the keys set in the document-type elements."))
  
  (:documentation "A collection of documents of a specific document-type."))

(defclass store ()
  ((universe :initarg :universe
	     :accessor universe
	     :initform nil
	     :documentation "The universe this store belongs to.")
   (name :initarg :name
	 :accessor name
	 :documentation "Store name string.")
   (collection-class :initarg :collection-class
		:accessor collection-class
		:initform 'collection
		:allocation :class
		:documentation "Then class that should be used to make collection documents.")
   (collections :initarg :collections
		:accessor collections
		:initform nil
		:documentation "List of collections represented by this store.")
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The directory path to the document-type files and collection files for this store.")
   )
  (:documentation "Data types and their associated collections are organized into groups called stores. 

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
		:documentation "The class that should be used to make store documents. 

NOTES:

store-class is delcaritively specied here because stores are dynamicly created when definition
files are loaded. (see store notes for more about this.).")
   (location :initarg :location
	     :accessor location
	     :initform (cl-fad:merge-pathnames-as-directory
			(user-homedir-pathname)
			(make-pathname :directory (list :relative "data-universe")))
	     :documentation "Directory path to stores."))
  (:documentation "Stores are held by a universe to make up a database." ))

(defgeneric get-store (universe store-name)
  (:documentation "Returns a store document if found in the universe."))

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

;;TODO: handle hashtable with sepialization
(defun persist-collection (collection)
  "Persists the documents in a collection in the order that they where added."

  (if (hash-table-p (documents collection))
      (maphash (lambda (key doc)
		 (declare (ignore key))
		 (persist-document collection doc))
	       (documents collection))
      (dolist (doc (documents collection))
	(persist-document collection doc))))

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


(defgeneric collection-container-loaded-p (container &key &allow-other-keys)
  (:documentation "Used by data-loaded-p

IMPL NOTES: 

If you change the collections underlying document-type in (documents collection) you have to implement this. Your implementation is expected to physically check the document count and not some status set. Be smart about it you are not expected to return a count so dont waist time counting just check if there is at least one document in the container."))

(defmethod collection-container-loaded-p (container &key &allow-other-keys)
  (when container
    (car container)))

(defmethod collection-container-loaded-p ((container hash-table) &key &allow-other-keys)
  (when container    
    (>= (hash-table-count container) 1)))

(defparameter *busy-loading* nil
  "Used to make sure loading does not go into a endless recursive loop.")

(defgeneric load-data (collection &key force-reload-p &allow-other-keys)
  (:documentation "Loads the data documents of a collection from file. If the data is already loaded it wont reload it, if you want the data to be reloaded use force-reload-p.

NOTES:

load-data could have been used to load universe or store as well but those have a wealth of other key word parameters to control exactly what to do that makes the load-data sigature a nightmare to understand. So seperate methods load-stores (for universe) and load-collections (for a store) exists for that."))

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
  (if (not (empty-p (location object)))
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

(defmethod load-data :around ((collection collection) &key force-reload-p &allow-other-keys)
  "Explicitly stops execution of main methods if already loaded, unless forced."

  (if force-reload-p
      (call-next-method)
      (when (not (collection-container-loaded-p (documents collection)))
	
	(let ((*busy-loading* *busy-loading*))	  
	  (unless (string-equal *busy-loading* (name collection))
	    
	    (setf *busy-loading* (name collection))
	    (call-next-method))
	  (setf *busy-loading* nil)))))


(defgeneric data-loaded-p (container &key *allow-other-keys)
  (:documentation "Checks if the data is loaded for the container, be it universe , store or collection.

NOTES: 

This physically checks each collection's underlying concrete data structure for data. This is done because a collection can be empty and still loaded, thus setting a status when loaded became confusing and could be missed by an over loadeding method."))

(defmethod data-loaded-p ((collection collection) &key &allow-other-keys)
  (when collection
    (collection-container-loaded-p (documents collection))))

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
