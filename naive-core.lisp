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
   (data-objects :initarg :data-objects
	  :accessor data-objects
	  :initform nil
	  :documentation "The objects contained by this collection. By default naive-store uses a list. 

*Note* Testing on my system (sbcl- --dynamic-space-size 12000) doing string comparisons etc while 
querying data (ie high touch) I only saw .7 seconds diff between an array of 10 mil (3.154) and a 
list of ten mil (3.851) plist data objects. The bulk of time is used to access a field in a data 
object and doing comparisons, so if you are desperate for speed look there. If you are using 
naive-store for more than 10 mil objects in a collection please let me know! ")  
   (loaded-p :initarg :loaded-p
	  :accessor loaded-p
	  :initform nil
	  :documentation "Indicates if the collection has been loaded from file yet."))
  
  (:documentation "A collection of objects of a specific data-type."))

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
		:documentation "Then class that should be used to make collection objects.")
   (collections :initarg :collections
		:accessor collections
		:initform nil
		:documentation "List of collections represented by this store.")
   (location :initarg :location
	     :accessor location
	     :initform nil
	     :documentation "The directory path to the data-type files and collection files for this store."))
  (:documentation "Data types and their associated collections are organized into groups called stores. 

Collection-class and data-type-class is delcaritively specied here because they are dynamicly created 
when definition files are loaded. The alternative would be defmethod hell where the customizer of 
naive-store would have to implement a whole lot of methods that do exactly what the provided methods 
do just to be able to be type specific in other methods where it is actually needed. Alternatively meta 
classes could be used for field-class but that opens another can of worms. "))


(defclass universe ()
  ((stores :initarg :stores
	   :accessor stores
	   :initform nil
	   :documentation "List of stores contained by this universe.")
   (store-class :initarg :store-class
		:accessor store-class
		:initform 'store
		:allocation :class
		:documentation "The class that should be used to make store objects. 

store-class is delcaritively specied here because stores are dynamicly created when definition
files are loaded. The alternative would be defmethod hell where the user of naive-store would have to 
implement a whole lot of methods that do exactly what the provided methods do just to be able to be type 
specific in other methods where it is actually needed. Alternatively meta classes could be used for field-class
 but that opens another can of worms.")
   (location :initarg :location
	     :accessor location
	     :initform (cl-fad:merge-pathnames-as-directory
			(user-homedir-pathname)
			(make-pathname :directory (list :relative "data-universe")))
	 
	     :documentation "Directory path to stores."))
  (:documentation "Stores are held by a universe to make up a database." ))


(defgeneric get-store (universe store-name)
  (:documentation "Returns a store object if found in the universe."))

(defmethod get-store ((universe universe) store-name)  
  (dolist (store (stores universe))
    (when (string-equal store-name (name store))
      (return-from get-store store))))

(defgeneric get-collection (store collection-name)
  (:documentation "Returns a collection object if found in the store."))

(defmethod get-collection ((store store) collection-name)
   (dolist (collection (collections store))
     (when (string-equal collection-name (name collection))
       (return-from get-collection collection))))

(defmethod persist ((store store) &key &allow-other-keys)
  "Persists a store definition and not what it contains! Path to file is of this general format
/universe/store-name/store-name.store."
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location store))
	       (make-pathname :name (name store)
			      :type "store"))
    (list :name (name store)
	 :location (location store))
  
   :if-exists :supersede))

(defgeneric persist-collection-def (colleciton)
  (:documentation "Persists a collection definition. Path to file is of this general format
/universe/store-name/collection-name.col."))

(defmethod persist-collection-def ((collection collection))
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location (store collection)))
	       (make-pathname :name (name collection)
			      :type "col"))
    (list 
    :name (name collection)
    :location (location collection))
		 		 
   :if-exists :supersede))

(defun persist-collection (collection)
  "Persists the objects in a collection in the order that they where added."
  (persist (data-objects collection)
	   :file
	   (cl-fad:merge-pathnames-as-file
	       (pathname (location collection))
	       (make-pathname :name (name collection)
			      :type "log"))))

(defmethod persist ((collection collection) &key def-only-p &allow-other-keys)
  "Persists a collection definition and the items in a collection. Path to file for data is this general format
/universe/store-name/collection-name/collection-name.log."
  (persist-collection-def collection)
  (unless def-only-p
    (persist-collection collection)))

(defgeneric add-store (universe store)
  (:documentation "Adds a store object to a universe."))

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


(defun get-store* (universe name)
  "Used internally to find or create a new store."
  (let ((store (get-store universe name)))
    (unless store
      (setf store (get-store-from-def universe name))
      ;;TODO: is this still needed or does it get lazy loaded some where
      ;;(load-store-data-types store)
      (add-store universe store))
    store))

(defgeneric add-collection (store collection)
  (:documentation "Adds a collection to a store."))

(defmethod add-collection ((store store) (collection collection))
  (unless (get-collection store (name collection))    
    (let ((location (location collection)))
      
      (when location
	(ensure-directories-exist (pathname location)))

      (unless location 
	(setf location
	      (cl-fad:merge-pathnames-as-directory
	       (pathname (location store))
	       (make-pathname :directory (list :relative (name collection)))))
	(ensure-directories-exist location))
      
      (setf (location collection) (pathname location))
      (pushnew collection (collections store))
      (setf (store collection) store)
      (persist collection :def-only-p t)))
  collection)

(defun get-collection* (store name)
  "Used internally to find or create a new collection."
  
  (let ((collection (get-collection store name)))
    (unless collection
      (setf collection (get-collection-from-def store name))
       (when collection
	(add-collection store collection))
      
      (unless collection
	  (error "Could not create collection ~A in store ~A" name (and store (name store)) )))
    collection))

(defgeneric load-data (collection &key &allow-other-keys)
  (:documentation "Loads the data objects of a collection from file."))
