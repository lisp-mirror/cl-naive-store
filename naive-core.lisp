(in-package :cl-naive-store)

(defclass field ()
  ((name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "Name of the field. This must be a KEYWORD.")
   (type-def :initarg :type-def
	     :accessor type-def
	     :initform nil
	     :documentation "A user defined object that defines the type specifics of an field.

 The default implementation of cl-naive-store does not to use these definitions when reading and 
writing data objects to and from file. This was by design, to place as little burden on reading and writing
data objects. Depending on the use of naive-store a user could customize the reading and writing methods of 
naive-store to use these definitions for validation and file layout specifics. 

GUI's like cl-wfx can also use these to help with generic rendering of user input screens. 

See cl-naive-items:*example-type-defs* for examples of type definitions to get a feel for the intended use.")
   (key-p :initarg :key-p
	  :accessor key-p
	  :initform nil
	  :documentation "Indicates that the field is part of the primary key.

 Used for indexing and object comparison. For example when a new object is persisted naive-store 
checks for objects with the same index value and then updates the existing object in its default mode.")
   (attributes :initarg :attributes
	       :accessor attributes
	       :initform nil
	       :documentation "A property list of additional field attributes that are not data type specific.
 Not used by naive-store."))
  
  (:documentation "A definition of a unit of data of a data oject. 

The type-def and attributes are not used by naive-store in its default implementation directly,
 but is supplied here so that creators of additional modules dont have to use class inheritence 
to add such data to type definitions.

By adding type-def and attributes here even though it is not used it helps to standardize possible use.

Fields can reference simple types, a complex object or objects based on other data-types. 

*Note*: naive-store is by default designed to be a hierarchical database."))

(defclass data-type ()
  ((store :initarg :store
	  :accessor store
	  :initform nil
	  :documentation "The store that this data-type belongs to.")
   (name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "String representing a data-type name.")
   (field-class :initarg :field-class
	       :accessor field-class
	       :initform 'field
	       :allocation :class
	       :documentation "The class that should be used to make data-type objects. 
field-class is delcaritively specied here because fields are dynamicly created when definition
files are loaded. The alternative would be defmethod hell where the user of naive-store would have to 
implement a whole lot of methods that do exactly what the provided methods do just to be able to be type 
specific in other methods where it is actually needed. Alternatively meta classes could be used for field-class
 but that opens another can of worms.")
   (label :initarg :label
	  :accessor label
	  :initform nil
	  :documentation "Human readable/formated short description.")
   (top-level-p :initarg :top-level-p
		:accessor top-level-p
		:initform nil
		:documentation "Not all data types have their own collections, only data types marked as top
level t have their own collections/files. Non top level type objectss are stored in their referencing data type's
collections. ")  
   (fields :initarg :fields
	   :accessor fields
	   :initform nil
	   :documentation "Field definitions that represents a data unit."))
  (:documentation "A grouping of field definitions that represents a complex data unit."))


(defclass collection ()
  ((store :initarg :store
	  :accessor store
	  :initform nil
	  :documentation "The store that this collection belongs to.")
   (name :initarg :name
	 :accessor name
	 :documentation "The collection name string.")   
   (data-type :initarg :data-type
	      :accessor data-type
	      :initform nil
	      :documentation "The data-type that this collection contains objects of.")
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
   (uuid-index :initarg :uuid-index
	  :accessor uuid-index
	  :initform (make-hash-table :test 'equalp)
	  :documentation "Hash table keyed on object hash codes for quick retrieval of an object.")
   (key-value-index :initarg :key-value-index
	  :accessor key-value-index
	  :initform (make-hash-table :test 'equalp)
	  :documentation "Hash table keyed on object key values for quick retrieval of an object. Used 
when doing key value equality comparisons.")
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
   (collection-class :initarg :collections-class
		:accessor collection-class
		:initform 'collection
		:allocation :class
		:documentation "Then class that should be used to make collection objects.")
   (data-type-class :initarg :data-type-class
	       :accessor data-type-class
	       :initform 'data-type
	       :allocation :class
	       :documentation "Then class that should be used to make data-type objects.")
   (data-types :initarg :data-types
	       :accessor data-types
	       :initform nil
	       :documentation "List of data-types represented by this store's collections.")
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

(defgeneric deleted-p (object)
  (:documentation "Indicates if a data object has been marked as deleted. 

naive-store writes data to file sequentially and when deleting data objects it does not 
remove a data object from the underlying file it just marks it as deleted."))

(defmethod deleted-p (object)
  (getf object :deleted-p))

(defgeneric (setf deleted-p) (value object &key &allow-other-keys))

(defmethod (setf deleted-p) (value object &key &allow-other-keys)
  (setf (getf object :deleted-p) value))

(defgeneric delete-data-object (collection object &key &allow-other-keys))

(defmethod delete-data-object ((collection collection) object &key &allow-other-keys)
  (setf (getf object :deleted-p) t)
  (persist-object collection object)
  (remove-data-object collection object))

(defgeneric hash (object)
  (:documentation "Returns the hash identifier for a data object. Data objects need a hash identifier
to work with naive-store, naive-store will edit the object to add a hash identifier when adding 
objects to a collection. naive-store uses a UUID in its default implementation."))

(defmethod hash (object)
  (frmt "~A" (getf object :hash)))

(defgeneric (setf hash) (value object))

(defmethod (setf hash) (value object)
  (setf (getf object :hash) (frmt "~A" value))
  object)

(defgeneric key-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of key values from the values of a data object.
Looks for :key or uses first value."))

(defmethod key-values (collection values &key &allow-other-keys)
  (or (getf values :key)
      (and (equalp (first values) :hash)
	   (fourth values))
      (and (equalp (first values) :deleted-p)
	   (nth 5 values))
      (second values)))

(defgeneric key-values-hash (collection values  &key &allow-other-keys)
  (:documentation "Returns a hash based on the set of key values from the values of a data object."))

(defmethod key-values-hash (collection values  &key &allow-other-keys)
  (let ((key-values (key-values collection values)))
    (sxhash key-values)))

(defgeneric index-lookup-values-hash (collection values &key &allow-other-keys)
  (:documentation "Looks up object in key value hash index."))

(defmethod index-lookup-values-hash ((collection collection) values &key &allow-other-keys) 
  (let* ((hashx (key-values-hash collection values)))  
    (gethash hashx
	     (key-value-index collection))))

(defgeneric index-lookup-uuid (collection hash)
  (:documentation "Looks up object in UUID hash index."))

(defmethod index-lookup-uuid (collection hash)
  (gethash (frmt "~A" hash)
	   (uuid-index collection)))

(defgeneric add-index (collection object &key &allow-other-keys)
  (:documentation "Adds an object to two indexes. The first uses a UUID that will stay with the object for
 its life time. The UUID is used when persisting the object and is never changed once created. This allows us to 
change key values without loosing the identify of the original object. The second is a key value hash index to
 be used when looking for duplicate objects during persist. The objects hash is also set to UUID."))

(defmethod add-index (collection object &key &allow-other-keys)
  (let* ((hash (uuid:make-v4-uuid))	
	 (hashx (key-values-hash collection object)))
    
    (when (or (empty-p (hash object))
	      (string-equal (format nil "~A" (hash object))
			    (format nil "~A" hashx)))
      
      (setf object (setf (hash object) hash)))    
    
    ;;add the uuid to the object for persistance
    (setf (gethash hashx (key-value-index collection)) object)    
    (setf (gethash (hash object) (uuid-index collection)) object)))


(defgeneric remove-index (collection object &key &allow-other-keys)
  (:documentation "Removes a data object from the UUID and key value indexes."))

(defmethod remove-index (collection object &key &allow-other-keys)
  (remhash (key-values-hash collection object)
	   (key-value-index collection))
  (remhash (hash object) (uuid-index collection)))


(defgeneric add-data-object (collection object &key &allow-other-keys)
  (:documentation "Adds data object to a collection. This method in combination with remove-data-object,
 and data-objects slot of the collection can be used to customize the container used for data objects. "))

(defmethod add-data-object ((collection collection) object &key &allow-other-keys)
  (let ((indexed-object))
    (remove-data-object collection object)
    (setf indexed-object (add-index collection object))
    (push indexed-object
	  (data-objects collection))
    indexed-object))

(defgeneric remove-data-object (collection object &key &allow-other-keys)
  (:documentation "Removes an object from the collection and its indexes. See add-data-object."))

(defmethod remove-data-object ((collection collection) object &key &allow-other-keys)
  (remove-index collection object)
  (setf (data-objects collection)
	(remove object (data-objects collection)
		:test #'(lambda (object item)			 
			  (or
			   (eql object item)
			   (and (not (empty-p (hash item)))
				(not (empty-p (hash object)))
				(equalp (hash item) (hash object)))
			   (equalp (key-values-hash collection item)
				   (key-values-hash collection object)))))))

(defgeneric get-store (universe store-name)
  (:documentation "Returns a store object if found in the universe."))

(defmethod get-store ((universe universe) store-name)  
  (dolist (store (stores universe))
    (when (string-equal store-name (name store))
      (return-from get-store store))))

(defgeneric get-data-type (store type-name)
  (:documentation "Returns a data-type object if found in the store."))

(defmethod get-data-type ((store store) type-name)
  (dolist (data-type (data-types store))
    (when (string-equal type-name (name data-type))
      (return-from get-data-type data-type))))

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

(defmethod persist ((data-type data-type) &key &allow-other-keys)
  "Persists a data-type definition and not what it contains! Path to file is of this general format
/universe/store-name/data-type-name.type."
  (let ((fields))    
    (dolist (field (fields data-type))
      (setf fields (append fields (list (list
					 :name (name field)
					 :key-p (key-p field)
					 :type-def (type-def field)
					 :attributes (attributes field))))))

    (write-to-file
     (cl-fad:merge-pathnames-as-file
	       (pathname (location (store data-type)))
	       (make-pathname :name (name data-type)
			      :type "type"))
     (list 
      :name (name data-type)
      :label (label data-type)
      :top-level-p t
      :fields fields)
     :if-exists :supersede)))

(defun persist-collection-def (collection)
  "Persists a collection definition. Path to file is of this general format
/universe/store-name/collection-name.col."
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location (store collection)))
	       (make-pathname :name (name collection)
			      :type "col"))
    (list 
    :name (name collection)
    :location (location collection)
    :data-type (and (data-type collection) (name (data-type collection))))
		 
		 
   :if-exists :supersede))

(defun persist-collection (collection)
  "Persists the objects in a collection."
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

(defmethod persist-object ((collection collection) object &key &allow-other-keys)
  "Writes an data object to file. Adds it to the collection before hand to ensure hash etc."
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location collection))
	       (make-pathname :name (name collection)
			      :type "log"))
   (add-data-object collection object)))

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

(defun load-store-data-types (store)
  "Finds and loads the files representing data types for a store."
  (let ((files (directory
		(cl-fad:merge-pathnames-as-file (pathname (location store))
						(make-pathname :directory '(:relative :wild-inferiors)
							       :name :wild
							       :type "type"))))
	(type-contents))
    (dolist (file files)
      (with-open-file (in file :if-does-not-exist :create)
	(with-standard-io-syntax              
	  (when in
	    (setf type-contents (read in nil))
	    (close in))))
      
      (let ((fields)
	    (data-type (add-data-type 
			store 
			(make-instance (data-type-class store)
				       :name (getf type-contents :name)
				       :label (getf type-contents :label)
				       :top-level-p (getf type-contents :top-level-p)
				       :fields nil))))
	
	(dolist (field (getf type-contents :fields))
	 (setf fields 
		(append fields 
			(list (make-instance
			       (field-class data-type)
			       :name (getf field :name)
			       :key-p (getf field :key-p)
			       :type-def (getf field :type-def)
			       :attributes (getf field :attributes)))))
	  
	  setf fields )
	(setf (fields data-type) fields)))))

(defun get-store* (universe name)
  "Used internally to find or create a new store."
  (let ((store (get-store universe name)))
    (unless store
      (setf store (get-store-from-def universe name))
      (load-store-data-types store)
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
	  (error "Could not create collection ~A" name)))
    collection))

(defgeneric add-data-type (store data-type)
  (:documentation "Adds a data-type to a store."))

(defmethod add-data-type ((store store) (data-type data-type))
  (unless (get-data-type store (name data-type))
    (setf (store data-type) store)
    (pushnew data-type (data-types store))
    (persist data-type))
  data-type)
