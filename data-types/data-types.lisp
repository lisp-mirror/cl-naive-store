(in-package :cl-naive-data-types)

(defclass field ()
  ((name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "Name of the field. This must be a KEYWORD.")
   (type-def :initarg :type-def
	     :accessor type-def
	     :initform nil
	     :documentation "A user defined object that defines the type specifics of an field.")
   (key-p :initarg :key-p
	  :accessor key-p
	  :initform nil
	  :documentation "Indicates that the field is part of the primary key.

 Can be used for indexing and object comparison. For example when a new object is persisted naive-store-items 
checks for objects with the same index value and then updates the existing object.")
   (attributes :initarg :attributes
	       :accessor attributes
	       :initform nil
	       :documentation "A property list of additional field attributes that are not data type specific."))
  
  (:documentation "A definition of a unit of data of a data oject. 

Fields can reference simple types, a complex object or objects based on other data-types. 

*Note*: naive-store can be used as a hierarchical database or a flat databes or a mix."))

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
	       :documentation "The class that should be used to make field objects. 
field-class is delcaritively specied here because so that fields can be dynamicly created when definition
type definitions are read from file. See naive-store-items for usage examples. ")
   (label :initarg :label
	  :accessor label
	  :initform nil
	  :documentation "Human readable/formated short description.")
   (top-level-p :initarg :top-level-p
		:accessor top-level-p
		:initform nil
		:documentation "When using hierarchical data-types this can be used for various purposes.

For example:

naive-store-items uses this to decide which files to write data to and how to load data into memory.
In naive-store-items not all data types have their own collections, only data types marked as top
level t have their own collections and filesfiles. Non top level type objectss are stored in their referencing data type's collections. 

cl-wfx uses this to structure data in a hierarchical grid.")  
   (fields :initarg :fields
	   :accessor fields
	   :initform nil
	   :documentation "Field definitions that represents a data unit."))
  (:documentation "A class that can be use to that represents a complex data unit.

The default implementation of cl-naive-store is unaware of data-types when reading and 
writing data objects to and from file. This was by design, to place as little burden on reading and writing
data objects. Depending on the use of naive-store a user could customize the reading and writing methods of 
naive-store to use data-types for validation and file layout specifics. 

GUI's like cl-wfx use these to help with generic rendering of user input screens. 

See cl-naive-type-defs:*example-type-defs* for examples of type definitions to get a feel for the intended use."))



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

(defclass data-type-collection-mixin ()
  ((data-type :initarg :data-type
	      :accessor data-type
	      :initform nil
	      :documentation "The data-type that this collection contains objects of."))
  
  (:documentation "Collection extention to make collection of a specific data-type."))


(defmethod persist-collection-def ((collection data-type-collection-mixin))
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

(defclass data-type-store-mixin ()
  ((data-type-class :initarg :data-type-class
	       :accessor data-type-class
	       :initform 'data-type
	       :allocation :class
	       :documentation "Then class that should be used to make data-type objects.")
   (data-types :initarg :data-types
	       :accessor data-types
	       :initform nil
	       :documentation "List of data-types represented by this store's collections.")))

(defmethod get-collection-from-def ((store data-type-store-mixin) collection-name)
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
      (let ((data-type (get-data-type store (getf collection-def :data-type))))
	(unless data-type
	  (load-store-data-types store)
	  (setf data-type (get-data-type store (getf collection-def :data-type))))

	(unless data-type
	  (error "Collection data-type could not be found."))
	
	(when data-type
	  
	  (make-instance (collection-class store)
			 :store store
			 :name (getf collection-def :name)
			 :location (getf collection-def :location)
			 :data-type data-type))))))

(defgeneric get-data-type (store type-name)
  (:documentation "Returns a data-type object if found in the store."))

(defmethod get-data-type ((store data-type-store-mixin) type-name)
  (dolist (data-type (data-types store))
    (when (string-equal type-name (name data-type))
      (return-from get-data-type data-type))))

(defgeneric add-data-type (store data-type)
  (:documentation "Adds a data-type to a store."))

(defmethod add-data-type ((store data-type-store-mixin) (data-type data-type))
  (unless (get-data-type store (name data-type))
    (setf (store data-type) store)
    (pushnew data-type (data-types store))
    (persist data-type))
  data-type)


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


(defmethod load-store-collections ((store data-type-store-mixin) &key with-data-p &allow-other-keys)
  "Finds and loads collection definitions for a store, with or without data objects."
  (let ((files (find-collection-definitions store)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	
	(when file-contents
	  
	  (let ((data-type (get-data-type store (getf file-contents :data-type)))
		(collection))
	    
	    (unless data-type
	      (load-store-data-types store)
	      (setf data-type (get-data-type store (getf file-contents :data-type))))

	    (unless data-type
	      (error "Collection data-type not found."))

	    (when data-type
	      (setf collection (add-collection 
				store 
				(make-instance (collection-class store)
					       :name (getf file-contents :name)
					       :location (getf file-contents :location)
					       :data-type data-type
					       :filter (getf file-contents :filter))))
	      (when with-data-p
		(load-data collection)))))))))

(defmethod load-store ((store data-type-store-mixin) &key with-data-p &allow-other-keys)
  (load-store-data-types store)
  (load-store-collections store with-data-p))
