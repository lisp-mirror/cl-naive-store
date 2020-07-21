(in-package :cl-naive-document-types)

(defclass element ()
  ((name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "Name of the element. This should be a KEYWORD if you want data portability and some internals might expect a keyword.")
   (type-def :initarg :type-def
	     :accessor type-def
	     :initform nil
	     :documentation "A user defined \"thing\" that defines the type specifics of an element.")
   (key-p :initarg :key-p
	  :accessor key-p
	  :initform nil
	  :documentation "Indicates that the element is part of the primary key.

 Can be used for indexing and document comparison. For example when a new document is persisted naive-store-documents checks for documents with the same index value and then updates the existing document.")
   (attributes :initarg :attributes
	       :accessor attributes
	       :initform nil
	       :documentation "A property list of additional element attributes that are not data type specific."))
  
  (:documentation "A definition of an element of a document.

NOTES: 

Elements can reference simple types, a complex document or documents based on other document-types. 

naive-store can be used as a hierarchical database or a flat databases or a mix."))

(defclass document-type ()
  ((store :initarg :store
	  :accessor store
	  :initform nil
	  :documentation "The store that this document-type belongs to.")
   (name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "String representing a document-type name.")
   (element-class :initarg :element-class
	       :accessor element-class
	       :initform 'element
	       :allocation :class
	       :documentation "The class that should be used to make element documents. 
NOTES:

element-class is declaratively specified here because so that elements can be dynamicly created when definition type definitions are read from file. See naive-store-documents for usage examples. ")
   (label :initarg :label
	  :accessor label
	  :initform nil
	  :documentation "Human readable/formated short description.")   
   (elements :initarg :elements
	   :accessor elements
	   :initform nil
	   :documentation "Field definitions that represents a data unit."))
  (:documentation "A class that can be use to represent a complex document.

NOTES:

The default implementation of cl-naive-store is unaware of document-types when reading and writing documents to and from file. This was by design, to place as little burden on reading and writing documents. Depending on the use of naive-store a user could customize the reading and writing methods of naive-store to use document-types for validation and file layout specifics. 

GUI's like cl-wfx use these to help with generic rendering of user input screens. 

See cl-naive-type-defs:*example-type-defs* for examples of type definitions to get a feel for the intended use."))



(defmethod persist ((document-type document-type) &key &allow-other-keys)
  "Persists a document-type definition. Path to file is of this general format
/universe/store-name/document-type-name.type."
  (let ((elements))    
    (dolist (element (elements document-type))
      (setf elements (append elements (list (list
					 :name (name element)
					 :key-p (key-p element)
					 :type-def (type-def element)
					 :attributes (attributes element))))))

    (naive-impl:write-to-file
     (cl-fad:merge-pathnames-as-file
	       (pathname (location (store document-type)))
	       (make-pathname :name (name document-type)
			      :type "type"))
     (list 
      :name (name document-type)
      :label (label document-type)
      :top-level-p t
      :elements elements)
     :if-exists :supersede)))

(defclass document-type-collection-mixin ()
  ((document-type :initarg :document-type
	      :accessor document-type
	      :initform nil
	      :documentation "The document-type that this collection contains documents of."))
  
  (:documentation "Collection extention to make collection of a specific document-type."))

(defmethod add-collection :after ((store store) (collection document-type-collection-mixin))
  (when (or
	 (not (keys collection))
	 (equalp (keys collection) '(:key)))
    (when (document-type collection)
      (let ((document-type (typecase (document-type collection)
				  (document-type
				   (document-type collection))
				  (t
				   (get-document-type store (document-type collection))))))
	(when document-type
	  (let (keys)
	    (dolist (element (elements document-type))	      
	      (when (key-p element)
		(push (name element) keys)))
	    (when keys
	      (setf (keys collection) (nreverse keys)))))))))

(defmethod persist-collection-def ((collection document-type-collection-mixin))
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location (store collection)))
	       (make-pathname :name (name collection)
			      :type "col"))
   
    (list 
    :name (name collection)
    :location (location collection)
    :document-type (and (document-type collection) (name (document-type collection))))
		 		 
    :if-exists :supersede))

(defclass document-type-store-mixin ()
  ((document-type-class :initarg :document-type-class
	       :accessor document-type-class
	       :initform 'document-type
	       :allocation :class
	       :documentation "Then class that should be used to make document-type documents.
IMPL NOTES: To deal with customization of document-type.")
   (document-types :initarg :document-types
	       :accessor document-types
	       :initform nil
	       :documentation "List of document-types represented by this store's collections.")))


(defmethod get-collection-from-def ((store document-type-store-mixin) collection-name)
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
      (let ((document-type (get-document-type store (getx collection-def :document-type))))
	(unless document-type
	  (load-store-document-types store)
	  (setf document-type (get-document-type store (getx collection-def :document-type))))

	(unless document-type
	  (error "Collection document-type could not be found."))
	
	(when document-type
	  
	  (make-instance (collection-class store)
			 :store store
			 :name (getx collection-def :name)
			 :location (getx collection-def :location)
			 :document-type document-type))))))

(defgeneric get-document-type-from-def (store document-type-name)
  (:documentation "Tries to find the document definition on disk."))

(defmethod get-document-type-from-def ((store store) document-type-name)
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location store))
		   (make-pathname :name document-type-name
				  :type "type")))
	(document-type-def))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax
	(when in
	  (setf document-type-def (read in nil))
	  (close in))))

    (when document-type-def
      (let ((elements)
	    (document-type (add-document-type 
			    store 
			    (make-instance (document-type-class store)
					   :name (getx document-type-def :name)
					   :label (getx document-type-def :label)
					   :elements nil))))
	
	(dolist (element (getx document-type-def :elements))
	 (setf elements 
		(append elements 
			(list (make-instance
			       (element-class document-type)
			       :name (getx element :name)
			       :key-p (getx element :key-p)
			       :type-def (getx element :type-def)
			       :attributes (getx element :attributes)))))
	  
	  setf elements )
	(setf (elements document-type) elements)
	
	document-type))))

(defgeneric get-document-type (store type-name)
  (:documentation "Returns a document-type document if found in the store."))

(defmethod get-document-type ((store document-type-store-mixin) type-name)
  (dolist (document-type (document-types store))
    (when (string-equal type-name (name document-type))
      (return-from get-document-type document-type))))

(defgeneric add-document-type (store document-type)
  (:documentation "Adds a document-type to a store."))

(defmethod add-document-type ((store document-type-store-mixin) (document-type document-type))
  (unless (get-document-type store (name document-type))
    (setf (store document-type) store)
    (pushnew document-type (document-types store))
    (persist document-type))
  document-type)


(defun load-store-document-types (store)
  "Finds and loads the files representing data types for a store."
  (let ((files (directory
		(cl-fad:merge-pathnames-as-file
		 (pathname (location store))
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
      
      (let ((elements)
	    (document-type (add-document-type 
			    store 
			    (make-instance (document-type-class store)
					   :name (getx type-contents :name)
					   :label (getx type-contents :label)
					   :elements nil))))
	
	(dolist (element (getx type-contents :elements))
	 (setf elements 
		(append elements 
			(list (make-instance
			       (element-class document-type)
			       :name (getx element :name)
			       :key-p (getx element :key-p)
			       :type-def (getx element :type-def)
			       :attributes (getx element :attributes)))))
	  
	  setf elements )
	(setf (elements document-type) elements)))))


(defmethod load-store-collections ((store document-type-store-mixin) &key with-data-p
								       &allow-other-keys)
  "Finds and loads collection definitions for a store, with or without data documents."
  (let ((files (find-collection-definitions store)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	
	(when file-contents	  
	  (let ((document-type (get-document-type store (getx file-contents :document-type)))
		(collection))
	    
	    (unless document-type
	      (load-store-document-types store)
	      (setf document-type (get-document-type store (getx file-contents :document-type))))

	    (unless document-type
	      (error "Collection document-type not found."))

	    (when document-type
	      (setf collection (add-collection 
				store 
				(make-instance (collection-class store)
					       :name (getx file-contents :name)
					       :location (getx file-contents :location)
					       :document-type document-type
					       :filter (getx file-contents :filter))))
	      (when with-data-p
		(load-data collection)))))))))

(defmethod load-store ((store document-type-store-mixin) &key with-data-p &allow-other-keys)
  (load-store-document-types store)
  (load-store-collections store with-data-p))

(defun values-from-key-elements% (elements document)
    (let ((keys)
	(values (document-values document)))
    (dolist (element elements)     
      (when (key-p element)
	(push (list (name element) nil (getx values (name element)))
	      keys)))
    (reverse keys)))

(defmethod key-values ((collection document-type-collection-mixin) document &key &allow-other-keys)
  (let ((document-type (document-type collection)))
    (when (stringp document-type)
      ;;If types have not been loaded yet load type.
      (unless (document-types (store collection))
	(get-document-type-from-def (store collection) document-type))
      
      (setf document-type (get-document-type (store collection) document-type)))
    
    (unless document-type
    ;;Raising an error here because its problem with datatype specifications some where.
      (error "index-keys called with document-type = nil. 
cl-wfx tip: If this happened on a save look for a mismatch between a collection and its document-type's destinations"))
    
    (values-from-key-elements% (elements document-type) document)))
