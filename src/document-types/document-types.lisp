(in-package :cl-naive-store.document-types)

(defclass element ()
  ((name :initarg :name
         :accessor name
         :initform nil
         :documentation "Name of the element. This should be a KEYWORD if you want data portability and some internals might expect a keyword.")
   (concrete-type :initarg :concrete-type
                  :accessor concrete-type
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
               :documentation "A property list of additional element attributes."))

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

element-class is declaratively specified here so that elements can be dynamicly created when definition type definitions are read from file. See naive-store-documents for usage examples. ")
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

;;TODO:Need to hunt down instances where this function can be used instead of the more
;;verbose code lying around.
;;currently not used any where?
(defgeneric document-of-type-p (document document-type))

(defmethod getx ((element element) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :name)
         (name element))
        ((equalp accessor :concrete-type)
         (concrete-type element))
        ((equalp accessor :key-p)
         (key-p element))
        ((equalp accessor :attributes)
         (attributes element))))

(defmethod (setf getx) (value (element element) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :name)
         (setf (name element) value))
        ((equalp accessor :concrete-type)
         (setf (concrete-type element) value))
        ((equalp accessor :key-p)
         (setf (key-p element) value))
        ((equalp accessor :attributes)
         (setf (attributes element) value))))

(defmethod getx ((document-type document-type) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :store)
         (store document-type))
        ((equalp accessor :name)
         (name document-type))
        ((equalp accessor :element-class)
         (element-class document-type))
        ((equalp accessor :label)
         (label document-type))
        ((equalp accessor :elements)
         (elements document-type))))

(defgeneric get-attribute (element attribute))

(defmethod get-attribute ((element element) attribute)
  (dolist (attribute (attributes element))
    ;;TODO: Lazy matching should use cond with more inteligent matching
    (when (string-equal (getx attribute :name) (format nil "~A" attribute))
      (return-from get-attribute attribute))))

(defgeneric get-element (document-type element))

(defmethod get-element (document-type element)
  (dolist (element (elements document-type))
    ;;TODO: Lazy matching should use cond with more inteligent matching
    (when (string-equal (getx element :name) (format nil "~A" element))
      (return-from get-element element))))

(defmethod (setf getx) (value (document-type document-type) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :store)
         (setf (store document-type) value))
        ((equalp accessor :name)
         (setf (name document-type) value))
        ((equalp accessor :element-class)
         (setf (element-class document-type) value))
        ((equalp accessor :label)
         (setf (label document-type) value))
        ((equalp accessor :elements)
         (setf (elements document-type) value))))

(defmethod persist ((document-type document-type) &key &allow-other-keys)
  "Persists a document-type definition. Path to file is of this general format /universe/store-name/document-type-name.type."
  (let ((elements (mapcar (lambda (element)
                            (list :name (name element)
                                  :key-p (key-p element)
                                  :concrete-type (concrete-type element)
                                  :attributes (attributes element)))
                          (elements document-type))))

    (naive-impl:write-to-file (cl-fad:merge-pathnames-as-file
                               (pathname (location (store document-type)))
                               (make-pathname :name (name document-type)
                                              :type "type"))
                              (list :name (name document-type)
                                    :label (label document-type)
                                    :class (type-of document-type)
                                    ;;TODO: naive-store does not use
                                    ;;top-level-p internally anywhere
                                    ;;so can we remove it?
                                    ;;Looks like only cl-wfx grid needs this.
                                    :top-level-p t
                                    :elements elements)
                              :if-exists :supersede)))

(defclass document-type-collection-mixin ()
  ((document-type :initarg :document-type
                  :accessor document-type
                  :initform nil
                  :documentation "The document-type that this collection contains documents of."))
  (:documentation "Collection extention to make collection of a specific document-type."))

(defmethod add-multiverse-element ((store store) (collection collection) &key persist-p)
  (if (not (store collection))
      (setf (store collection) store)
      (unless (eql (store collection) store)
        (error "Collection already references a different store instance!")))

  (unless (get-multiverse-element :collection store (name collection))
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
      (when persist-p
        (persist-collection-def collection))))
  collection)

(defmethod add-multiverse-element :after ((store store) (collection document-type-collection-mixin) &key persist-p)
  "Uses document type to figure out what the keys of the collection are."
  (declare (ignore persist-p))
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
    :class (type-of collection)
    :location (location collection)
    :document-type (and (document-type collection) (name (document-type collection))))

   :if-exists :supersede))

(defclass document-type-store-mixin ()
  ((document-type-class :initarg :document-type-class
                        :accessor document-type-class
                        :initform 'document-type
                        :allocation :class
                        :documentation "The class that should be used to make document-type documents.
IMPL NOTES: To deal with customization of document-type.")
   (document-types :initarg :document-types
                   :accessor document-types
                   :initform nil
                   :documentation "List of document-types represented by this store's collections.")))

(defmethod cl-naive-store.naive-core:instance-from-definition
    ((store store)(definition-type (eql :document-type))
     definition &key class persist-p)

  (let ((instance (raw-instance-from-definition definition-type definition
                                                :class class)))
    (when instance
      (setf (store instance) store)
      (add-multiverse-element store instance :persist-p persist-p))

    instance))

(defmethod get-collection-from-def ((store document-type-store-mixin) collection-name)
  "Tries to find the collection definition file on disk and loads it into the store, but it does not load the collection's data.
Needs to find the associated docment-type as well which might mean loading all the document types for the store."
  (let ((collection-def (naive-impl:sexp-from-file
                         (cl-fad:merge-pathnames-as-file
                          (pathname (location store))
                          (make-pathname :name collection-name
                                         :type "col")))))

    (when collection-def
      (let ((document-type (get-document-type
                            store
                            (getx collection-def :document-type))))
        (unless document-type
          (load-store-document-types store)
          (setf document-type (get-document-type
                               store
                               (getx collection-def :document-type))))

        (unless document-type
          (error "Collection document-type could not be found."))

        (let ((collection (make-instance (collection-class store)
                                         :store store
                                         :name (getx collection-def :name)
                                         :location (getx collection-def :location)
                                         :document-type document-type)))
          (setf (location collection) (ensure-location collection))
          collection)))))

(defgeneric get-document-type-from-def (store document-type-name)
  (:documentation "Tries to find the document definition on disk."))

(defmethod get-document-type-from-def ((store store) document-type-name)
  (let ((document-document-type
          (naive-impl:sexp-from-file (cl-fad:merge-pathnames-as-file
                                      (pathname (location store))
                                      (make-pathname :name document-type-name
                                                     :type "type")))))
    (when document-document-type
      (let ((document-type (cl-naive-store.naive-core:add-multiverse-element
                            store
                            (make-instance (or
                                            (getx document-document-type :class)
                                            (document-type-class store))
                                           :name (getx document-document-type :name)
                                           :label (getx document-document-type :label)
                                           :elements nil))))
        (setf (elements document-type)
              (mapcar (lambda (element)
                        (make-instance (element-class document-type)
                                       :name (getx element :name)
                                       :key-p (getx element :key-p)
                                       :concrete-type (getx element :concrete-type)
                                       :attributes (getx element :attributes)))
                      (getx document-document-type :elements)))
        document-type))))

(defmethod cl-naive-store.naive-core:get-multiverse-element
    ((element-type (eql :document-type))
     (store store) name)
  (cl-naive-store.naive-core::get-multiverse-element* store document-types))

(defgeneric get-document-type (store type-name)
  (:documentation "Returns a document-type document if found in the store."))

(defmethod get-document-type ((store document-type-store-mixin) type-name)
  (get-multiverse-element :document-type store type-name))

(defmethod cl-naive-store.naive-core:add-multiverse-element
    ((store store) (document-type document-type) &key persist-p)

  (if (not (store document-type))
      (setf (store document-type) store)
      (unless (eql (store document-type) store)
        (error
         "Document-Type already references a different STORE instance!")))
  (unless (get-multiverse-element :document-type store (name document-type))
    ;;TODO: Document does not have a location as lon as the store
    ;;location is ok all should be good??
    ;;(cl-naive-store.naive-core::set-and-ensure-locations store
    ;;document-type)
    (setf (store document-type) store)
    (if persist-p
        (persist document-type))
    (pushnew document-type (document-types store)))
  document-type)

(defparameter *global-css* (make-hash-table :test #'equalp))

(defmethod global-css :around (what)
  (if (boundp '*global-css*)
      (setf (gethash what *global-css*) (call-next-method))
      (progn
        (print (call-next-method))
        (error "*global-css* special variable is unbound"))))

(defgeneric add-document-type (store document-type &key persist-p)
  (:documentation "Adds a document-type to a store."))

(defmethod add-document-type ((store document-type-store-mixin)
                              (document-type document-type)
                              &key (persist-p t))

  (add-multiverse-element store document-type :persist-p persist-p))

(defun load-store-document-types (store)
  "Finds and loads the files representing data types for a store."
  (let ((files (directory
                (cl-fad:merge-pathnames-as-file
                 (pathname (location store))
                 (make-pathname :directory '(:relative :wild-inferiors)
                                :name :wild
                                :type "type")))))
    (dolist (file files)
      (let ((type-contents (naive-impl:sexp-from-file file)))

        (unless (getx type-contents :name)
          (error "Type contents is nil but the file exists. ~%~%File: ~S~%Contents: ~S~%Contents Queried on Error again: ~S"
                 file type-contents
                 (naive-impl:sexp-from-file file)))

        (let ((elements)
              (document-type (cl-naive-store.naive-core:add-multiverse-element
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
                                 :concrete-type (getx element :concrete-type)
                                 :attributes (getx element :attributes)))))

            setf elements)
          (setf (elements document-type) elements))))))

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
          (let ((document-type
                  (get-document-type store
                                     (or
                                      (getx file-contents :document-type)
                                      ;;TODO: remove later, backward compatibility issue
                                      (getx file-contents :document-type))))
                (collection))

            (unless document-type
              (load-store-document-types store)
              (setf document-type
                    (get-document-type store
                                       (or
                                        (getx file-contents :document-type)
                                        ;;TODO: remove later, backward compatibility issue
                                        (getx file-contents :document-type)))))

            (unless document-type
              (error "Collection document-type not found."))

            (when document-type
              (setf collection (add-collection
                                store
                                (make-instance (collection-class store)
                                               :name (getx file-contents :name)
                                               :location (getx file-contents :location)
                                               :document-type document-type)))
              (when with-data-p
                (load-data collection)))))))))

(defmethod load-store ((store document-type-store-mixin) &key with-data-p &allow-other-keys)
  (load-store-document-types store)
  (load-store-collections store :with-data-p with-data-p))

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
      ;;Raising an error here because there is a problem with datatype specifications some where.
      (error "index-keys called with document-type = nil.
cl-wfx tip: If this happened on a save look for a mismatch between a collection and its document-type's destinations"))

    (values-from-key-elements% (elements document-type) document)))
