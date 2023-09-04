(in-package :cl-naive-store.document-types)

(defclass element ()
  ((name :initarg :name
         :accessor name
         :initform nil
         :documentation "Name of the element. This should be a KEYWORD if you want data portability and some internals might expect a keyword.")
   (document-type :initarg :document-type
                  :accessor document-type
                  :initform nil
                  :documentation "The document-type that this element belongs to.")
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
   (element-class :initarg :element-class
                  :accessor element-class
                  :initform 'element
                  :allocation :class
                  :documentation "The class that should be used to make element documents.
NOTES:

element-class is declaratively specified here so that elements can be dynamicly created when definition type definitions are read from file. See naive-store-documents for usage examples. ")
   (name :initarg :name
         :accessor name
         :initform nil
         :documentation "String representing a document-type name.")

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

(defclass document-type-collection-mixin ()
  ((document-type :initarg :document-type
                  :accessor document-type
                  :initform nil
                  :documentation "The document-type that this collection contains documents of."))
  (:documentation "Collection extention to make collection of a specific document-type."))

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

(defmethod getx ((store document-type-store-mixin) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :universe)
         (universe store))
        ((equalp accessor :name)
         (name store))
        ((equalp accessor :collections)
         (collections store))
        ((equalp accessor :collection-class)
         (collection-class store))
        ((equalp accessor :document-types)
         (document-types store))
        ((equalp accessor :document-type-class)
         (document-type-class store))
        ((equalp accessor :location)
         (location store))))

(defmethod (setf getx) (value (store document-type-store-mixin) accessor
                        &key &allow-other-keys)
  ""
  (cond ((equalp accessor :universe)
         (setf (universe store) value))
        ((equalp accessor :name)
         (setf (name store) value))
        ((equalp accessor :collections)
         (setf (collections store) value))
        ((equalp accessor :collection-class)
         (setf (collection-class store) value))
        ((equalp accessor :document-types)
         (setf (document-types store) value))
        ((equalp accessor :document-type-class)
         (setf (document-type-class store) value))
        ((equalp accessor :location)
         (setf (location store) value))))

(defmethod cl-naive-store.naive-core:query-multiverse
    ((element element) fn)
  (let ((result))
    (let ((fn-result (funcall fn element)))
      (when fn-result
        (push fn-result result)))))

(defmethod cl-naive-store.naive-core:query-multiverse
    ((collection document-type-collection-mixin) fn)
  (let ((result))
    (let ((fn-result (funcall fn collection)))
      (when fn-result
        (push fn-result result)))))

(defmethod cl-naive-store.naive-core:query-multiverse
    ((document-type document-type) fn)
  (let ((result (mapcar (lambda (element)
                          (query-multiverse element fn))
                        (elements document-type))))
    (let ((fn-result (funcall fn document-type)))
      (when fn-result
        (push fn-result result)))))

(defmethod cl-naive-store.naive-core:query-multiverse
    ((store document-type-store-mixin) fn)
  (let ((result (append
                 (mapcar (lambda (document-type)
                           (query-multiverse document-type fn))
                         (document-types store))
                 (mapcar (lambda (collection)
                           (query-multiverse collection fn))
                         (collections store)))))
    (let ((fn-result (funcall fn store)))
      (when fn-result
        (push fn-result result)))))

;;TODO:Need to hunt down instances where this function can be used instead of the more
;;verbose code lying around.
;;currently not used any where?
(defgeneric document-of-type-p (document document-type))

(defmethod getx ((element element) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :document-type)
         (document-type element))
        ((equalp accessor :name)
         (name element))
        ((equalp accessor :concrete-type)
         (concrete-type element))
        ((equalp accessor :key-p)
         (key-p element))
        ((equalp accessor :attributes)
         (attributes element))))

(defmethod (setf getx) (value (element element) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :document-type)
         (setf (document-type element) value))
        ((equalp accessor :name)
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

(defmethod cl-naive-store.naive-core:persist
    ((document-type document-type) &key &allow-other-keys)
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
                                    :elements elements)
                              :if-exists :supersede)))

(defmethod cl-naive-store.naive-core:get-multiverse-element
    ((element-type (eql :element)) (document-type document-type) name)
  (cl-naive-store.naive-core::get-multiverse-element* document-type elements name))

(defmethod cl-naive-store.naive-core:get-multiverse-element
    ((element-type (eql :document-type))
     (store document-type-store-mixin) name)
  (cl-naive-store.naive-core::get-multiverse-element* store document-types name))

(defmethod cl-naive-store.naive-core:add-multiverse-element
    ((document-type document-type) (element element)
     &key persist-p)
  (if (not (document-type element))
      (setf (document-type element) document-type)
      (unless (eql (document-type element) document-type)
        (error "Element already references a different document-type instance!")))

  (unless (get-multiverse-element :element document-type (name element))

    (pushnew element (elements document-type))

    ;;Elements are not peristed in seperate files they are part of the
    ;;document-type file
    (when persist-p
      (persist document-type)))
  element)

(defmethod cl-naive-store.naive-core:add-multiverse-element
    ((store document-type-store-mixin)
     (collection collection) &key persist-p)
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

      ;;TODO: Change to proper persist.
      (when persist-p
        (persist-collection-def collection))))
  collection)

(defmethod cl-naive-store.naive-core:add-multiverse-element :after
    ((store document-type-store-mixin)
     (collection document-type-collection-mixin)
     &key persist-p)
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

(defmethod cl-naive-store.naive-core:instance-from-definition
    ((class (eql 'element)) definition)

  (let ((definition-body (cl-naive-store.naive-core:definition-body
                          definition)))
    (make-instance (or
                    (getx definition-body :class)
                    class)
                   :name (getx definition-body :name)
                   :concrete-type (getx definition-body :concrete-type)
                   :key-p (getx definition-body :key-p)
                   :attributes (getx definition-body :attributes))))

(defmethod cl-naive-store.naive-core:load-from-definition
    ((document-type document-type) (definition-type (eql :element))
     definition &key class persist-p with-children-p with-data-p)

  (declare (ignore with-data-p))

  (let ((definition-body (cl-naive-store.naive-core:definition-body
                          definition))
        (instance (instance-from-definition (or class 'element) definition)))

    (when with-children-p
      (dolist (child-definition (getx definition-body :elements))
        (cl-naive-store.naive-core:load-from-definition
         instance
         :element
         child-definition
         :class (getx definition-body :element-class)
         :persist-p persist-p)))

    (when instance
      (setf (document-type instance) document-type)
      (add-multiverse-element document-type instance :persist-p persist-p))

    instance))

(defmethod cl-naive-store.naive-core:instance-from-definition
    ((class (eql 'document-type)) definition)

  (let ((definition-body (cl-naive-store.naive-core::definition-body
                          definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :label (getx definition-body :label)
                   :element-class (getx definition-body :element-class))))

(defmethod cl-naive-store.naive-core:load-from-definition
    ((store document-type-store-mixin) (definition-type (eql :document-type))
     definition &key class persist-p with-children-p with-data-p)

  (declare (ignore with-data-p))

  (let* ((definition-body (cl-naive-store.naive-core::definition-body
                           definition))
         (instance (instance-from-definition (or (getx definition-body :class)
                                                 class
                                                 (getx store :document-type-class)
                                                 'document-type)
                                             definition)))

    (when with-children-p
      (dolist (child-definition (getx definition-body :elements))
        (cl-naive-store.naive-core:load-from-definition
         instance
         :element
         child-definition
         :class (getx definition-body :element-class)
         :persist-p persist-p
         :with-children-p with-children-p)))

    (when instance
      (setf (store instance) store)
      (add-multiverse-element store instance :persist-p persist-p))

    instance))

;;TODO: instance-from-definition to set document-class of store!!!!!!

(defmethod cl-naive-store.naive-core:instance-from-definition
    ((class (eql 'document-type-store-mixin))
     definition)
  (let ((definition-body (cl-naive-store.naive-core::definition-body definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :location (getx definition-body :location)
                   :collection-class (getx definition-body :collection-class)
                   :document-type-class (getx definition-body :document-type-class))))

(defmethod cl-naive-store.naive-core:load-from-definition-file
    (parent
     (definition-type (eql :element))
     name &key class
     with-children-p
     with-data-p)
  (declare (ignore parent definition-type definition-type
                   name class with-children-p with-data-p))
  (error "Elements are part of the document-type and do not have files of their own."))

(defmethod cl-naive-store.naive-core:load-from-definition
    ((store document-type-store-mixin)
     (definition-type (eql :collection))
     definition &key class persist-p
     with-children-p
     with-data-p)

  (declare (ignore with-children-p))

  (let* ((definition-body (cl-naive-store.naive-core::definition-body definition))
         (instance (instance-from-definition (or
                                              (getx definition-body :class)
                                              class
                                              (getx store :collection-class)
                                              'document-type-collection-mixin)
                                             definition))
         (document-type-definition (and (getx definition-body :document-type)
                                        (cl-naive-store.naive-core:get-definition
                                         (location instance)
                                         :document-type
                                         (getx definition-body :document-type)))))

    (when (getx definition-body :document-type)
      (let ((document-type (get-document-type
                            store
                            (getx definition-body :document-type))))

        (unless document-type
          (setf document-type (load-from-definition
                               store
                               :document-type document-type-definition
                               :class (getx definition-body :collection-class)
                               :persist-p persist-p)))

        (unless document-type
          (error "Collection document-type could not be found."))))

    (when persist-p
      (unless (location instance)
        (error "Cannot persist the colleciton, there is no location."))
      (persist instance))

    (when with-data-p
      (load-data instance))

    (when instance
      (setf (store instance) store))

    instance))

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

(defmethod cl-naive-store.naive-core:get-multiverse-element
    ((element-type (eql :document-type))
     (store store) name)
  (cl-naive-store.naive-core::get-multiverse-element* store document-types name))

(defgeneric get-document-type (store type-name)
  (:documentation "Returns a document-type document if found in the store."))

(defmethod get-document-type ((store document-type-store-mixin) type-name)
  (get-multiverse-element :document-type store type-name))

(defmethod cl-naive-store.naive-core:add-multiverse-element
    ((document-type document-type) (element element) &key persist-p)
  (cl-naive-store.naive-core::add-multiverse-element* document-type
                                                      element
                                                      persist-p
                                                      document-type element
                                                      elements))

(defmethod cl-naive-store.naive-core:add-multiverse-element
    ((store store) (document-type document-type) &key persist-p)

  (if (not (store document-type))
      (setf (store document-type) store)
      (unless (eql (store document-type) store)
        (error
         "Document-Type already references a different STORE instance!")))

  (unless (get-multiverse-element :document-type store (name document-type))
    (when persist-p
      (persist document-type))
    (pushnew document-type (document-types store)))
  document-type)

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
        (setf document-type
              (cl-naive-store.naive-core::instance-from-definition-file
               (store collection)
               :document-type document-type))))

    (unless document-type
      ;;Raising an error here because there is a problem with datatype specifications some where.
      (error "key-values called with document-type = nil.
cl-wfx tip: If this happened on a save look for a mismatch between a collection and its document-type's destinations"))

    (values-from-key-elements% (elements document-type) document)))
