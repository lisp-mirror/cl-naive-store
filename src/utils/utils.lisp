(in-package :cl-naive-store.utils)

(defgeneric make-elements (document-type document-type-def)
  (:documentation "Convert the elements of the document-type-def into a list of ELEMENT instances."))

(defmethod make-elements (document-type document-type-def)
  (declare (ignore document-type))
  (mapcar (lambda (element)
            (make-instance 'cl-naive-store.document-types:element
                           :name (getf element :name)
                           :key-p (getf element :key-p)
                           :concrete-type (getf element :concrete-type)
                           :attributes (getf element :attributes)))
          (getf document-type-def :elements)))

(defgeneric definition-keys (document-type document-type-def)
  (:documentation "Get keys form document-type-def."))

(defmethod definition-keys (document-type document-type-def)
  (declare (ignore document-type))
  (mapcar (lambda (element)
            (getf element :name))
          (getf document-type-def :elements)))

(defgeneric implement-document-definition (store document-type-def &key collection-name indexes)
  (:documentation "Adds a document-type based on the definition to the
  store. Not all document-types are stored in their own collections so
  the user needs to explicitly indicate if a collection is
  required.

  Then indexes for a collection can also be specified, the keys are
  calculated from the type def.

  Returns (values document-type [collection])
"))

(defmethod implement-document-definition (store document-type-def &key collection-name indexes)
  (let ((document-type (make-instance
                        'cl-naive-store.document-types:document-type
                        :name (getf document-type-def :name)
                        :label (getf document-type-def :label))))

    (setf (cl-naive-store.document-types:elements document-type)
          (make-elements document-type document-type-def))

    (if collection-name
        (values document-type
                (add-collection
                 store
                 (make-instance 'cl-naive-store.naive-documents:document-collection
                                :name collection-name
                                :document-type (add-document-type
                                                store
                                                document-type)
                                :keys (definition-keys document-type
                                                       document-type-def)
                                ;;Specifying the elements to set up indexes for.
                                :indexes indexes)))
        (values document-type nil))))

(defgeneric implement-definitions-collection (store definitions-collection
                                              &key load-data-p load-data-parallel-p)
  (:documentation "Takes a definitions collection and bootstraps the
  definitions and collections for a store.

Boostrap means collection and data types are loaded for the store.

Any peristed data is not loaded for the collections! If data should be
load use laod-data-p."))

(defmethod implement-definitions-colllection (store definitions-collection
                                              &key load-data-p (load-data-parallel-p nil))
  "We run through the collection of definitons first to add all the
type defs to the store, because we need the definitions to create the
collections. Then we map accross the list of discovered collections
from the first itteration and create any collections.
"

  (let ((collections))
    (mapcar (lambda (def)
              (if (equal (car def) :document-type)
                  (add-document-type
                   store
                   (make-instance
                    'cl-naive-store.document-types:document-type
                    :name (getf (getx def :document-type) :name)
                    :label (getf (getx def :document-type) :label)
                    :elements (make-elements nil (getx def :document-type))))
                  (push def collections)))
            definitions-collection)

    (mapcar (lambda (def)
              (when (equal (car def) :collection)
                (let* ((document-type (cl-naive-store.naive-core:get-document-type
                                       store
                                       (getf (second def) :data-type)))
                       (collection
                         (add-collection
                          store
                          (make-instance 'cl-naive-store.naive-documents:document-collection
                                         :name (getf (getx def :collection) :name)
                                         :document-type document-type

                                         :keys (definition-keys document-type
                                                                def)
                                         ;;Specifying the elements to set up indexes for.
                                         :indexes (getf (getx def :collection) :indexes)))))
                  (when load-data-p
                    (load-data collection :parallel-p load-data-parallel-p)))))
            collections)))

