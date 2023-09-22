(in-package :cl-naive-store.utils)

(defun naive-document-to-sexp (document &key (versions-p t))
  "Converts a naive-documents:document to a sexp."

  (let ((dochash (make-hash-table :test #'equalp)))
    (labels ((parse-values (value)
               (if (listp value)
                   (loop for val in value
                         collect (parse-values val))
                   (if (cl-naive-store.naive-documents:document-p value)
                       (or (gethash (getx value :hash) dochash)
                           (naive-document-to-sexp value))
                       value))))

      (let ((sexp `(:universe ,(name
                                (universe
                                 (store
                                  (cl-naive-store.naive-documents:document-collection
                                   document))))
                    :store ,(name
                             (store
                              (cl-naive-store.naive-documents:document-collection
                               document)))
                    :collection ,(name
                                  (cl-naive-store.naive-documents:document-collection
                                   document))
                    :document-type
                    ,(name
                      (cl-naive-store.naive-documents:document-document-type
                       document))
                    :hash
                    ,(cl-getx:getx document :hash)
                    :elements
                    ,(loop for (key value) on (getx document :elements~)
                           by #'cddr
                           collect key
                           collect (parse-values value))

                    :versions
                    ,(when versions-p
                       (loop for version in (getx
                                             document :versions~)
                             collect (loop for (key value) on version
                                           by #'cddr
                                           collect key
                                           collect (parse-values value))))
                    :deleted-p ,(cl-naive-store.naive-documents:document-deleted-p
                                 document))))
        (setf (gethash (getx document :hash) dochash) sexp)
        sexp))))

(defun naive-documents-to-sexps (documents)
  "Used to prepare docs to be sent over HTTP or shared with some other
system that does not have access to cl-naive-store."

  (loop for document in documents
        collect
        (naive-document-to-sexp document)))

(defun existing-document% (collection document)
  (or (and (cl-naive-store.naive-indexed:hash document)
           (cl-naive-store.naive-indexed:index-lookup-hash
            collection
            (cl-naive-store.naive-indexed:hash document)))
      (let ((key-values
              (key-values collection document)))

        (cl-naive-store.naive-indexed::try-better-value-match
         collection
         (cl-naive-store.naive-indexed:index-lookup-values
          collection key-values)
         key-values))))

(defun sexp-to-naive-document (multiverse sexp)
  "This function converts a sexp to a naive-documents:document. What it does not do is
sync the info with any existing documents in the collection. You will
have to do a perstist-document (or add-document if not a persisted
collection) on this document to sync with any existing documents in
the collection.

If you intend to use add-document just note that it does not sync any
child documents only persist-document does that as well. So if you are
not carefull you can end up with more than one document with the same
UUID and different values for child documents.

The function does check for existing-documents to at least make sure
that the same UUID is used and any child documents that do not have UUID's
already are supplied with UUIDs. Child documents that are reference
documents are also checked for consitent UUID.

The conversion is done on the assumption you have not trashed the document
beyond repair when you possibly hacked the sexp.

"
  ;;Using dochash to not convert child docs that have already been
  ;;converted.  For example country-town in two seperate (physical
  ;;postal) address documents in addresses could be exactly the same
  ;;and are assumed the to be the same.
  (let ((dochash (make-hash-table :test #'equalp)))
    (labels ((parse-values (value)
               (if (listp value)
                   (if (member :hash value)
                       (or (gethash (getx value :hash) dochash)
                           (sexp-to-naive-document multiverse value))
                       (loop for val in value
                             collect (parse-values value)))
                   value)))

      (let* ((universe (and (getx sexp :collection)
                            (cl-naive-store.naive-core:get-multiverse-element
                             :universe multiverse (getx sexp :universe))))
             (store (and (getx sexp :collection)
                         universe
                         (cl-naive-store.naive-core:get-multiverse-element
                          :store universe (getx sexp :store))))
             (collection (and (getx sexp :collection)
                              store
                              (cl-naive-store.naive-core:get-multiverse-element
                               :collection store (getx sexp :collection))))
             (new-doc (apply 'cl-naive-store.naive-documents:make-document
                             `(:hash ,(getx sexp :hash)
                               :store ,store
                               :collection ,collection
                               :deleted-p ,(getx sexp :deleted-p)
                               :elements ,(loop for (key value) on (getx sexp :elements)
                                                by #'cddr
                                                collect key
                                                collect (parse-values value)))))
             (existing-document (and (getx sexp :collection)
                                     collection
                                     (existing-document%
                                      collection
                                      new-doc))))

        (unless (cl-naive-store.naive-indexed:hash new-doc)
          (if existing-document
              (setf (cl-naive-store.naive-indexed:hash new-doc)
                    (cl-naive-store.naive-indexed:hash existing-document))
              (setf (cl-naive-store.naive-indexed:hash new-doc)
                    (uuid:make-v4-uuid))))

        (setf (gethash (cl-naive-store.naive-indexed:hash new-doc) dochash) new-doc)

        new-doc))))

;;;;TODO: Everything below should be moved some where else!

;;TODO: Use some ptrees function
(defun get-document-type-names (doc-type-defs)
  (mapcar (lambda (doc-type-def)
            (getf (getf doc-type-def :document-type) :name))
          doc-type-defs))

;;TODO: Use some ptrees function
(defun get-collection-names (collection-defs)
  (mapcar (lambda (collection-def)
            (getf (getf collection-def :collection) :name))
          collection-defs))

(defun get-referenced-types (collection-defs known-doc-types)
  (cl-naive-ptrees:query collection-defs
                         `(chain
                           (((lambda (node)
                               (when (listp node)
                                 (find (getf node :document-type) ',known-doc-types
                                       :test #'string-equal)))
                             :collection)))))

(defun get-collections-dependencies (doc-type-defs known-coll-names)
  (cl-naive-ptrees:query
   doc-type-defs
   `(chain
     (((lambda (node)
         (when (listp node)
           (when (equalp (getf node :type) :document)
             (find (getf (getf node :spec) :type)
                   ',known-coll-names
                   :test #'string-equal))))
       :concrete-type)))))

(defun depth-first-search (node adjacency-list visited ordering)
  (push node visited)
  (dolist (neighbor (gethash node adjacency-list))
    (unless (member neighbor visited)
      (depth-first-search neighbor adjacency-list visited ordering)))
  (push node ordering))

(defun topological-sort (nodes adjacency-list)
  (let ((visited '())
        (ordering '()))
    (dolist (node nodes)
      (unless (member node visited)
        (setf ordering
              (depth-first-search node adjacency-list visited ordering))))
    (nreverse ordering)))

(defun build-adjacency-list (nodes edges)
  (let ((adjacency-list (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (setf (gethash node adjacency-list) '()))
    (dolist (edge edges)
      (let ((from (first edge))
            (to (second edge)))
        (push to (gethash from adjacency-list))))
    adjacency-list))

(defun get-topological-order (nodes edges)
  (let ((adjacency-list (build-adjacency-list nodes edges)))
    (topological-sort nodes adjacency-list)))

(defgeneric create-multiverse-element-from-definition (parent definition)
  (:documentation "Creates a multiverse element from the supplied definition and adds"))

(defun create-document-types (store document-type-definitions known-doc-types
                              &key (document-type-class
                                    'cl-naive-store.document-types:document-type)
                              persist-p)
  "Create document types in the specified order of dependencies and add them to the store."
  (let* ((doc-type-defs
           (mapcar (lambda (doc-type-def)
                     (cons  (getf (getf doc-type-def :document-type) :name)
                            doc-type-def))
                   document-type-definitions))
         (referenced-types (get-referenced-types
                            document-type-definitions
                            known-doc-types))
         (sorted-doc-type-names (get-topological-order
                                 (mapcar #'car doc-type-defs)
                                 referenced-types)))

    (dolist (doc-type-name sorted-doc-type-names)
      (let* ((doc-type-def (getf
                            (cdr (assoc doc-type-name doc-type-defs))
                            :document-type))
             (doc-type (make-instance (or
                                       (getx doc-type-def :class)
                                       document-type-class
                                       'cl-naive-store.document-types:document-type)
                                      :store store
                                      :name (getf doc-type-def :name)
                                      :label (getf doc-type-def :label)
                                      :elements '())))

        (dolist (element-def (getf doc-type-def :elements))
          (let ((element (make-instance (or
                                         (getx element-def :class)
                                         (getx doc-type-def :element-class)
                                         'cl-naive-store.document-types:element)
                                        :name (getf element-def :name)
                                        :key-p (getf element-def :key-p)
                                        :concrete-type (getf element-def :concrete-type)
                                        :attributes (getf element-def :attributes))))

            (push element (cl-naive-store.document-types:elements doc-type))))

        (cl-naive-store.naive-core:add-multiverse-element
         store doc-type :persist-p persist-p)))))

(defun create-collections (store
                           collection-definitions
                           document-types
                           known-coll-names
                           &key (collection-class
                                 'cl-naive-store.naive-documents:document-collection)
                           persist-p)
  "Create collections in the specified order of dependencies and add them to the store."

  (let* ((referenced-collections (get-collections-dependencies
                                  document-types
                                  known-coll-names))
         ;; Topological sorting of collection names
         (sorted-collection-names (get-topological-order
                                   (get-collection-names collection-definitions)
                                   referenced-collections)))

    (dolist (collection-name sorted-collection-names)
      (let* ((collection-def (find-named-element
                              :collection
                              collection-name
                              collection-definitions))
             (collection-definition (getf collection-def :collection)))

        (cl-naive-store.naive-core:add-multiverse-element
         store
         (make-instance
          ;;because there is a high likelyhood that nil was
          ;;passed for collection-class which would negate
          ;;default value doubling up with or
          (or
           (getx collection-definition :class)
           collection-class
           'cl-naive-store.naive-documents:document-collection)
          :store store
          :name (getf collection-definition :name)
          :location (merge-pathnames
                     (make-pathname
                      :directory
                      (list :relative
                            (getf collection-definition :name)))
                     (namestring
                      (pathname (cl-naive-store.naive-core:location
                                 store))))
          :document-type
          (cl-naive-store.document-types:get-document-type
           store
           (getf collection-definition :data-type)))
         :persist-p persist-p)))))

(defun traverse-apply (criteria plist-tree func)
  ""
  (dolist (plist (cl-naive-ptrees:query plist-tree criteria))
    (funcall func plist)))

(defun create-universe (multiverse universe-definition)
  (setf universe-definition (getx universe-definition :universe))
  (or
   (cl-naive-store.naive-core:get-multiverse-element :universe
                                                     multiverse
                                                     (getf universe-definition :name))
   (make-instance (or
                   (getx universe-definition :universe-class)
                   (cl-naive-store.naive-core:universe-class
                    multiverse))
                  :store-class (getx universe-definition :store-class)
                  :location
                  (merge-pathnames
                   (make-pathname
                    :directory
                    (list :relative
                          (getx universe-definition :name)))
                   (cl-naive-store.naive-core:location
                    multiverse)))))

(defun make-multiverse (multiverse-definition)
  (make-instance 'cl-naive-store.naive-core:multiverse
                 :name (digx multiverse-definition :multiverse :name)
                 :location (digx multiverse-definition :multiverse :location)
                 :universe-class
                 (or (digx multiverse-definition :multiverse :universe-class)
                     'cl-naive-store.naive-core:universe)))

(defun create-store (universe store-definition)
  (when (equal (car store-definition) :store)
    (setf store-definition (getx store-definition :store)))
  (or
   (cl-naive-store.naive-core:get-multiverse-element :store universe
                                                     (getx store-definition :name))

   (cl-naive-store.naive-core:add-multiverse-element
    universe
    (make-instance (or (getx store-definition :store-class)
                       (cl-naive-store.naive-core:store-class universe))
                   :universe universe
                   :name (getx store-definition :name)
                   :collection-class (or
                                      (getx store-definition :collection-class)
                                      (getx store-definition :collection-class))
                   :location
                   (merge-pathnames
                    (make-pathname
                     :directory
                     (list :relative
                           (getx store-definition :name)))
                    ;;TODO: Check for trailing / we need it
                    (namestring
                     (cl-naive-store.naive-core:location
                      universe))))
    :persist-p t)))

(defun create-stores (universe universe-definition &key persist-p)
  ;;Using traverse-apply instead of simple dolist so that we can
  ;;accomodate at least a small degree of flexibility in the
  ;;structure of the universe definition
  (traverse-apply
   '(chain
     ((:stores))
     ((:store)))
   universe-definition
   (lambda (store-definition)
     (setf store-definition (getx store-definition :store))

     (let* ((store (create-store universe store-definition)))
       (let* ((known-doc-types (get-document-type-names
                                (getx store-definition :document-types)))
              (known-coll-names (get-collection-names
                                 (getx store-definition :collections))))

         (create-document-types store
                                (getx store-definition :document-types)
                                known-doc-types
                                :document-type-class
                                (or
                                 (getx store-definition :document-type-class)
                                 (getx universe-definition :document-type-class))
                                :persist-p persist-p)

         (create-collections
          store
          (getx store-definition :collections)
          (getx store-definition :document-types)
          known-coll-names
          :collection-class (or (getx store-definition :collection-class)
                                (getx universe-definition :collection-class))
          :persist-p persist-p)

         (cl-naive-store.naive-core:add-multiverse-element universe store :persist-p t)

         (when persist-p
           (ensure-directories-exist (cl-naive-store.naive-core:location store))
           (cl-naive-store.naive-core:persist store)))))))

(defun create-multiverse (multiverse-definition &optional persist-p multiverse)
  (unless multiverse
    (setf multiverse
          (make-multiverse multiverse-definition))

    ;;Using traverse-apply instead of simple dolist so that we can
    ;;accomodate at least a small degree of flexibility in the
    ;;structure of the definition
    (traverse-apply
     '(chain
       ((:multiverse))
       ((:universe)))
     multiverse-definition
     (lambda (universe-definition)
       (let* ((universe (create-universe multiverse universe-definition)))
         (cl-naive-store.naive-core:add-multiverse-element multiverse universe
                                                           :persist-p t)
         (when persist-p
           (ensure-directories-exist (cl-naive-store.naive-core:location universe)))
         (create-stores universe universe-definition :persist-p persist-p)))))
  multiverse)

;;TODO: Most of these (badly named) utility functions has been
;;superceded by get/add/remove-multiverse-elements and/or
;;instance-from-definition for the different multiverse classes.

;;The collection one is a bit different to the add-multiverse-element
;;as it tries to load the document type as well. Should we ammend
;;add-multiverse-element to have similar behaviour?

;;TODO: Depricated
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
                (add-multiverse-element
                 store
                 (make-instance 'cl-naive-store.naive-documents:document-collection
                                :name collection-name
                                :document-type
                                (cl-naive-store.naive-core:add-multiverse-element
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
                  (cl-naive-store.naive-core:add-multiverse-element
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
                         (add-multiverse-element
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

