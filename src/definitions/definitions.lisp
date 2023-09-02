(in-package :cl-naive-store.definitions)

(defun query-chain (definition criteria)
  (let ((results))
    (labels ((query* (definition criteria)
               (let ((result (cl-naive-ptrees:query definition (list (car criteria)))))
                 (when result

                   (if (cdr criteria)
                       (dolist (huh result)
                         (query* huh (cdr criteria)))
                       (push result results))))))

      (query* definition criteria)

      (nreverse (car results)))))

;;; These definition manipulation functions are for convenience sake
;;; but are simple in the sense that they will not ensure the
;;; integrity of the final definition. For example if you remove a
;;; data-type that data type might still be used by a collection which
;;; will cause errors when trying to use the definition.

(defgeneric query-definition (definition &key fn element)
  (:documentation "Queries the definition passed for an element or elements.

If an element is supplied limits calling the function to those elements or if no function is supplied just fetches elements of keyword element.

If you need more control use cl-naive-ptrees directly."))

(defmethod query-definition (definition &key fn element)
  (if fn
      (if element
          (cl-naive-ptrees::query definition
                                  `(((lambda (node)
                                       (when (listp node)
                                         (funcall fn node)))
                                     ,element)))
          (cl-naive-ptrees::map-plists definition
                                       fn))
      (if element
          (cl-naive-ptrees::query definition
                                  `((,element)))
          definition)))

(defgeneric get-definition-element (element-type parent name)
  (:documentation "Fetches a definition of the type element-type by name from the parent definition."))

;;TODO: depricated
(defgeneric find-named-elements (element name definition &key fn)
  (:documentation
   "Returns all the specific named definition-elements found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-elements (element name definition &key fn)
  (cl-naive-ptrees::query definition
                          (if fn
                              `(((lambda (node)
                                   (when (listp node)
                                     (and
                                      (equalp (getf node :name) ,name)
                                      (funcall fn node))))
                                 ,element))
                              `(((lambda (node)
                                   (when (listp node)
                                     (equalp (getf node :name) ,name)))
                                 ,element)))))

(defgeneric find-named-element (element name definition)
  (:documentation
   "Returns the first specific named definition-element found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-element (element name definition)
  (car (find-named-elements element name definition)))

(defgeneric add-definition-element (element-type definition element &key name-path)
  (:documentation "Adds a definition element to the parent definition."))

(defun build-name-path-chain (name-path)
  `,(mapcar (lambda (path-element)
              `((lambda (node)
                  (when (listp node)
                    (when  (equalp (getf node :name) ,(second path-element))
                      node)))
                ,(first path-element)))
            name-path))

(defmacro add-defintion-element* (element-type list-key definition element name-path
                                  replace-p)
  (let ((element-type% (gensym))
        (list-key% (gensym))
        (definition% (gensym))
        (element% (gensym))
        (name-path% (gensym))
        (replace-p% (gensym)))

    `(let ((,element-type% ,element-type)
           (,list-key% ,list-key)
           (,definition% ,definition)
           (,element% ,element)
           (,name-path% ,name-path)
           (,replace-p% ,replace-p))

       (let* ((elements (or (and ,name-path%
                                 (getx
                                  (second
                                   (car
                                    (query-chain
                                     ,definition%
                                     (cl-naive-store.definitions::build-name-path-chain
                                      ,name-path%))))
                                  ,list-key%))
                            (getx ,definition% ,list-key%)))
              (existing-element (find-named-element ,element-type%
                                                    (digx ,element% ,element-type% :name)
                                                    elements)))

         (unless elements
           (error (format nil "~A not found in definition."
                          (string-capitalize ,list-key%))))

         (if existing-element
             (if (not ,replace-p%)
                 (error "Collection definition already exsists: ~A"
                        (digx ,element% :collection :name))
                 (setf elements (nsubstitute ,element% existing-element elements)))
             (nconc elements (list ,element)))
         ,definition%))))

(defmethod add-definition-element ((element-type (eql :collection))
                                   definition
                                   collection
                                   &key name-path replace-p)
  "Adds a collection to :collections."

  (add-defintion-element* :collection
                          :collections
                          definition
                          collection
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :document-type))
                                   definition
                                   document-type
                                   &key name-path replace-p)
  "Adds a document-type to :document-types."

  (add-defintion-element* :document-type
                          :document-types
                          definition
                          document-type
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :store))
                                   definition
                                   store
                                   &key name-path replace-p)
  "Adds a store to :stores."

  (add-defintion-element* :store
                          :stores
                          definition
                          store
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :universe))
                                   definition
                                   universe
                                   &key name-path replace-p)
  "Adds a universe to :universes."

  (add-defintion-element* :universe
                          :universes
                          definition
                          universe
                          name-path
                          replace-p))

(defgeneric remove-definition-element (element-type  definition element-name
                                       &key name-path)
  (:documentation "Removes a definition element from the definition."))

(defmacro remove-defintion-element* (element-type list-key definition name name-path)
  (let ((definition% (gensym))
        (name% (gensym))
        (name-path% (gensym))
        (element-type% (gensym))
        (list-key% (gensym)))

    `(let* ((,definition% ,definition)
            (,name% ,name)
            (,name-path% ,name-path)
            (,element-type% ,element-type)
            (,list-key% ,list-key))

       (let* ((path-element (second (car
                                     (query-chain
                                      ,definition%
                                      (build-name-path-chain ,name-path%)))))
              (elements (or (and ,name-path%
                                 path-element
                                 (getx
                                  path-element
                                  ,list-key%))
                            (getx path-element ,list-key%)))
              (existing-element (find-named-element ,element-type%
                                                    ,name%
                                                    elements)))

         (unless elements
           (error (format nil "~A not found in definition."
                          (string-capitalize ,list-key%))))

         (when existing-element
           (setf (getx (or path-element ,definition%) ,list-key%)
                 (remove existing-element elements)))

         ,definition%))))

(defmethod remove-definition-element ((element-type (eql :universe))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :universe
   :universes
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :store))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :store
   :stores
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :collection))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :collection
   :collections
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :document-type))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :document-type
   :document-types
   definition
   name
   name-path))

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

(defun make-universe (multiverse universe-definition)
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

(defun make-store (universe store-definition)
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

     (let* ((store (make-store universe store-definition)))
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
    ;;structure of the universe definition
    (traverse-apply
     '(chain
       ((:multiverse))
       ((:universe)))
     multiverse-definition
     (lambda (universe-definition)
       (let* ((universe (make-universe multiverse universe-definition)))
         (cl-naive-store.naive-core:add-multiverse-element multiverse universe
                                                           :persist-p t)
         (when persist-p
           (ensure-directories-exist (cl-naive-store.naive-core:location universe)))
         (create-stores universe universe-definition :persist-p persist-p)))))
  multiverse)
