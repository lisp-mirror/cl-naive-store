(in-package :cl-naive-store.definitions)

;;; These definition manipulation functions are for convenience sake
;;; but are simple in the sense that they will not ensure the
;;; integrity of the final definition. For example if you remove a
;;; data-type that data type might still be used by a collection which
;;; will cause errors when trying to use the definition.

(defgeneric find-named-elements (element name definition)
  (:documentation
   "Returns all the specific named definition-elements found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-elements (element name definition)
  (cl-naive-ptrees::query definition
                          `(((lambda (node)
                               (when (listp node)
                                 (equalp (getf node :name) ,name)))
                             ,element))))

(defgeneric find-named-element (element name definition)
  (:documentation
   "Returns the first specific named definition-element found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-element (element name definition)
  (car (find-named-elements element name definition)))

(defun add-collection (universe-name store-name collection-definition
                       multiverse-definition)
  "Adds a collection definition to a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))

        (when (string-equal store-name (getx (getx store :store) :name))
          (if (find-named-element
               :collection
               (getx (getx collection-definition :collection) :name)
               store)
              (error "Collection definition already exsists: ~A"
                     (getx (getx collection-definition :collection) :name))
              (progn
                (setf (getx (getx store :store) :collections)
                      (append (getx (getx store :store) :collections)
                              collection-definition))
                (return-from add-collection multiverse-definition))))))))

(defun remove-collection (universe-name store-name collection-name
                          multiverse-definition)
  "Removes a collection definition from a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getx (getx store :store) :name))

          (dolist (collection (getx (getx store :store) :collections))

            (when (string-equal (getx (getx collection :collection) :name)
                                collection-name)

              (setf (getx (getx store :store) :collections)
                    (remove collection (getx (getx store :store) :collections)))
              (return-from remove-collection multiverse-definition))))))))

(defun add-document-type (universe-name store-name type-definition
                          multiverse-definition)
  "Adds a document-type definition to a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getf (getf store :store) :name))
          (if (find-named-element
               :data-type
               (getx (getx type-definition :data-type) :name)
               store)
              (error "Data type definition already exsists: ~A"
                     (getx (getx type-definition :collection) :name))
              (pushnew type-definition (getx store :document-types)))
          (return-from add-document-type multiverse-definition))))))

(defun remove-document-type (universe-name store-name type-name
                             multiverse-definition)
  "Removes a document-type definition from a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getf (getf store :store) :name))
          (dolist (type-definition (getx (getf store :store) :document-types))
            (when (string-equal
                   (cl-getx:digx type-definition :document-type :name) type-name)
              (setf (getx store :document-types)
                    (remove type-definition (getx store :document-types)))
              (return-from remove-document-type multiverse-definition))))))))

(defun add-store (universe-name store-definition
                  multiverse-definition)
  "Adds a store definition to a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (pushnew store-definition (cl-getx:digx universe :universe :stores))

      (return-from add-store multiverse-definition))))

(defun remove-store (universe-name store-name
                     multiverse-definition)
  "Removes a store definition from a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getf (getf store :store) :name))
          (setf (cl-getx:digx universe :universe :stores)
                (remove store (cl-getx:digx universe :universe :stores)))))

      (return-from remove-store multiverse-definition))))

(defun add-universe (universe-definition
                     multiverse-definition)
  "Adds a universe definition to the multiverse definition."
  (pushnew universe-definition (getx multiverse-definition :multiverse))
  multiverse-definition)

(defun remove-universe (universe-name multiverse-definition)
  "Removes a universe definition from the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))

    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))

      (setf (getx multiverse-definition :multiverse)
            (remove universe (getx multiverse-definition :multiverse)))
      (return-from remove-universe multiverse-definition))))

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
                     (format nil "~A"
                             (pathname (cl-naive-store.naive-core:location
                                        store))))
          :document-type
          (cl-naive-store.document-types:get-document-type
           store
           (getf collection-definition :data-type)))
         :persist-p persist-p)))))

(defun traverse-apply (criteria plist-tree func)
  "Traverses the plist-tree and calls the attribute-func for each attribute pair that matches the criteria supplied.
The criteria selects plists in the plist-tree.  The ATTRIBUTE-FUNC is called one each attribute of each selected plist.

Note: all the attribute in the criteria are matched into a single plist, to select it.

An item in the criteria can be specific like (:att \"x\") which will match all attribute with name = :att and value = \"x\".
Or the item in the criteria can be non specific (:att) which will match all attributes with then name :att.
Specific and non specific criteria elements can be mixed like ((:att \"x\") (:other-att))
"
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
                    (format nil "~A/"
                            (cl-naive-store.naive-core:location
                             universe))))
    :persist-p t)))

(defun create-stores (universe universe-definition &key persist-p)
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
