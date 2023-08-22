(in-package :cl-naive-store.definitions)

;;; These definition manipulation functions are for convenience sake
;;; but are simple in the sense that they will not ensure the
;;; integrity of the final definition. For example if you remove a
;;; data-type that data type might still be used by a collection which
;;; will cause errors when trying to use the definition.

(defun find-collection-definition (collection-name
                                   definition)
  "Finds a collection definition in a store definition."
  (car (cl-naive-ptrees::query definition
                               `((((lambda (node)
                                     (when (listp node)
                                       (equalp (getf node :name) ,collection-name)))
                                   :collection))))))

(defun find-document-type-definition (document-type-name
                                      definition)
  "Finds a collection definition in a store definition."
  (car (cl-naive-ptrees::query definition
                               `((((lambda (node)
                                     (when (listp node)
                                       (equalp (getf node :name) ,document-type-name)))
                                   :document-type))))))

(defun add-collection-definition (universe-name store-name collection-definition
                                  multiverse-definition)
  "Adds a collection definition to a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))

        (when (string-equal store-name (getx (getx store :store) :name))
          (if (find-collection-definition
               (getx (getx collection-definition :collection) :name)
               store)
              (error "Collection definition already exsists: ~A"
                     (getx (getx collection-definition :collection) :name))
              (progn
                (setf (getx (getx store :store) :collections)
                      (append (getx (getx store :store) :collections)
                              collection-definition))
                (return-from add-collection-definition multiverse-definition))))))))

(defun remove-collection-definition (universe-name store-name collection-name
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
              (return-from remove-collection-definition multiverse-definition))))))))

(defun add-document-type-definition (universe-name store-name type-definition
                                     multiverse-definition)
  "Adds a document-type definition to a store in a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getf (getf store :store) :name))
          (pushnew type-definition (getx store :document-types))
          (return-from add-document-type-definition multiverse-definition))))))

(defun remove-document-type-definition (universe-name store-name type-name
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
              (return-from remove-document-type-definition multiverse-definition))))))))

(defun add-store-definition (universe-name store-definition
                             multiverse-definition)
  "Adds a store definition to a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (pushnew store-definition (cl-getx:digx universe :universe :stores))

      (return-from add-store-definition multiverse-definition))))

(defun remove-store-definition (universe-name store-name
                                multiverse-definition)
  "Removes a store definition from a universe in the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))
    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))
      (dolist (store (cl-getx:digx universe :universe :stores))
        (when (string-equal store-name (getf (getf store :store) :name))
          (setf (cl-getx:digx universe :universe :stores)
                (remove store (cl-getx:digx universe :universe :stores)))))

      (return-from remove-store-definition multiverse-definition))))

(defun add-universe-definition (universe-definition
                                multiverse-definition)
  "Adds a universe definition to the multiverse definition."
  (pushnew universe-definition (getx multiverse-definition :multiverse))
  multiverse-definition)

(defun remove-universe-definition (universe-name multiverse-definition)
  "Removes a universe definition from the multiverse definition."
  (dolist (universe (getx multiverse-definition :multiverse))

    (when (string-equal universe-name
                        (cl-getx:digx universe :universe :name))

      (setf (getx multiverse-definition :multiverse)
            (remove universe (getx multiverse-definition :multiverse)))
      (return-from remove-universe-definition multiverse-definition))))

(defun get-document-types (doc-type-defs)
  (mapcar (lambda (doc-type-def)
            (getf (getf doc-type-def :document-type) :name))
          doc-type-defs))

(defun get-collection-names (collection-defs)
  (mapcar (lambda (collection-def)
            (getf (getf collection-def :collection) :name))
          collection-defs))

(defun get-referenced-types (collection-defs known-doc-types)
  (cl-naive-ptrees:query collection-defs
                         `(chain
                           (((lambda (node)
                               (when (listp node)
                                 (find (getf element :name) ',known-doc-types
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
                              &key persist-p)
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
             (doc-type (make-instance 'document-type
                                      :store store
                                      :name (getf doc-type-def :name)
                                      :label (getf doc-type-def :label)
                                      :elements '())))

        (dolist (element-def (getf doc-type-def :elements))
          (let ((element (make-instance 'element
                                        :name (getf element-def :name)
                                        :key-p (getf element-def :key-p)
                                        :concrete-type (getf element-def :concrete-type)
                                        :attributes (getf element-def :attributes))))

            (push element (elements doc-type))))

        (add-document-type store doc-type :persist-p persist-p)))))

(defun create-collections (store
                           collection-definitions
                           document-types
                           known-coll-names
                           &key (collection-class
                                 'cl-naive-store.docmuent-collection)
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

      (let* ((collection-def (find collection-name collection-definitions
                                   :key #'(lambda (def)
                                            (getf (getf def :collection) :name))
                                   :test #'string-equal))
             (collection-definition (getf collection-def :collection))
             (collection
               (add-collection store
                               (make-instance
                                collection-class
                                :store store
                                :name (getf collection-definition :name)
                                :location (merge-pathnames
                                           (make-pathname
                                            :directory
                                            (list :relative
                                                  (getf collection-definition :name)))
                                           (format nil "~A"
                                                   (pathname (location store))))
                                :document-type
                                (get-document-type
                                 store
                                 (getf collection-definition :data-type))))))
        (when persist-p
          (persist-collection-def collection))))))

(defun create-multiverse (multiverse &optional persist-p)
  (let ((universes '()))
    (dolist (universe-def (getf multiverse :multiverse))

      (let* ((universe-definition (getf universe-def :universe))
             (universe
               (make-instance (getf universe-definition :universe-class)
                              :store-class (getf universe-definition :store-class)
                              :location
                              (merge-pathnames
                               (make-pathname
                                :directory
                                (list :relative (getf universe-definition :name)))
                               (getf universe-definition :location)))))
        (when persist-p
          (ensure-directories-exist (location universe)))

        (dolist (store-def (getf universe-definition :stores))
          (let* ((store-definition (getf store-def :store))
                 (store
                   (add-store universe
                              (make-instance (getf universe-definition :store-class)
                                             :universe universe
                                             :name (getf store-definition :name)
                                             :collection-class
                                             (getf universe-definition :collection-class)
                                             :location
                                             (merge-pathnames
                                              (make-pathname
                                               :directory
                                               (list :relative
                                                     (getf store-definition :name)))
                                              (format nil "~A/"
                                                      (location universe)))))))
            (when persist-p
              (ensure-directories-exist (location store))
              (persist store))

            ;; Get the known document types and collection names first
            (let* ((known-doc-types (get-document-types
                                     (getf store-definition :document-types)))
                   (known-coll-names (get-collection-names
                                      (getf store-definition :collections))))

              ;; Then pass them to the creation functions
              (create-document-types store
                                     (getf store-definition :document-types)
                                     known-doc-types
                                     :persist-p persist-p)
              (create-collections
               store
               (getf store-definition :collections)
               (getf store-definition :document-types)
               known-coll-names
               :collection-class (getf universe-definition :collection-class)
               :persist-p persist-p))))
        (push universe universes)))
    (nreverse universes)))
