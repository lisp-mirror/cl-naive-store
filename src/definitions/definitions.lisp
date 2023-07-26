(in-package :cl-naive-store.definitions)

(defun walk-data-filter (lst keywords &key filter-fn path)
  "Walks through the provided plist tree, applies a filter function to all elements that match the given keywords, and returns the results. Matching is done on partail keys i.e. gaps before, after and inbetween keywords in order are ok."
  (cond
    ((null lst) nil)
    ((keywordp (car lst))
     (let ((new-path (append path (list (car lst)))))

       (append
        (when (and filter-fn
                   (equalp (car lst) (first (last keywords)))
                   (sublist-in-order-p keywords new-path)
                   (funcall filter-fn lst))
          (list lst))
        (walk-data-filter (cdr lst) keywords :filter-fn filter-fn :path new-path))))
    ((consp (car lst))
     (append (walk-data-filter (car lst) keywords :filter-fn filter-fn :path path)
             (walk-data-filter (cdr lst) keywords :filter-fn filter-fn :path path)))
    (t (walk-data-filter (cdr lst) keywords :filter-fn filter-fn :path path))))

(defun get-document-types (doc-type-defs)
  (mapcar (lambda (doc-type-def)
            (getf (getf doc-type-def :document-type) :name))
          doc-type-defs))

(defun get-collection-names (collection-defs)
  (mapcar (lambda (collection-def)
            (getf collection-def :name))
          collection-defs))

(defun sublist-in-order-p (sublist list)
  (loop with state = sublist
        for item in list
        when (eq (first state) item)
        do (setq state (rest state))
        when (endp state)
        do (return t)
        finally (return nil)))

(defun get-referenced-types (doc-type-defs known-doc-types)
  (walk-data-filter doc-type-defs
                    '(:concrete-type :type)
                    :filter-fn
                    (lambda (element)
                      (find  (getf element :type) known-doc-types
                             :test #'string-equal))))

(defun get-collections-dependencies (collection-defs known-coll-names)
  (walk-data-filter collection-defs
                    '(:collection :document-type)
                    :filter-fn
                    (lambda (element)
                      (find  (getf element :name) known-coll-names
                             :test #'string-equal))))

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
                                            (getf def :name))
                                   :test #'string-equal))
             (collection
               (add-collection store
                               (make-instance
                                collection-class
                                :store store
                                :name (getf collection-def :name)
                                :location (merge-pathnames
                                           (make-pathname
                                            :directory
                                            (list :relative
                                                  (getf collection-def :name)))
                                           (format nil "~A"
                                                   (pathname (location store))))
                                :document-type
                                (get-document-type
                                 store
                                 (getf collection-def :data-type))))))
        (when persist-p
          (persist-collection-def collection))))))

(defun create-multiverse (universe-definitions &optional persist-p)
  (let ((universes '()))
    (dolist (universe-def universe-definitions)
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
          (let ((store
                  (add-store universe
                             (make-instance (getf universe-definition :store-class)
                                            :universe universe
                                            :name (getf store-def :name)
                                            :collection-class
                                            (getf universe-definition :collection-class)
                                            :location
                                            (merge-pathnames
                                             (make-pathname
                                              :directory
                                              (list :relative (getf store-def :name)))
                                             (format nil "~A/"
                                                     (location universe)))))))
            (when persist-p
              (ensure-directories-exist (location store))
              (persist store))

            ;; Get the known document types and collection names first
            (let* ((known-doc-types (get-document-types
                                     (getf store-def :document-types)))
                   (known-coll-names (get-collection-names
                                      (getf store-def :collections))))

              ;; Then pass them to the creation functions
              (create-document-types store
                                     (getf store-def :document-types)
                                     known-doc-types
                                     :persist-p persist-p)
              (create-collections
               store
               (getf store-def :collections)
               (getf store-def :document-types)
               known-coll-names
               :collection-class (getf universe-definition :collection-class)
               :persist-p persist-p))))
        (push universe universes)))
    (nreverse universes)))
