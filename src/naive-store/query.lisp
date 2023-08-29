(in-package :cl-naive-store.naive-core)

;;;; Query and naive-reduce exists to hide the structure/internals of the
;;;; collection from the user, so its for convenience all the functionality
;;;; here could be done with plain map and reduce cl functions.

;;;;TODO: Consider integrating/extending query-* for queries accross http aka naive-api's

(defgeneric naive-reduce (collection &key query function initial-value &allow-other-keys)
  (:documentation "Uses query to select data documents from a collection and applies the function to
those documents returning the result.

NOTES:

Does lazy loading."))

(defmethod naive-reduce :before ((collection collection) &key shards &allow-other-keys)
  "Lazy loading data."
  (if shards
      (cl-naive-store.naive-core::load-shards collection shards)
      (load-data collection)))

(defvar *query-lock* (bt:make-lock))

(defmethod naive-reduce ((collection collection) &key query function initial-value shards
                         &allow-other-keys)
  (let ((%result% nil))

    (do-sequence (shard (or shards
                            (shards collection))
                  :parallel-p t)

      (let ((result (naive-reduce (documents shard)
                                  :query query
                                  :function function
                                  :initial-value initial-value)))
        (bt:with-recursive-lock-held (*query-lock*)

          (setf %result%
                (concatenate 'list %result%
                             result)))))
    %result%))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((hash-table hash-table) &key query function initial-value
                         &allow-other-keys)
  (let ((result initial-value))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (if query
           (if (funcall query document)
               (if function
                   (setf result (funcall function result document))
                   (push document result)))
           (if function
               (setf result (funcall function result document))
               (push document result))))
     hash-table)
    result))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((sequence sequence) &key query function initial-value  &allow-other-keys)
  (reduce #'(lambda (result document)
              (if query
                  (if (apply query (list document))
                      (if function
                          (funcall function result document)
                          (push document result))
                      result)
                  (if function
                      (funcall function result document)
                      (push document result))))
          sequence
          :initial-value initial-value))

(defgeneric query-data (collection &key query &allow-other-keys)
  (:documentation "Returns the data that satisfies the query.

NOTES:

Does lazy loading."))

(defmethod query-data :before ((collection collection) &key shards &allow-other-keys)
  "Lazy loading data."
  (if shards
      (progn ; only load the specified shards.
        (naive-impl::debug-log "query-data -shards" :file-p t)
        (cl-naive-store.naive-core::load-shards collection shards))
      (progn
        (naive-impl::debug-log "query-data -collection" :file-p t)
        (load-data collection))))

(defmethod query-data ((collection collection) &key query shards &allow-other-keys)
  (let ((%result% nil))
    (do-sequence (shard (or shards (shards collection)) :parallel-p t)
      (let ((result (query-data (documents shard) :query query)))
        (bt:with-recursive-lock-held (*query-lock*)
          (setf %result% (append result %result%)))))
    %result%))

(defmethod query-data ((store store) &key collection-name query shards &allow-other-keys)
  (let ((collection (get-multiverse-element :collection store collection-name)))

    (unless collection
      (setf collection (get-collection-from-def
                        store
                        collection-name))
      (when collection
        (add-collection store collection))
      (unless collection
        (error "Could not find or create collection ~A" collection-name)))

    (query-data collection :query query :shards shards)))

(defgeneric query-document (collection &key query &allow-other-keys)
  (:documentation "Returns the first last document found, and any others that satisfies the query

NOTES:

Does lazy loading."))

(defmethod query-document :before ((collection collection) &key shards &allow-other-keys)
  "Lazy loading data."
  (if shards
      (cl-naive-store.naive-core::load-shards collection shards)
      (load-data collection)))

(defmethod query-document ((collection collection) &key query shards &allow-other-keys)
  (let ((documents (query-data collection :query query :shards shards)))
    (values (first documents) (rest documents))))

(defmethod query-document ((store store) &key collection-name query &allow-other-keys)
  (let ((documents (query-data store :collection-name collection-name :query query)))
    (values (first documents) (rest documents))))

;; Don't use map-append, it falls apart at 10 mil records.
;; Note: we use reduce to build explicitely a list only of the queried documents.
;; With remove-if-not, we'd have to coerce the whole sequence to a list first.
(defmethod query-data ((sequence sequence) &key query &allow-other-keys)
  (if query
      (reduce (lambda (results document)
                (cond
                  ((deleted-p document)     results)
                  ((funcall query document) (cons document results))
                  (t                        results)))
              sequence :initial-value '())
      (coerce sequence 'list)))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-data ((hash-table hash-table) &key query &allow-other-keys)
  (let ((documents))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (unless (deleted-p document)
         (if query
             (when (funcall query document)
               (push document documents))
             (push document documents))))
     hash-table)
    documents))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-document ((list list) &key query &allow-other-keys)
  (let ((documents (query-data list :query query)))
    (values (first documents) (rest documents))))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-document ((hash-table hash-table) &key query &allow-other-keys)
  (let ((documents))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (when (funcall query document)
         (push document documents)))
     hash-table)
    (values (first documents) (rest documents))))
