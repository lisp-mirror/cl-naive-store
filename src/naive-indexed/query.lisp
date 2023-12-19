(in-package :cl-naive-store.naive-indexed)

(defun indexed-values (collection index-values shards)
  (let ((data))
    (dolist (index index-values)
      (let ((documents (index-lookup-values collection index :shards shards)))
        (when documents
          (setf data (append documents data)))))
    data))

(defmethod naive-reduce ((collection indexed-collection-mixin)
                         &key index-values query function initial-value
                         shards)
  "Extends naive-reduce to be able to take advantage of indexing. Reduce is done on values retrieved by the supplier index."
  (let ((indexed-values (indexed-values collection index-values shards)))

    (if indexed-values
        (naive-reduce indexed-values
                      :query query
                      :function function
                      :initial-value initial-value)
        (naive-reduce indexed-values
                      :query query
                      :function function
                      :initial-value initial-value
                      :shards (if shards
                                  shards
                                  (shards collection))))))

(defvar *index-query-lock* (bt:make-lock))

(defmethod query-data ((collection indexed-collection-mixin)
                       &key index-values query shards &allow-other-keys)
  "Extends query-data to be able to take advantage of indexing. Query is done on values retrieved by the supplied index."
  (let ((indexed-values (indexed-values collection index-values shards)))

    (if indexed-values

        (progn
          (query-data indexed-values
                      :query query))
        (let ((%result% nil))
          (do-sequence (shard (if shards
                                  shards
                                  (shards collection))
                        :parallel-p t)

            (let ((result (query-data (documents shard)
                                      :query query)))
              (bt:with-recursive-lock-held (*index-query-lock*)
                (setf %result% (append result %result%)))))
          %result%))))

