(in-package :indexed-impl)

(defmethod cl-murmurhash:murmurhash ((s uuid:uuid)
				     &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash::hash-string (frmt "~A" s) seed mix-only))

(defun hash-values (hash-table)
  (when hash-table
    (loop for value being the hash-values of hash-table collect value)))

(defgeneric index-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of index values from the values of a data document."))

(defmethod index-values (collection values &key &allow-other-keys)
  (let ((index-values))
    (dolist (index (indexes collection))
      (push
       (loop for (a b) on values by #'cddr
	  when (find a index :test 'equalp)
	  :collect (list a b))
       index-values))
    index-values))

(defgeneric push-value-index (collection index-values document &key shard &allow-other-keys)
  (:documentation "Uses lists within the key-value-index hash-table to store/group documents that match a key value combination. 

On updates of documents could end up with duplicate documents returned by the index lookup. The speed more than makes up for the occasional duplicate for now!

TODO: Implement index-lookup-value that strips out duplicates??"))

(defmethod push-value-index (collection index-values document &key shard &allow-other-keys)

  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))
  
  (unless (key-value-index shard)
    (setf (key-value-index shard) (make-hash-table :test 'equalp)))

  (push document (gethash-safe (cl-murmurhash:murmurhash index-values)
				 (key-value-index shard)
				 :lock (getx (lock shard) :values-index))
	  
	  ))

(defun populate-partial-value-index (collection index-values document)
  (let ((compounded)
	  (compounded-count 1))
    (dolist (pair index-values)      
      (push pair compounded)
      (when (> compounded-count 1)
	(push-value-index collection (reverse compounded) document))

      (push-value-index collection pair document)
      
      (incf compounded-count))))

(defun populate-value-index (collection indexes-values document)
  (dolist (index-values indexes-values)    
    (if *do-partial-indexing*
	(populate-partial-value-index collection index-values document)
	(push-value-index collection index-values document))))

(defun remove-partial-value-index (collection shard index-values document)
  (let ((compounded)
	  (compounded-count 1))
    (dolist (pair index-values)      
      (push pair compounded)
      (when (> compounded-count 1)
	(remove-value-index collection shard(reverse compounded) document))
      (remove-value-index collection shard pair document)
      (incf compounded-count))))

(defun remove-index-values (collection shard indexes-values document)
  (dolist (index-values indexes-values)
    (if *do-partial-indexing*
	(remove-partial-value-index collection shard index-values document)
	(remove-value-index collection shard index-values document))))

(defgeneric remove-value-index (collection shard index-values document &key &allow-other-keys)
  (:documentation "Removes a value index."))

(defmethod remove-value-index (collection shard index-values document &key &allow-other-keys)
  (when (and shard (key-value-index shard))
    (remove document (gethash-safe (cl-murmurhash:murmurhash index-values)
				     (key-value-index shard)
				     :lock (getx (lock shard) :values-index)))))





