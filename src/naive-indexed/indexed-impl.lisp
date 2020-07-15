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

(defgeneric push-value-index (collection index-values document &key &allow-other-keys)
  (:documentation "Uses lists within the key-value-index hash-table to store/group documents that match a key value combination. 

On updates of documents could end up with duplicate documents returned by the index lookup. The speed more than makes up for the occactional duplicate for now!

TODO: Implement index-lookup-value that strips out duplicates??"))

(defmethod push-value-index (collection index-values document &key &allow-other-keys)
  (unless (key-value-index collection)
    (setf (key-value-index collection) (make-hash-table :test 'equalp)))

  (push document (gethash (cl-murmurhash:murmurhash index-values)
			      (key-value-index collection))))

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

(defun remove-partial-value-index (collection index-values document)
  (let ((compounded)
	  (compounded-count 1))
    (dolist (pair index-values)      
      (push pair compounded)
      (when (> compounded-count 1)
	(remove-value-index collection (reverse compounded) document))
      (remove-value-index collection pair document)
      (incf compounded-count))))

(defun remove-index-values (collection indexes-values document)
  (dolist (index-values indexes-values)
    (if *do-partial-indexing*
	(remove-partial-value-index collection index-values document)
	(remove-value-index collection index-values document))))

(defgeneric remove-value-index (collection index-values document &key &allow-other-keys)
  (:documentation "Removes a value index."))

(defmethod remove-value-index (collection index-values document &key &allow-other-keys)
  (when (key-value-index collection)
    (remove document (gethash (cl-murmurhash:murmurhash index-values)
			  (key-value-index collection)))))

(defmethod remove-value-index ((collection indexed-values-hashtables-mixin)
			     index-values document &key &allow-other-keys)
  
  (let ((internal-hash (gethash (cl-murmurhash:murmurhash index-values)
				(key-value-index collection))))         
    (when internal-hash
      (remhash (hash document) internal-hash))))



