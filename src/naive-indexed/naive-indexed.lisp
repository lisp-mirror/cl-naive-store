(in-package :cl-naive-indexed)

;;TODO: Doing partial-indexing doubles the time it takes to load a database
;;Try to delay or spool of partial indexing on different thread.
(defparameter *do-partial-indexing* t
  "When this is set to t (which is the default), indexing is done for the individual elements of the indexes as well.")

;;TODO: Deal with shards????
(defclass indexed-shard (shard)
  ((hash-index :initarg :hash-index
	  :accessor hash-index
	  :initform nil
	  :documentation "Hash table keyed on document uuid for quick retrieval of an document.")
   (key-value-index :initarg :key-value-index
	  :accessor key-value-index
	  :initform nil
	  :documentation "Hash table keyed on document key values for quick retrieval of an document.
 Used when doing key value equality comparisons."))
  
  (:documentation "Extends shards with indexes."))




(defclass indexed-collection-mixin ()
  ((indexes :initarg :indexes
	    :accessor indexes
	    :initform nil
	    :documentation "List of index combinations. Also indexes members partially if *partial-indexing* is t, for example '((:emp-no :surname gender)) is indexed as (:emp-no :surname :gender), (:emp-no :surname), :emp-no, :surname and :gender"))
  (:documentation "Collection extension to add very basic indexes."))

(defmethod get-shard ((collection indexed-collection-mixin) shard-mac &key &allow-other-keys)
  (let ((shard (lparallel:pfind (or shard-mac (name collection)) (shards collection)
				:test 'equal :key 'mac)))

    (unless shard
      
      (setf shard (make-instance 'indexed-shard
				 :mac (or shard-mac (name collection))
				 :location
				 (cl-fad:merge-pathnames-as-file
				  (pathname (ensure-location collection))
				  (make-pathname ;;:directory (list :relative (name collection))
				   :name (or shard-mac (name collection))
				   :type "log"))
				 :key-value-index
				 #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized t)
				 #+(not (or sbcl ecl)) (make-hash-table :test 'equalp )
				 :hash-index
				 #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized t)
				 #+(not (or sbcl ecl)) (make-hash-table :test 'equalp )))
      
      (vector-push-extend shard (shards collection)))
    shard))

(defgeneric hash (document)
  (:documentation "Returns the hash identifier for a data document. Data documents need a hash identifier to work with naive-store-indexed. naive-store-indexed will edit the document to add a hash identifier when adding documents to a collection. naive-store-indexed uses a UUID in its default implementation."))

(defmethod hash (document)
  (if (getx document :hash)
      (frmt "~A" (getx document :hash))))

(defgeneric (setf hash) (value document))

(defmethod (setf hash) (value document)
  (setf (getx document :hash) (frmt "~A" value)))

(defgeneric index-lookup-values (collection values &key shards &allow-other-keys)
  (:documentation "Looks up document in key value hash index. If you are not using document-types then the order of values matter."))

(defmethod index-lookup-values :before (collection values &key shards &allow-other-keys)  
  (if shards
      (cl-naive-store::load-shards collection shards)
      (load-data collection)))

(defmethod index-lookup-values ((collection collection) values &key shards &allow-other-keys)
  (declare (ignorable shards))
  (warn "Not implemented!"))

(defmethod index-lookup-values ((collection indexed-collection-mixin)
				values &key shards &allow-other-keys)

  (let ((shard (lparallel:pfind (cl-murmurhash:murmurhash values)
				(or shards
				    (shards collection))
				:test (lambda (key index)					
					(gethash key index))
				:key #'key-value-index)))
    (when shard
      (remove-duplicates
	      (gethash (cl-murmurhash:murmurhash values)
		       (key-value-index shard))))))

(defgeneric index-lookup-hash (collection hash &key shards &allow-other-keys)
  (:documentation "Looks up document in UUID hash index. If sharsd is not supplied all loaded shards will be searched."))

(defmethod index-lookup-hash :before (collection hash &key shards &allow-other-keys)
  (if shards
      (cl-naive-store::load-shards collection shards)
      (load-data collection)))

;;TODO: Deal with shards. Loop through shard indexes.
(defmethod index-lookup-hash ((collection indexed-collection-mixin) hash &key shards &allow-other-keys)
  (when hash
    (let ((shard (lparallel:pfind hash (or shards
					   (shards collection))
				:test (lambda (key index)
					(gethash key index))
				:key #'hash-index)))
      (when shard
	(gethash (frmt "~A" hash)
		 (hash-index shard)))
    )
    #|
    (do-sequence (shard (or shards                            
			    (shards collection)))

      
	(let ((doc (gethash (frmt "~A" hash)
			    (hash-index shard))))
	  (when doc
	    (return-from index-lookup-hash doc))))
    
    
    |#
    ))

(defgeneric add-index (collection shard document &key &allow-other-keys)
  (:documentation "Adds a document to two indexes. The first uses a UUID that will stay with the document for its life time. The UUID is used when persisting the document and is never changed once created. This allows us to change key values without loosing the identify of the original document. 

The second is a key value hash index to be used when looking for duplicate documents during persist. If you are not using document-types the order of the keys in the plist matter. To make sure that you dont muck with the order of values/keys in your plists initialize all the possible value pairs with nil so that way the order is set."))

(defmethod add-index ((collection indexed-collection-mixin) shard document
		      &key key-values &allow-other-keys)
  (let* ((index-values (indexed-impl:index-values collection document))
	 (key-values (or key-values (key-values collection document))))

    (setf (gethash (hash document) (hash-index shard) ) document)
    
    ;;TODO: Check if this is still true???
    ;;Used to do document value comparisons to find index-document
    ;; (setf (gethash (frmt "~S" key-values) (key-value-index collection)) (list document))
    (indexed-impl::push-value-index collection key-values document)

    ;;key-values are in effect their own value index and because it could be completely
    ;;different from index-values it is added to value indexes as well.
    (indexed-impl::populate-value-index collection (list key-values) document)
    (indexed-impl::populate-value-index collection index-values document)))

(defgeneric remove-index (collection shard document &key &allow-other-keys)
  (:documentation "Removes a data document from the UUID and key value indexes."))


;;TODO: Deal with shards. Loop through shards to find index hash to remove
(defmethod remove-index ((collection indexed-collection-mixin)
			 shard document &key &allow-other-keys)
  (let ((key-values (key-values collection document))
	(indexes-values (indexed-impl:index-values collection document)))

    (indexed-impl:remove-value-index collection shard key-values document)
    (indexed-impl::remove-index-values collection shard (list key-values) document)
    (indexed-impl::remove-index-values collection shard indexes-values document)

    (remhash (hash document) (hash-index shard))))

(defmethod remove-document ((collection indexed-collection-mixin)
			    document &key shard &allow-other-keys)
  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))
  
  (remove-index collection shard document)
  (setf (documents shard) (remove document (documents shard))))

;;NOTE: Doing this because murmurhash is creating duplicates when you go beyond 10 million index values
(defun try-better-value-match (collection list key-values)
  (dolist (document list)
    (when (equalp key-values (key-values collection document))      
      (return document))))


(defmethod existing-document ((collection indexed-collection-mixin) shard document
			      &key key-values &allow-other-keys)
  (or
   (and (hash document)
	(index-lookup-hash collection (hash document) :shards (and shard (list shard))))
   (let ((key-values
	  (or key-values (key-values collection document))))
     
       (try-better-value-match
	 collection
	 (index-lookup-values collection key-values :shards (and shard (list shard)))
	 key-values))))

(defmethod add-document ((collection indexed-collection-mixin) document
			      &key shard (replace-existing-p t) (update-index-p t) &allow-other-keys)
  "Duplicates are not allowed for indexed collections!

If the document has no hash and a document with the same keys exists in the collection the supplied document's hash will be set to that of the existing document. The existing document will then be replaced with the supplied document. This is done to maintain hash consistancy of the store.

If you set replace-existing-p to nil then an existing document wont be replaced by the supplied document. Basically nothing will be done.

Indexes will be updated by default, if you want to stop index updates set update-index-p to nil. Just remember that if the document is really \"new\" to the collection the indexes will be updated in any case."
  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))

  (let* ((key-values (if (not (naive-impl:empty-p (hash document)))
			 (key-values collection document)))
	 (existing-document (existing-document collection shard document
					       :key-values key-values))
	 (action-taken))

    
    (cond (existing-document
	   (when (and (not (empty-p (hash document)))
		      (not (equalp (hash document) (hash existing-document))))

	     (naive-impl:write-log (location (universe (store collection)))
				   :error (list "A document with a different hash but a document with the same key values already exists.~% You are not allowed to clobber an existing object with a new hash because you are ~%violating hash consistency of the store. ~%~%~S~%~S" document existing-document)))
	   (when replace-existing-p
	     (setf (hash document) (hash existing-document))
	     (when update-index-p               
	       (add-index collection shard document) :key-values key-values)
             
	     (setf action-taken :replaced)))
	  ((not existing-document)           
	   (when (naive-impl:empty-p (hash document))
	     (setf (hash document) (uuid:make-v4-uuid)))
	   (setf action-taken :added)
	   (add-index collection shard document :key-values key-values)
	   (vector-push-extend document (documents shard))))
    
    ;;Add document to the collection
    (values
     document
     action-taken
     (if (equalp action-taken :replaced)
	 existing-document))))



