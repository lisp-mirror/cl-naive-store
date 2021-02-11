(in-package :cl-naive-store)

(defgeneric document-values (document)
  (:documentation "Returns a plist of document values.

NOTES:

Exists to ease the compatibility of various implementation functions. Basically it blurs the line between plists and more complex documents like cl-naive-documents document struct.

This helps keep the amount of specializations needed down considerably."))

;;TODO: Add a setf
(defmethod document-values (document)
  document)

(defgeneric key-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of key values from the values of a data document.
Checks the collection keys or uses hash."))

(defmethod key-values ((collection collection) values &key &allow-other-keys)
  (loop for (a b) on values by #'cddr
     when (member a (keys collection))
     do (return (list (list a b)))
     unless (or (equalp a :hash)
		(equalp a :deleted-p))
     :collect (list a b)))

(defgeneric existing-document (collection shard document  &key &allow-other-keys)
  (:documentation "Finds any documents with the same key values. This could return the exact same document or a similar document.

IMPL NOTES:

This is an essential part of loading and persisting documents, take care when implementing."))

(defmethod existing-document (collection shard document &key &allow-other-keys)
  (let ((position (position (key-values collection document) (documents shard)
			    :test (function equalp)
			    :key (lambda (document) (key-values collection document)))))
    (values (and position (elt (documents shard) position))
	    position)))


(defgeneric deleted-p (document)
  (:documentation "Indicates if a data document has been marked as deleted. 

naive-store writes data to file sequentially and when deleting data documents it does not remove a data document from the underlying file it just marks it as deleted."))

(defmethod deleted-p (document)
  (getx document :deleted-p))

(defgeneric (setf deleted-p) (value document &key &allow-other-keys))

(defmethod (setf deleted-p) (value document &key &allow-other-keys)
  (setf (getx document :deleted-p) value)
  document)

(defgeneric remove-document (collection document &key shard &allow-other-keys)
  (:documentation "Removes an document from the collection and its indexes. See add-document."))

(defmethod remove-document ((collection collection) document &key shard &allow-other-keys)
  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))

  #|
  (break "~A~%~A" (if (keys collection)
			  (key-values collection document)
			  document)
	 shard)
|#
  (setf (documents shard)
	(remove (if (keys collection)
		    (key-values collection document)
		    document)
		(documents shard)
		:test #'equalp :key (lambda (documentx)
				      (if (keys collection)
					  (key-values collection documentx)
					  documentx)))))

(defgeneric delete-document (collection document &key shard &allow-other-keys)
  (:documentation "Removes a document from the collection, marks the document as deleted and persists the deleted document to disk."))

(defmethod delete-document ((collection collection) document &key shard &allow-other-keys)
  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))
  
  (remove-document collection document :shard shard)
  (setf (deleted-p document) t)
  (persist-document collection document :delete-p t))

;;TODO: Deal with shards
(defgeneric add-document (collection document &key shard &allow-other-keys)
  (:documentation "Adds a document to the collection, it DOES NOT PERSIST the change, if you want adding with persistance use persist-document or persist the collection as a whole after you have done your adding.

Supply shard the document should belong to if you can, especially if you have a lot of documents to add to a specific shard. If not supplied the (shard-elements collection) will be used to calculate which shard to use for the document.
 
add-document returns multiple values:

The first returned value is the actual document supplied.
The second returned value indicates what action was taken ie. was it added newly or was an exiting document replaced.
The third returned value is the replaced document. 

NOTES:   

In general you should not be calling add-document directly, you should use persist-document. Calling add-document directly is allowed so you can create temporary collections that can be thrown away.

cl-naive-store does not have a update-document function, add-document does both and its behaviour can be complex depending on the key parameters supplied. Also the behaviour can differ for different types of collections. Check the appropriate collection documentation for more details."))


;;TODO: Deal with shards
(defmethod add-document ((collection collection) document
			    &key shard (handle-duplicates-p t) (replace-existing-p t) &allow-other-keys)
  "None of the following will have an effect if handle-duplicates = nil. 

If a document with the same keys exists in the collection the supplied the existing document will be replaced with the supplied document.

If you set replace-existing-p to nil then an existing document wont be replaced by the supplied document. Basically nothing will be done."
  (let ((existing-document%)
	(action-taken)
)
    (unless shard
      (setf shard (get-shard collection (document-shard-mac collection document))))

    
    (cond ((not handle-duplicates-p)
	   (bt:with-lock-held ((getx (lock shard) :docs))
	     (vector-push-extend document (documents shard)))
	   (setf action-taken :added-possible-duplicate))
	  (t
	   (if (keys collection)
	       (multiple-value-bind (existing-document position)
		   (existing-document collection shard document)
		 (setf existing-document% existing-document)
		 (if (and position replace-existing-p)
		     (progn
		       (bt:with-lock-held ((getx (lock shard) :docs))
			 (setf (elt (documents shard) position) document))
		       (setf action-taken :replaced))
		     (progn
		       (bt:with-lock-held ((getx (lock shard) :docs))
			 (vector-push-extend document (documents shard)))
		       (setf action-taken :added))))
	       (progn
		 (bt:with-lock-held ((getx (lock shard) :docs))
		   (vector-push-extend document (documents shard)))
		 (setf action-taken :added)))))

    (values
     document
     action-taken
     (if (equalp action-taken :replaced)
	 existing-document%))))

(defgeneric persist-document (collection document-form &key shard &allow-other-keys)
  (:documentation "Traverses the document and composes a list representation that is written to file. If the document is new it is added to the collection."))


(defmethod persist-document :before (collection document &key shard &allow-other-keys)
  ;;Loads collection if not loaded yet.

  (if shard
      (cl-naive-store::load-shard collection shard nil)      
      (load-data collection)))

;;TODO: Deal with shards
(defmethod persist-document ((collection collection) document			     
			     &key shard (handle-duplicates-p t) delete-p
			       file-stream &allow-other-keys)

  (unless shard
    (setf shard (get-shard collection (document-shard-mac collection document))))

  (cond ((or delete-p (getx document :deleted-p))
	 (remove-document collection document :shard shard)
	 (if file-stream
	     (naive-impl::write-to-stream file-stream
					  (naive-impl::persist-parse collection shard document nil))
	     (naive-impl:write-to-file (location shard)
				       (naive-impl::persist-parse collection shard document nil))))
	(t

	 (if file-stream
	     (naive-impl::write-to-stream
	      file-stream
	      (naive-impl:persist-parse
	       collection
	       shard
	       (add-document collection document
			     :handle-duplicates-p handle-duplicates-p
			     :shard shard)
	       nil))
	     (naive-impl:write-to-file
	      (location shard)
	      (naive-impl:persist-parse
	       collection
	       shard
	       (add-document collection document
			     :handle-duplicates-p handle-duplicates-p
			     :shard shard)
	       nil)))))
  document)


