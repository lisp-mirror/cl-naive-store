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

(defgeneric existing-document (collection document  &key &allow-other-keys)
  (:documentation "Finds any documents with the same key values. This could return the exact same document or a similar document.

IMPL NOTES:

This is an essential part of loading and persisting documents, take care when implementing."))

(defmethod existing-document (collection document  &key &allow-other-keys)
  (let ((position (position document (documents collection)
			   :test (lambda (x y)
				   (equalp (key-values collection x)
					   (key-values collection y))))))
    (values (and position (nth position (documents collection)))
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

(defgeneric remove-document (collection document &key &allow-other-keys)
  (:documentation "Removes an document from the collection and its indexes. See add-document."))

(defmethod remove-document ((collection collection) document &key &allow-other-keys)  
  (setf (documents collection)
	(remove (if (keys collection)
		    (key-values collection document)
		    document)
		(documents collection)
		:test #'equalp :key (lambda (documentx)
				      (if (keys collection)
					  (key-values collection documentx)
					  documentx)))))

(defgeneric delete-document (collection document &key &allow-other-keys)
  (:documentation "Removes a document from the collection, marks the document as deleted and persists the deleted document to disk."))

(defmethod delete-document ((collection collection) document &key &allow-other-keys)
    (remove-document collection document)
    (setf (deleted-p document) t)
    (persist-document collection document :delete-p t))

(defgeneric add-document (collection document &key &allow-other-keys)
  (:documentation "Adds a document to the collection, it DOES NOT PERSIST the change, if you want adding with persistance use persist-document or persist the collection as a whole after you have done your adding.

add-document returns multiple values:

The first returned value is the actual document supplied.
The second returned value indicates what action was taken ie. was it added newly or was an exiting document replaced.
The third returned value is the replaced document. 

NOTES:   

In general you should not be calling add-document directly, you should use persist-document. Calling add-document directly is allowed so you can create temporary collections that can be thrown away.

cl-naive-store does not have a update-document function, add-document does both and its behaviour can be complex depending on the key parameters supplied. Also the behaviour can differ for different types of collections. Check the appropriate collection documentation for more details."))

(defmethod add-document ((collection collection) document
			    &key (handle-duplicates-p t) (replace-existing-p t) &allow-other-keys)
  "None of the following will have an effect if handle-duplicates = nil. 

If a document with the same keys exists in the collection the supplied the existing document will be replaced with the supplied document.

If you set replace-existing-p to nil then an existing document wont be replaced by the supplied document. Basically nothing will be done."
  (let ((existing-document%)
	(action-taken))

    (cond ((not handle-duplicates-p)
	   (push document
		  (documents collection))
	    (setf action-taken :added-possible-duplicate))
	  (t
	   (if (keys collection)
	       (multiple-value-bind (existing-document position)
		   (existing-document collection document)
                 
		 (setf existing-document% existing-document)
		 (if (and position replace-existing-p)
		     (progn                           
		       (setf (nth position (documents collection)) document)
		       (setf action-taken :replaced))
		     (progn
		       (push document (documents collection))
		       (setf action-taken :added))))
	       (progn
		 (push document (documents collection))
		 (setf action-taken :added)))))
    (values
     document
     action-taken
     (if (equalp action-taken :replaced)
	 existing-document%))))

(defgeneric persist-document (collection document-form &key &allow-other-keys)
  (:documentation "Traverses the document and composes a list representation that is written to file. If the document is new it is added to the collection."))


(defmethod persist-document :before (collection document &key &allow-other-keys)
  ;;Loads collection if not loaded yet.
  (load-data collection))

(defmethod persist-document ((collection collection) document
			     &key (handle-duplicates-p t) delete-p &allow-other-keys)
  (cond ((or delete-p (getx document :deleted-p))
	 (remove-document collection document)
	 (naive-impl:write-to-file (ensure-location collection)
				   (naive-impl::persist-parse collection document nil)))
	(t
	 (naive-impl:write-to-file
	  (ensure-location collection)
	  (naive-impl:persist-parse
	   collection
	   (add-document collection document
			      :handle-duplicates-p handle-duplicates-p)
	   nil)))
	)
  document)


