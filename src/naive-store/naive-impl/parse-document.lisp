(in-package :naive-impl)

(defun load-document-reference-collection (universe document-ref)
  "When documents are persisted to file any document values that are referencing an document in a different collection is first sanitized (just enough info to retrieve the document later from where it is stored).

When documents are read from a file the references need to be converted to documents but for that to happen the collection containing the referenced documents need to be loaded first."
  (let* ((store (get-store* universe (getx document-ref :store)))
	 (collection (get-collection* store (getx document-ref :collection))))
    
    ;;Incase the collection exists but has not been loaded try and load it.
    (when collection
      (load-data collection))
    
    (unless collection
      (add-collection store collection)
      (load-data collection))
    collection))

(defgeneric find-document-by-hash (collection hash)
  (:documentation "Finds the document that matches the hash."))

(defmethod find-document-by-hash (collection hash)
 (dolist (document (documents collection))    
    (when (string-equal
	   (digx document :hash)
	   hash)
      (return-from find-document-by-hash document))))

;;TODO: Implment hash-table.
(defgeneric type-of-sexp (collection sexp)
  (:documentation "Reports if the sexp represents a special form, like a blob or reference."))

(defmethod type-of-sexp (collection sexp)
  (declare (ignorable collection))
  
  (cond ((and (listp sexp)
	      (equalp (car sexp) :blob%))
	 :blob)
	((and (listp sexp)
	      (equalp (car sexp) :|hash-table|))
	 :hash-table)
	((and (listp sexp)
	      (atom (car sexp))
	      (symbolp (car sexp))             
	      (cl-getx:getx sexp :reference%))
	 :reference)
	
	(t nil)))

(defgeneric compose-special (collection sexp type)
  (:documentation "Does special processing to compose a specific type of document or element."))

(defmethod compose-special (collection sexp (type (eql :document)))
  (if (getx sexp :deleted-p)
	(remove-document collection sexp)
	;;TODO: Where to get handle-duplicates-p ???
	(add-document collection sexp)))

(defmethod compose-special (collection sexp (type (eql :blob)))
  (declare (ignorable collection))
  (read-blob (cdr sexp)))

(defmethod compose-special (collection sexp (type (eql :hash-table)))
  (declare (ignorable collection))
  (error "Reading of hash-tables not implmented yet."))

(defmethod compose-special (collection sexp (type (eql :reference)))
  (let ((ref-document (and collection
			 (find-document-by-hash 
			  (load-document-reference-collection
			   (universe (store collection)) sexp)
			  (digx sexp :hash)))))           
    (unless ref-document
      (write-log (location (universe (store collection)))
		 :error (list "Could not resolve reference ~S~%" sexp)))
    ref-document))

;;Made this a seperate method so simple units tests can test basic parsing.
(defgeneric compose-document (collection document-form &key &allow-other-keys)
  (:documentation "The loading of documents happens in a two step process. First documents are read with (*read-eval* nil). Then the sexp representing a raw document is processed to compose the required in memory representation."))

(defgeneric compose-parse (collection sexp doc)
  (:documentation "Processes document form for compose-document."))

(defmethod compose-parse (collection sexp doc)
  (cond ((null sexp)
	 (nreverse doc))                   
        ((consp (car sexp))                    
	 (compose-parse collection (cdr sexp)
		(if (type-of-sexp collection (car sexp))
                    (cons
		     (compose-special collection (car sexp)
				      (type-of-sexp collection (car sexp)))
		     doc)
		    (cons (compose-parse collection (car sexp) nil) doc))))
	(t
	 (compose-parse collection (cdr sexp)
		(cons (car sexp) doc)))))

(defmethod compose-document (collection document-form &key &allow-other-keys)
    (compose-special collection
		     (compose-parse collection document-form nil)
		     :document))
