(in-package :naive-impl)

(defgeneric type-of-doc-element (collection sexp)
  (:documentation "Reports if the sexp represents a special form."))

(defmethod type-of-doc-element (collection element)
  (cond ((blob-p element)
	 :blob)
	((hash-table-p element)
	 :hash-table)
	((and (listp element)
	      (atom (car element))
	      (symbolp (car element))             
	      (cl-getx:getx element :reference%))
	 :reference)
	
	(t nil)))

(defgeneric persist-form (collection element element-type &key &allow-other-keys)
  (:documentation "Convert a document element to its list representation.

IMPL NOTES:

specialize element type like this (element-type (eql :blob)). DONT specialize on object
type directly because that will break type-of-doc-element. If you specialize element you need
to supply your own implementation of type-of-doc-element as well."))

(defmethod persist-form (collection document (element-type (eql :document))
			 &key &allow-other-keys)
  (break "shit")
  document)

;;TODO: Sort out blob paths once and for all!!!!
(defmethod persist-form (collection blob (element-type (eql :blob))
			 &key &allow-other-keys)
  (write-blob (getx blob :location) (blob-raw blob))
  (list :file-type (getx blob :file-type)
	:file-ext (getx blob :file-ext)
	:location (getx blob :location)
	:parent-accessor (getx blob :parent-accessor)))

(defmethod persist-form (collection reference (element-type (eql :reference))
			 &key &allow-other-keys)
  reference)

;;TODO: Deal with tests that is not just the funciton name
(defmethod persist-form (collection hash-table (element-type (eql :hash-table))
			 &key &allow-other-keys)
  (list :|hash-table|
	:|hash-table-test| (hash-table-test hash-table)
	(maphash-collect
	 (lambda (key value)               
	   (list :key key :object value))
	 hash-table)))

;;Made this a seperate method so simple units tests can test basic parsing.
(defgeneric persist-parse (collection sexp doc &key &allow-other-keys)
  (:documentation "Transcribes document to list form for peristance."))

(defmethod persist-parse (collection sexp doc &key &allow-other-keys)
  (cond ((null sexp)
	 (nreverse doc))                   
        ((consp (car sexp))                    
	 (persist-parse collection (cdr sexp)
			(if (type-of-doc-element collection (car sexp))
                            (cons
			     (persist-form collection
					   (car sexp)
					   (type-of-doc-element collection (car sexp)))
			     doc)
			    (cons (persist-parse collection
						 (car sexp)
						 nil)
				  doc))))
	(t
	 (persist-parse collection (cdr sexp)
			(cons (car sexp) doc)))))

(defgeneric persist-delete-document (collection document file &key &allow-other-keys)
  (:documentation "Marks document as deleted."))

(defmethod persist-delete-document (collection document file
				    &key &allow-other-keys)
  (remove-document collection document)
  (setf (deleted-p document) t)
  (naive-impl:write-to-file file (naive-impl:persist-form
				  collection
				  document
				  :document)))




