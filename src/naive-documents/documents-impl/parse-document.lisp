(in-package :documents-impl)

(defun document-values-p (list)
  "Checks if plist contains :values keyword which would indicate the plist represents an document."
  (find :values list :test #'equalp))


(defmethod naive-impl:type-of-sexp ((collection document-collection) document-form)
  (cond ((and (listp document-form)
	      (equalp (first document-form) :blob%))
	 :blob)
	((and (listp document-form)
	      (atom (first document-form))
	      (symbolp (first document-form))
	      (> (length document-form) 1)
	      (document-values-p document-form)
	      ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
	      (not (or
		    (digx document-form :elements :reference%)
		    (digx document-form :values :reference%))))
	 :child-document)
	((and (listp document-form)
	      (atom (first document-form))
	      (symbolp (first document-form))
	      (> (length document-form) 1)
	      (document-values-p document-form)
	      ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
	      (or
	       (digx document-form :values :reference%)
	       (digx document-form :values :reference%)))
	 :reference)
	
	(t nil)))


(defmethod naive-impl:compose-special ((collection document-collection) sexp (type (eql :document)))
  (let* ((resolved-values (naive-impl:compose-parse collection
						    ;;TODO: For backwards compatibility 
						    (or (digx sexp :elements)
							(digx sexp :values))
						    nil))
	 (existing-document (index-lookup-hash 
				 collection
				 (digx sexp :hash)))
	 (final-document))

    (if (getx sexp :deleted-p)
	(remove-document collection existing-document)        
	(cond (existing-document
	       (unless (equalp (document-elements existing-document) resolved-values)
		 (push (document-elements existing-document)
		       (document-versions existing-document))
		 (setf (document-elements existing-document) resolved-values))
	       (setf final-document existing-document))
	      (t

	       (setf final-document
		       (make-document
			:store (store collection)
			:collection collection
			:type-def (document-type collection)
			:hash (frmt "~A" (digx sexp :hash))
			:elements resolved-values))
	       
	       (add-document collection final-document))))
    
    final-document))

(defmethod naive-impl:compose-special ((collection document-collection) sexp (type (eql :child-document)))
  (make-document
   ;;TODO: For backwards compatibility
   :type-def (or
	  (digx sexp :document-type)
	  (digx sexp :item-type))
   :hash (frmt "~A" (getx sexp :hash))
   ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
   :elements (naive-impl:compose-parse collection (or (digx sexp :elements)
						    (digx sexp :values))
				     nil)))


(defmethod naive-impl:compose-special ((collection document-collection) sexp (type (eql :blob)))
  (declare (ignorable collection))
  (read-blob (cdr sexp)))

(defmethod naive-impl:compose-document ((collection document-collection) document-form &key &allow-other-keys)
    (naive-impl:compose-special collection
		     document-form
		     :document))