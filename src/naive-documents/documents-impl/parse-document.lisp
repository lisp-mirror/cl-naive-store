(in-package :documents-impl)

(defun document-values-p (list)
  "Checks if plist contains :elements keyword which would indicate the plist represents an document."
  (or
   (find :elements list :test #'equalp)
   ;;TODO: Here for backwards compatibility remove some time
   (find :values list :test #'equalp)))


(defmethod naive-impl:type-of-sexp ((collection document-collection) document-form)
  (cond ((and (listp document-form)
	      (equalp (first document-form) :blob%))
	 :blob)
	((and (listp document-form)
	      (atom (first document-form))
	      (symbolp (first document-form))
	      (> (length document-form) 1)
	      (document-values-p document-form)
	     
	      (not (or
		    (digx document-form :elements :reference%)
		     ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
		    (digx document-form :values :reference%))))
	 :child-document)
	((and (listp document-form)
	      (atom (first document-form))
	      (symbolp (first document-form))
	      (> (length document-form) 1)
	      (document-values-p document-form)
	     
	      (or
	       (digx document-form :elements :reference%)
	        ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
	       (digx document-form :values :reference%)))
	 :reference)
	
	(t
	 (if (or
	      (find :type document-form)
	       (find :document-type document-form )
	       (find :data-type document-form ))
	     (naive-impl:write-log (location (universe (store collection)))
			:error (list "Parsing is missing a child or reference ~%~A"  document-form)))

	 nil)))


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
	(if existing-document
	    (remove-document collection existing-document))        
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
			:type-def (cl-naive-document-types:document-type collection)
			:hash (frmt "~A" (digx sexp :hash))
			:elements resolved-values))
	       
	       (add-document collection final-document))))
    
    final-document))

(defmethod naive-impl:compose-special ((collection document-collection) sexp
				       (type (eql :child-document)))
  (make-document
   ;;TODO: For backwards compatibility
   :type-def (or
	      (digx sexp :type)
	      (digx sexp :document-type)
	      (digx sexp :data-type))
   :hash (frmt "~A" (getx sexp :hash))
   ;;TODO: For backwards compatibility with cl-naive-items, revisit this one day
   :elements (naive-impl:compose-parse collection (or (digx sexp :elements)
						      (digx sexp :values))
				       nil)))


(defmethod naive-impl:compose-special ((collection document-collection) sexp (type (eql :blob)))
  (declare (ignorable collection))
  ;;TODO: dealing with historical data should remove the check some time
  ;;was most likely to ensure balanced plists, should maybe implement that again
  ;;would make checking for types simpler
  
  (if (listp (car (cdr sexp)))
      (read-blob (car (cdr sexp)))
      (read-blob (cdr sexp))))

(defmethod naive-impl:compose-document ((collection document-collection) document-form &key &allow-other-keys)
  (naive-impl:compose-special collection
		     document-form
		     :document))
