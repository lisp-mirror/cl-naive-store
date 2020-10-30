(in-package :naive-impl)


(defparameter *task-pool-x* (make-instance 'cl-naive-task-pool:task-pool :thread-pool-size 8))

(defun load-document-reference-collection (universe document-ref)
  "When documents are persisted to file any document values that are referencing an document in a different collection is first sanitized (just enough info to retrieve the document later from where it is stored).

When documents are read from a file the references need to be converted to documents but for that to happen the collection containing the referenced documents need to be loaded first."
  (let* ((store (get-store* universe (getx document-ref :store)))
	 (collection (get-collection* store (getx document-ref :collection)))
	 (shard-mac (getx document-ref :shard-mac))
	 (timeout 0))
    ;; (break "fuck")


    (unless shard-mac
      (unless collection
	(setf collection (add-collection store (getx document-ref :collection))))
      (when collection
;;	(break "ref load ~A" collection)
	(load-data collection :parallel-p nil)))

    (when shard-mac
      ;;Incase the collection exists but has not been loaded try and load it.
      (when collection
	(let ((shard (get-shard collection shard-mac)))
	  (cl-naive-store::load-shard  collection shard nil)
	  ;;Another thread could have already started loading the docs
	  ;;and and load-shard will just return if that is the case
	  ;;but we need to know the docs are loaded successfully
	  ;;so we wait while loading.
	  ;;TODO: Need some timeout mechanism
	  (loop while (and (equalp (cl-naive-store::status shard) :loading)
			   (< timeout 1000000)
			   )
		:do (progn (incf timeout)
			   (when (> timeout 900000)
			     ;; (break "poes ~A" shard)
			     )))))
      
      (unless collection
	(add-collection store (getx document-ref :collection))
	(let ((shard (get-shard collection shard-mac)))
	  (cl-naive-store::load-shard collection shard nil)
	  ;;Another thread could have already started loading the docs
	  ;;and and load-shard will just return if that is the case
	  ;;but we need to know the docs are loaded successfully
	  ;;so we wait while loading.
	  ;;TODO: Need some timeout mechanism	
	  (loop while (and (equalp (cl-naive-store::status shard) :loading)
			   (< timeout 1000000)
			   )
		:do (progn (incf timeout)
			   (when (> timeout 900000)
			     ;; (break "poes ~A" shard)
			     )))
	  )))
    
    collection))

(defgeneric find-document-by-hash (collection hash &key shards &allow-other-keys)
  (:documentation "Finds the document that matches the hash."))

;;TODO: Deal with shards. 
(defmethod find-document-by-hash (collection hash &key shards &allow-other-keys)

  (do-sequence (shard (if shards shards
			  (shards collection)) :parallel-p nil)
    
    (do-sequence (document (documents shard))    
      (when (string-equal
	     (digx document :hash)
	     hash)
	
	(return-from find-document-by-hash document)))))

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

(defgeneric compose-special (collection shard sexp type)
  (:documentation "Does special processing to compose a specific type of document or element."))

(defmethod compose-special (collection shard sexp (type (eql :document)))
  (naive-impl::debug-log (format nil "core:Compose-special :document ~A~%" (name collection)))
  (if (getx sexp :deleted-p)
	(remove-document collection sexp :shard shard)
	;;TODO: Where to get handle-duplicates-p ???
	(add-document collection sexp :shard shard)))

(defmethod compose-special (collection shard sexp (type (eql :blob)))
  (declare (ignorable collection) (ignorable shard) (ignorable type))
  
  (read-blob (cdr sexp)))

(defmethod compose-special (collection shard sexp (type (eql :hash-table)))
  (declare (ignorable collection) (ignorable shard) (ignorable sexp) (ignorable type))
  (error "Reading of hash-tables not implmented yet."))

(defmethod compose-special (collection shard sexp (type (eql :reference)))
  (declare (ignorable shard))

  (naive-impl::debug-log (format nil "core:Compose-special :reference ~A~%" (name collection)))
  
  (let* ((ref-collection (load-document-reference-collection
			   (universe (store collection)) sexp))
	(ref-document (and collection
			 (find-document-by-hash 
			  ref-collection
			  (digx sexp :hash)))))           
    (unless ref-document

      ;;(break "grrr")
      
      #|
      (break "shit ~A~%~A~%~A" ref-collection (digx sexp :hash)
	     (find-document-by-hash 
			  ref-collection
			  (digx sexp :hash)))
      |#
      (write-log (location (universe (store collection)))
		 :error (list "Could not resolve reference ~S~%" sexp)))

    (naive-impl::debug-log (format nil "END core:Compose-special :reference ~A~%" (name collection)))
    
    ref-document))

;;Made this a seperate method so simple units tests can test basic parsing.
(defgeneric compose-document (collection shard document-form &key &allow-other-keys)
  (:documentation "The loading of documents happens in a two step process. First documents are read with (*read-eval* nil). Then the sexp representing a raw document is processed to compose the required in memory representation."))

(defgeneric compose-parse (collection shard sexp doc)
  (:documentation "Processes document form for compose-document."))

(defmethod compose-parse (collection shard sexp doc)
  (cond ((null sexp)
	 (nreverse doc))                   
        ((consp (car sexp))                    
	 (compose-parse collection shard (cdr sexp)
		(if (type-of-sexp collection (car sexp))
                    (cons
		     (compose-special collection shard (car sexp)
				      (type-of-sexp collection (car sexp)))
		     doc)
		    (cons (compose-parse collection shard (car sexp) nil) doc))))
	(t
	 (compose-parse collection shard (cdr sexp)
		(cons (car sexp) doc)))))

(defmethod compose-document (collection shard document-form &key &allow-other-keys)
  (naive-impl::debug-log (format nil "core:Compose-document ~A~%" (name collection)))
  (let ((doc
	  (compose-special collection
			   shard
			   (compose-parse collection shard document-form nil)
			   :document)))
    (naive-impl::debug-log (format nil  "END core:Compose-document ~A~%" (name collection)))
    doc)

  )
