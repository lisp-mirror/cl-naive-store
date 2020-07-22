(in-package :cl-naive-store)

;;;; Query and naive-reduce is exists to hide the structure/internals of the collection form the user, the so its for convenience all the functionality here could be done with plain map and reduce cl functions.

(defgeneric naive-reduce (collection &key query function initial-value &allow-other-keys)
  (:documentation "Uses query to select data documents from a collection and applies the function to 
those documents returning the result.

NOTES:

Does lazy loading."))

(defmethod naive-reduce :before ((collection collection) &key &allow-other-keys)
  "Lazy loading data."
  (load-data collection))

(defmethod naive-reduce ((collection collection) &key query function initial-value  &allow-other-keys)
  (naive-reduce (documents collection)
		:query query
		:function function
		:initial-value initial-value))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((hash-table hash-table) &key query function initial-value  &allow-other-keys)
  (let ((result initial-value))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (if query
	   (if (funcall query document)
	       (if function
		   (setf result (funcall function result document))
		   (push document result)))
	   (if function
	       (setf result (funcall function result document))
	       (push document result))))
     hash-table)
    result))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((list list) &key query function initial-value  &allow-other-keys)
  (reduce #'(lambda (result document)
	      (if query
		  (if (apply query (list document))
		      (if function		       
			  (funcall function result document)
			  (push document result))
		      result)
		  (if function		       
		      (funcall function result document)
		      (push document result))))
	  list
	  :initial-value initial-value))

(defgeneric query-data (collection &key query &allow-other-keys)
  (:documentation "Returns the data that satisfies the query.

NOTES:

Does lazy loading."))

(defmethod query-data :before ((collection collection) &key &allow-other-keys)
  "Lazy loading data."  
  (load-data collection))

(defmethod query-data ((collection collection) &key query &allow-other-keys)  
  (query-data (documents collection)
		  :query query))

(defmethod query-data ((store store) &key collection-name query &allow-other-keys)
  (let ((collection (get-collection store collection-name)))
    
    (unless collection
      (setf collection (get-collection-from-def 
			store
			collection-name))
      (when collection	
	(add-collection store collection))
      (unless collection
	(error "Could not find or create collection ~A" collection-name)))
    
    (if query
	(query-data collection
		    :query query)
	(documents collection))))

(defgeneric query-document (collection &key query &allow-other-keys)
  (:documentation "Returns the first last document found, and any others that satisfies the query

NOTES:

Does lazy loading."))

(defmethod query-document :before ((collection collection) &key &allow-other-keys)
  "Lazy loading data."
  (load-data collection))

(defmethod query-document ((collection collection) &key query &allow-other-keys)
  (let ((documents (query-data collection :query query)))
    (values (first documents) (rest documents))))

(defmethod query-document ((store store) &key collection-name query &allow-other-keys)
  (let ((documents (query-data store :collection-name collection-name :query query)))
    (values (first documents) (rest documents))))

;;Dont use map-append, it falls apart at 10 mil records.
(defmethod query-data ((list list) &key query &allow-other-keys)
  (let ((results))
    (if query
	(setf results (reduce #'(lambda (result document)		 
				  (if (apply query (list document))
				    (push document result)
				    result))
			      list
			      :initial-value '()))
	list)
     results))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-data ((hash-table hash-table) &key query &allow-other-keys)
  (let ((documents))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (if query
	   (when (funcall query document)
	     (push document documents))
	   (push document documents)))
     hash-table)
    documents))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-document ((list list) &key query &allow-other-keys)
   (let ((documents (query-data list :query query)))
    (values (first documents) (rest documents))))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-document ((hash-table hash-table) &key query &allow-other-keys)
  (let ((documents))
    (maphash
     (lambda (key document)
       (declare (ignore key))
       (when (funcall query document)
	 (push document documents)))
     hash-table)
    (values (first documents) (rest documents))))
