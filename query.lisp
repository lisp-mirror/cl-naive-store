(in-package :cl-naive-store)

(defgeneric naive-reduce (collection &key query function initial-value &allow-other-keys)
  (:documentation "Uses query to select data objects from a collection and applies the function to 
those objects returning the result."))

(defmethod :before naive-reduce (collection &key &allow-other-keys)
  "Lazy loading data."
  (load-data collection))

(defmethod naive-reduce ((collection collection) &key query function initial-value  &allow-other-keys)
  (naive-reduce (data-objects collection)
		:query query
		:function function
		:initial-value initial-value))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((hash-table hash-table) &key query function initial-value  &allow-other-keys)
  (let ((result initial-value))
    (maphash
     (lambda (key object)
       (declare (ignore key))
       (if query
	   (if (funcall query object)
	       (if function		       
		   (setf result (funcall function result object))
		   (push object result)))
	   (if function
	       (setf result (funcall function result object))
	       (push object result))))
     hash-table)))

;;:TODO: This should be target to move to cl-query or something
(defmethod naive-reduce ((list list) &key query function initial-value  &allow-other-keys)
  (reduce #'(lambda (result object)
	      (if query
		  (if (apply query (list object))
		      (if function		       
			  (funcall function result object)
			  (push object result))
		      result)
		  (if function		       
		      (funcall function result object)
		      (push object result))))
	  list
	  :initial-value initial-value))

(defgeneric query-data (collection &key query &allow-other-keys)
  (:documentation "Returns the data that satisfies the query"))

(defmethod query-data :before ((collection collection) &key &allow-other-keys)
  "Lazy loading data."
  (load-data collection))

(defmethod query-data ((collection collection) &key query &allow-other-keys)  
  (query-data (data-objects collection)
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
	(data-objects collection))))

(defgeneric query-data-object (collection &key query &allow-other-keys)
  (:documentation "Returns the first last-data object found, and any others that satisfies the query"))

(defmethod query-data-object :before ((collection collection) &key &allow-other-keys)
  "Lazy loading data."
  (load-data collection))

(defmethod query-data-object ((collection collection) &key query &allow-other-keys)
  (let ((objects (query-data collection :query query)))
    (values (first objects) (rest objects))))

(defmethod query-data-object ((store store) &key collection-name query &allow-other-keys)
  (let ((objects (query-data store :collection-name collection-name :query query)))
    (values (first objects) (rest objects))))

;;TODO: Replace reduce with map-append
(defmethod query-data ((list list) &key query &allow-other-keys)
  (if query
      (map-append #'(lambda (object)
				      (when (apply query (list object))
					(list object)))
				  list)
      list))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-data ((hash-table hash-table) &key query &allow-other-keys)
  (let ((objects))
    (maphash
     (lambda (key object)
       (declare (ignore key))
       (if query
	   (when (funcall query object)
	     (push object objects))
	   (push object objects)))
     hash-table)
    objects))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-data-object ((list list) &key query &allow-other-keys)
   (let ((objects (query-data list :query query)))
    (values (first objects) (rest objects))))

;;:TODO: This should be target to move to cl-query or something
(defmethod query-data-object ((hash-table hash-table) &key query &allow-other-keys)
  (let ((objects))
    (maphash
     (lambda (key object)
       (declare (ignore key))
       (when (funcall query object)
	 (push object objects)))
     hash-table)
    (values (first objects) (rest objects))))
