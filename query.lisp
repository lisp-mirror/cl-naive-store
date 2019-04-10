(in-package :cl-naive-store)

(defun get-store-from-def (universe store-name)
  (let ((filename (format nil "~A~A/~A.store" 
			  (location universe) store-name store-name))
	(store-def)
	(store))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf store-def (read in nil))
	  (close in))))
    
    (when store-def
      (setf store
	    (make-instance (store-class universe)
			   :name (getf store-def :name)		    
			   :location (getf store-def :location)))
      (load-store-data-types store))
    store))

(defun get-collection-from-def (store collection-name)
  (let ((filename (format nil "~A~A.col" (location store) collection-name))
	(collection-def))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax
	(when in
	  (setf collection-def (read in nil))
	  (close in))))

    (when collection-def
      (let ((data-type (get-data-type store (getf collection-def :data-type))))
	(unless data-type
	  (load-store-data-types store)
	  (setf data-type (get-data-type store (getf collection-def :data-type))))

	(unless data-type
	  (error "Collection data-type could not be found."))
	
	(when data-type
	  (make-instance (collection-class store)
			 :store store
			 :name (getf collection-def :name)
			 :location (getf collection-def :location)
			 :data-type data-type))))))


(defun pushx (&optional result object)
  "Used by naive-reduce because cl push is a makro and not a function and reduce cant use it."
  (push object result))

(defgeneric naive-reduce (collection function query &key initial-value &allow-other-keys)
  (:documentation "Uses query to select data objects from a collection and then applies the function to 
those objects."))

(defmethod naive-reduce ((collection collection) function query &key initial-value)
  (let ((actual-result))
    (unless (data-types (store collection))
      (load-store-data-types (store collection)))

    ;;Load if not loaded
    (when (or (not (loaded-p collection))
	      (not (data-objects collection)))	
      (load-data collection))
 
    (reduce #'(lambda (result object)
		(declare (ignore result))
		       (when (apply query (list object))
			 (setf actual-result (funcall function actual-result object))))
		   (data-objects collection)
		   :initial-value initial-value)
    actual-result))

(defgeneric query-data (collection &key query &allow-other-keys)
  (:documentation "Returns the data that satisfies the query"))

(defmethod query-data ((collection collection) &key query &allow-other-keys)
  (if query
	(naive-reduce collection #'pushx query :initial-value '())
	(data-objects collection)))

(defmethod query-data ((store store) &key collection-name query &allow-other-keys)
  (let ((collection (get-collection store collection-name)))
    
    (unless collection
      (setf collection (get-collection-from-def 
			store
			collection-name))
      (when collection	
	(add-collection store collection))
      (unless collection
	(error "Could not create collection ~A" collection-name)))
    
    (if query
	(naive-reduce collection #'pushx query :initial-value '())
	(data-objects collection))))

(defgeneric query-data-object (collection &key query &allow-other-keys)
  (:documentation "Returns the first last-data object found, and any others that satisfies the query"))

(defmethod query-data-object ((collection collection) &key query &allow-other-keys)
  (let ((objects (query-data collection :query query)))
    (values (first objects) (rest objects))))

(defmethod query-data-object ((store store) &key collection-name query &allow-other-keys)
  (let ((objects (query-data store :collection-name collection-name :query query)))
    (values (first objects) (rest objects))))


(defmethod query-data ((list list) &key query &allow-other-keys)
  (if query
	(reduce #'(lambda (result object)
			    (when (apply query (list object))		
			      (funcall #'pushx result object)))
			list
			:initial-value '())
	list))

(defmethod query-data-object ((list list) &key query &allow-other-keys)
   (let ((objects (query-data list :query query)))
    (values (first objects) (rest objects))))


(defmethod query-data-object ((hashtable hash-table) &key query &allow-other-keys)
  (let ((objects))
    (maphash
     (lambda (key object)
       (declare (ignore key))
       (when (funcall query object)
	 (push object objects)))
     hashtable)
    (values (first objects) (rest objects))))

#|
(defun fetch-items* (store collection
		     &key test test-args 
		       (return-type 'list)
		       find-first-item-p)
  (let ((items))
    
    (unless (data-types store)
      (load-store-data-types store))

    (when collection      
      (unless (data-objects collection)
	
	(load-data collection))
      
      (setf items (if test
		      (map return-type
			   (lambda (item)
			     (if find-first-item-p
				 (when (apply test item test-args)
				   (return-from fetch-items* item))
				 (and (apply test item test-args) item)))
			   (data-objects collection))
		      (data-objects collection))))
    (remove-if #'not items)))

(defun fetch-store-items* (store collection-name 
			   &key test test-args 
			     (return-type 'list))
  (let ((collection (get-collection store collection-name)))
    
    (unless collection
      (setf collection (get-collection-from-def 
			store
			collection-name))
      (when collection	
	(add-collection store collection))
      (unless collection
	  (error "Could not create collection ~A" collection-name)))
   
    (fetch-items* store collection 
		  :test test
		  :test-args test-args
		  :return-type return-type)))




(defgeneric fetch-items (object &key test test-args
				  return-type
				  &allow-other-keys))

(defmethod fetch-items ((store store) &key collection-name
					test test-args
					(return-type 'list))
  
  (fetch-store-items* store collection-name :test test
		      :test-args test-args
		      :return-type return-type))

(defmethod fetch-items ((collection collection) 
			&key test test-args
			  (return-type 'list))
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:return-type return-type))

(defgeneric fetch-item (collection &key test test-args))

(defmethod fetch-item ((collection collection) 
		       &key test test-args)
  (fetch-items* (store collection) collection
		:test test
		:test-args test-args
		:return-type nil	
		:find-first-item-p t))





(defun find-in-item-list (item-list test)
  (map nil
       (lambda (item)
	 (when (apply test (list item))
	       (return-from find-in-item-list item)))
       item-list))

(defun find-items-in-item-list (item-list test)
  (remove-if #'not  (map 'list
			 (lambda (item)
			   (apply test (list item)))
			 item-list)))




(defun match-item (item item-list match-fields)
  (let ((exists nil))
    (dolist (list-item item-list)
      (dolist (field match-fields)
	(if (equalp (getx item field) (getx list-item field))	  
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(return-from match-item list-item)))))

(defun match-replace-item (item item-list match-fields)
  (let ((exists nil))
    (dolist (list-item item-list)
      (dolist (field match-fields)
	(if (equalp (getx item field) (getx list-item field))	  
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(remove list-item item-list)
	(push item item-list)))
   item-list))
|#
