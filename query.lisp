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



#|
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
