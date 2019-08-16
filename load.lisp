(in-package :cl-naive-store)

(defmethod load-data ((collection collection) &key &allow-other-keys)
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location collection))
		   (make-pathname :name (name collection)
				  :type "log"))))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax      
	(when in
	  (loop for line = (read in nil)
	     while line
	     do (parse-data-object
		 collection line :top-level-p t))
	  (close in))))
    
    (setf (loaded-p collection) t)))

(defun find-collection-definitions (store)
  (directory
   (cl-fad:merge-pathnames-as-file (pathname (location store))
				   (make-pathname :directory '(:relative :wild-inferiors)
						  :name :wild
						  :type "col"))))

(defun find-store-definitions (universe)
  (directory
   (cl-fad:merge-pathnames-as-file (pathname (location universe))
				   (make-pathname :directory '(:relative :wild-inferiors)
						  :name :wild
						  :type "store"))))

(defgeneric load-store-collections (store  &key with-data-p &allow-other-keys)
  (:documentation "Finds and loads collections a store, with or without data objects."))


(defmethod load-store-collections ((store store) &key with-data-p &allow-other-keys)
  "Finds and loads collection for a store, with or without data objects."
  (let ((files (find-collection-definitions store)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	
	(when file-contents
	  (let ((collection
		 (add-collection 
		  store 
		  (make-instance (collection-class store)
				 :name (getf file-contents :name)
				 :location (getf file-contents :location)			
				 :filter (getf file-contents :filter)))))
	    (when with-data-p
	      (load-data collection))))))))

(defgeneric load-stores (universe  &key with-collections-p with-data-p &allow-other-keys)
  (:documentation "Finds and loads collections a store, with or without data objects."))

(defmethod load-stores (universe &key with-collections-p with-data-p &allow-other-keys)
  "Loads a whole universe, with or without collections and data objects."
  (let ((files (find-store-definitions universe)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	(when file-contents
	  (let ((store (add-store 
			universe
			(make-instance
			 (store-class universe)
			 :name (getf file-contents :name)
			 :location (getf file-contents :location)))))
	    
	    (when (or with-collections-p with-data-p)
	      (load-store-collections store with-data-p))))))))


(defgeneric load-store (store &key &allow-other-keys)
  (:documentation "Loads the data-types and collections, with or without the actual data objects."))

(defmethod load-store ((store store) &key with-data-p &allow-other-keys)
  (load-store-collections store with-data-p))


