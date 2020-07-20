(in-package :cl-naive-store)

(defmethod load-data ((collection collection) &key &allow-other-keys)
  (let ((filename (ensure-location collection)))
    (with-open-file (in filename :if-does-not-exist :create)
      (when in
	  (loop for document-form = (read in nil)
	     while document-form
	     do (naive-impl::compose-document
		 collection document-form))
	  (close in)))))

(defun find-collection-definitions (store)
  (directory
   (cl-fad:merge-pathnames-as-file (pathname (ensure-location store))
				   (make-pathname :directory '(:relative :wild-inferiors)
						  :name :wild
						  :type "col"))))

(defun find-store-definitions (universe)
  (directory
   (cl-fad:merge-pathnames-as-file (pathname (ensure-location universe))
				   (make-pathname :directory '(:relative :wild-inferiors)
						  :name :wild
						  :type "store"))))

(defgeneric load-collections (store  &key with-data-p &allow-other-keys)
  (:documentation "Finds and loads collections of a store, with or without documents."))

(defmethod load-collections ((store store) &key with-data-p &allow-other-keys)
  "Finds and loads collection for a store, with or without documents."
  (let ((files (find-collection-definitions store)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (when in
	      (setf file-contents (read in nil))
	      (close in)))
	
	(when file-contents
	  (let ((collection
		 (add-collection 
		  store 
		  (make-instance (collection-class store)
				 :name (getx file-contents :name)
				 :location (getx file-contents :location)			
				 :filter (getx file-contents :filter)))))
	    (when with-data-p
	      (load-data collection))))))))

(defgeneric load-stores (universe  &key with-collections-p with-data-p &allow-other-keys)
  (:documentation "Finds and loads collections a store, with or without data documents."))

(defmethod load-stores (universe &key with-collections-p with-data-p &allow-other-keys)
  "Loads a whole universe, with or without collections and data documents."
  (let ((files (find-store-definitions universe)))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (when in
	      (setf file-contents (read in nil))
	      (close in)))
	(when file-contents
	  (let ((store (add-store 
			universe
			(make-instance
			 (store-class universe)
			 :name (getx file-contents :name)
			 :location (getx file-contents :location)))))
	    
	    (when (or with-collections-p with-data-p)
	      (load-collections store with-data-p))))))))


(defgeneric load-store (store &key &allow-other-keys)
  (:documentation "Loads the document-types and collections, with or without the actual data documents."))

(defmethod load-store ((store store) &key with-data-p &allow-other-keys)
  (load-collections store with-data-p))


