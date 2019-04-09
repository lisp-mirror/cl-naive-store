(in-package :cl-naive-store)

(defgeneric load-data (collection &key &allow-other-keys))

(defmethod load-data ((collection collection) &key &allow-other-keys)
  (let ((filename (format nil "~A/~A.log"
			  (location collection)
			  (name collection))))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax      
	(when in
	  (loop for line = (read in nil)
	     while line
	     do (parse-data-object
		 collection line :top-level-p t))
	  (close in))))
    (when (data-objects collection)
      (setf (loaded-p collection) t))))


(defun load-store-collections (store with-data-p)
  "Finds and loads collection definitions for a store, if with-items-p is t then the items are loaded as well."
  (let ((files (directory (format nil "~A**/*.col" (location store)))))
    (dolist (file files)
      (let ((file-contents))
	(with-open-file (in file :if-does-not-exist :create)
	  (with-standard-io-syntax              
	    (when in
	      (setf file-contents (read in nil))
	      (close in))))
	
	(when file-contents
	  
	  (let ((data-type (get-data-type store (getf file-contents :data-type)))
		(collection))
	    
	    (unless data-type
	      (load-store-data-types store)
	      (setf data-type (get-data-type store (getf file-contents :data-type))))

	    (unless data-type
	      (error "Collection data-type not found."))

	    (when data-type
	      (setf collection (add-collection 
				store 
				(make-instance (collection-class store)
					       :name (getf file-contents :name)
					       :location (getf file-contents :location)
					       :data-type data-type
					       :filter (getf file-contents :filter))))
	      (when with-data-p
		(load-data collection)))))))))

(defun load-stores (universe with-collections-p with-items-p)
  "Loads a whole universe, with or without collections and items. This is a way to pre load all the definitions 
in a universe, where as the default behaviour of naive-store is to lazy load definitions and data."
  (let ((files (directory (format nil "~A**/*.store" (location universe)))))
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
	    
	    (load-store-data-types store)
	    (when (or with-collections-p with-items-p)
	      (load-store-collections store with-items-p))))))))



(defgeneric load-store (store &key &allow-other-keys))

(defmethod load-store ((store store)  &key &allow-other-keys)
  (load-store-data-types store)
  (load-store-collections store t)
  (dolist (collection (collections store))
    (load-data collection)))


;;TODO: Move this to a maintenance.lisp and add functionality to do sanitize when loading data for the first time.

(defgeneric sanitize-data-file (collection &key &allow-other-keys)
  (:documentation "This removes all the deleted items from a collection. When a collection is loaded
only the active objects are loaded and by simply writing those active items out to a new file and then replacing
the old file deleted objects are removed."))

(defmethod sanitize-data-file ((collection collection) &key &allow-other-keys)
  (let ((items (fetch-items 
		collection)))
    (when (probe-file
	   (format nil "~A/~A.old"
		   (location collection)
		   (name collection)))
      (fad:copy-file

       (format nil "~A/~A.old"
	       (location collection)
	       (name collection))
       (format nil "~A/~A.old.old"
	       (location collection)
	       (name collection))
       :overwrite t))

    (fad:copy-file

     (format nil "~A/~A.log"
	     (location collection)
	     (name collection))
       
     (format nil "~A/~A.old"
	     (location collection)
	     (name collection))
     :overwrite t)
    
    (when items
      (dolist (item items)
	(cl-naive-store::persist item
				 :file (format nil "~A/~A.new"
					       (location collection)
					       (name collection))
				 :new-file-p t))
      (fad:copy-file (format nil "~A/~A.new"
			     (location collection)
			     (name collection))
		     (format nil "~A/~A.log"
			     (location collection)
			     (name collection))
		     :overwrite t))))
