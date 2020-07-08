(in-package :cl-naive-items)

(defgeneric parse-item-to-file-rep (collection object &key &allow-other-keys)
  (:documentation "Parses the object to a representation that can be pesisted to file."))

(defun item-to-file-rep-reference (item)
  (list
   :store (name (item-store item))
   :collection (name (item-collection item))
   :data-type (if (stringp (item-data-type item))
		  (item-data-type item)
		  (name (item-data-type item)))
   :hash (item-hash item)
   :values 
   '(:reference% t)))


(defun child-item-to-file-rep (collection item)
  (list
   :data-type (if (stringp (item-data-type item))
		  (item-data-type item)
		  (name (item-data-type item)))
   :hash (item-hash item)
   :values (parse-item-to-file-rep
	    collection
	    (or (item-changes item)
		(item-values item)))))

(defun item-to-file-rep (collection item)
  (list   
   :hash (item-hash item)
   :deleted-p (if (item-deleted-p item)
		  t
		  nil)
   :values (parse-item-to-file-rep
	    collection
	    (or (item-changes item)
		(item-values item)))))

(defmethod parse-item-to-file-rep ((collection item-collection) object
				   &key top-level-p location &allow-other-keys)
  (cond ((null object)
	 nil)
	 
	(top-level-p
	 (item-to-file-rep collection object))
	
	((and (item-p object) (not (item-collection object))) ;;item with no collection
	 (child-item-to-file-rep collection object))
	
	((and (item-p object) (item-collection object)) ;;reference-object
	 (item-to-file-rep-reference object))

	((blob-p object)
         
	 (let ((file (or (and (not (empty-p (blob-location object)))
			      (blob-location object))

			 (cl-fad:merge-pathnames-as-file
			  (pathname location)
			  (make-pathname :directory
					 (list :relative  (frmt "~A" (blob-parent-key object)))
					 :name (frmt "~A" (blob-parent-hash object))
					 :type (blob-file-ext object))))))
		
	   ;;TODO: move write to outside of parsing!!!!!
	   (write-blob file (blob-raw object))
	   
	   (list :blob%
		 (list :file-type (blob-file-type object)
		       :file-type (blob-file-ext object)
		       :location file))))
	
	((atom object)
	 object)
	
        ((consp object)
	 (mapcar (lambda (child)
		   (parse-item-to-file-rep collection child))
		 object))
        (t object)))


(defmethod write-object ((object item) stream)
  "Used to write an item."
  (if (not (item-collection object))
      (error
       "Item does not know its collection. Cant be written to stream. Is this a top level item?")
      (write-object (parse-item-to-file-rep (item-collection object) object
					    :top-level-p t
					    :location (location (item-collection object))) 
		    stream)))


(defgeneric check-item-values (object &key &allow-other-keys)
  (:documentation "Parses the object to a representation that can be pesisted to file."))

(defun check-child-item-values (object)
  ;;TODO: remove duplicate children
  ;;if children without a collection how to get the keys without going to the
  ;;data-type definition???
  ;;is duplicates not just an issue because of the way wfx is lazy to do its own
  ;;duplicate checking?
  ;;(remove-duplicate-items collection values)
  (mapcar (lambda (child)
	    (check-item-values child))
	  object))

(defun check-keys-and-synq (old new allow-key-change-p)
  (if (equalp (key-values (item-collection new) new)
	      (key-values (item-collection old) old))
      (progn
	    (setf (item-changes old) (or (item-changes new) (item-values new)))
	    old)	      
      (if allow-key-change-p
	  (progn
	    (setf (item-changes old) (or (item-changes new) (item-values new)))
	    old)
	  (error (frmt "Attempted key change not allowed ~%~S~%with~%~S" old new)))))

(defun check-item-values-top (item allow-key-change-p)
  (let* ((key-vals (key-values (item-collection item) item))
	(lookup-old (or (index-lookup-uuid (item-collection item)
					   (item-hash item))
			(and key-vals
			     (first (index-lookup-values
				     (item-collection item)
				     key-vals)))))
	 (final-item))

   ;; (break "~A" key-vals)
    (if lookup-old
	(if (eql item lookup-old)
	  (setf final-item item)
	  
	  (if (item-hash item)
	      (if (not (equalp (item-hash item) (item-hash lookup-old)))
		  (error (frmt "Clobbering ~%~S~%with~%~S" lookup-old item))
		  (progn
		    (setf final-item (check-keys-and-synq lookup-old item allow-key-change-p))
		    ))
	      (progn
		(setf final-item (check-keys-and-synq lookup-old item allow-key-change-p))
		)))
	(setf final-item item))
        
    (setf (item-values final-item)
	  (check-item-values (item-values final-item)))
    final-item))

(defmethod check-item-values (object
			      &key top-level-p allow-key-change-p &allow-other-keys)
  (cond ((null object)
	 nil)
	(top-level-p
	 (check-item-values-top object allow-key-change-p))	
	((item-p object) ;;reference-object
	 (setf (item-values object) (check-item-values (item-values object)))
	 object)

	((blob-p object)
         object)
	
	((atom object)
	 object)
	((consp object)
	  (check-child-item-values object))
	(t
	 object)))

(defun check-location (item &key collection)
  (let ((col (or collection (item-collection item))))    
    (if col
	(progn
	  (setf (item-collection item) col)
	  (if (store col)
	      (setf (item-store item) (store col))
	      (error
	       (format nil
		       "Dont know which store to use to persist item ~S" item))))
	(error (format nil "Dont know where to persist item ~S" item))))    
  item)

(defun parse-persist-object (file item)
  (with-file-lock-write (file)
    (fresh-line out)
    (write-object item out)
    (fresh-line out))
  item)

(defmethod persist ((item item) &key
				  collection
				  file
				  allow-key-change-p
				  new-file-p
				  &allow-other-keys)
  (let ((*persist-p* nil)
	(derived-file))

    (unless file
      ;;Resolve the location of the item
      (setf item (check-location item :collection collection))

      
      ;;Loads collection if not loaded yet.
      (load-data (item-collection item))

      (setf derived-file (cl-fad:merge-pathnames-as-file
			  (pathname (location (item-collection item)))
			  (make-pathname :name (name (item-collection item))
					 :type "log"))))


    (cond ((item-deleted-p item)
	   (remove-data-object (item-collection item) item)
	   (parse-persist-object (or file derived-file)
				 item)
	   (setf (item-persisted-p item) t))
	  (t	   
	   (let* ((changed-item (if new-file-p
				    item
				    (check-item-values item
						       :allow-key-change-p allow-key-change-p
						       :top-level-p t)))
		  ;;parsing the objects because its the easiest way to check
		  ;;equality of objets, especially hierarchical objects.
		  (original-item-parsed
		   (parse-item-to-file-rep (item-collection item) item
					   :top-level-p t
					   :location (location (item-collection item))))
		  (changed-item-parsed
		   (parse-item-to-file-rep (item-collection changed-item) changed-item
					   :top-level-p t
					   :location (location (item-collection changed-item)))))
	     
	     (when (or (empty-p (hash changed-item))
		       (not (equalp (getx original-item-parsed :values)
				    (getx changed-item-parsed :values))))
	       
	       (add-data-object (item-collection changed-item) changed-item)
	       (parse-persist-object (or file derived-file)
				     changed-item-parsed)
	       (setf (item-persisted-p item) t)))))))


