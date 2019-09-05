(in-package :cl-naive-items)
;;;;There is a lot of legacy code in this file that needs to be redone!!!!

(defmethod write-object ((object item) stream)
  "Used to write an item."
  (pprint 
   (list
    :store (name (store (item-collection object)))
    :collection (name (item-collection object))
    :data-type (if (stringp (item-data-type object))
		   (item-data-type object)
		   (name (item-data-type object)))
    :hash (item-hash object)
    :deleted-p (item-deleted-p object)
    :values (item-values object))
   stream))

(defun key-values% (fields values)
  (let ((keys))
    (dolist (field fields)     
      (when (key-p field)
	(if (item-p (getx values (name field)))
	    (push (list (name field) (item-hash (getx values (name field)))) keys)
	    (push (list (name field) (getx values (name field))) keys))))
    (reverse keys)))

(defmethod index-values ((collection item-collection) (values item) &key &allow-other-keys)
  (loop for (a b) on (item-values values) by #'cddr
       when (find a (indexes collection) :test 'equalp)
     :collect (list a b)))

(defmethod key-values ((collection item-collection) values &key &allow-other-keys)
  (let ((data-type (data-type collection)))
    (unless data-type
    ;;Raising an error here because its problem with datatype specifications some where.
      (error "index-keys called with data-type = nil. 
cl-wfx tip: If this happened on a save look for a mismatch between a collection and its data-type's destinations"))
    (key-values% (fields data-type) (item-values% values))))

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

(defun parse-item (item)
  (plist-to-value-pairs (item-values% item)))

(defun set-hash (item)
  (unless (item-hash item)
    (let* ((hash (uuid:make-v4-uuid)))
      (setf (item-hash item) hash)
      hash)))

(defun item-to-reference (item location)
  (if (item-p item)
      (if (item-collection item)
	  (list
	   :store (name (item-store item))
	   :collection (name (item-collection item))
	   :data-type (if (stringp (item-data-type item))
			  (item-data-type item)
			  (name (item-data-type item)))
	   :hash (item-hash item)
	   :values 
	   '(:reference% t))
	  (list
	   :data-type (if (stringp (item-data-type item))
			  (item-data-type item)
			  (name (item-data-type item)))
	   :hash (or (item-hash item) (set-hash item ))
	   :values (parse-to-references%
		    item
		    (or (item-changes item)
			(item-values item))
		    location)))
      item))

(defun parse-to-references% (item values location)
  (let ((final)
	(value-pairs (parse-item values)))

    
    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))

	(if (item-p val)
	    (setf final (append final
				(list key (item-to-reference
					   val
					   location))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (item-p (first val))))
		(let ((children))
		  (dolist (it val)
		    (if (item-p it)
			(setf children
			      (append children 
				      (list (item-to-reference
					    
					     it
					     location))))
			(setf children (append children (list it)))))
		  (setf final (append final (list key children))))
		(cond ((blob-p val)
		      
		       (let ((file (or (and (not (empty-p (blob-location val)))
					    (blob-location val))

				       (cl-fad:merge-pathnames-as-file
						    (pathname location)
						    (make-pathname :directory (list :relative  (frmt "~A" key))
								   :name (item-hash item)
								   :type (blob-file-ext val))))))
		
			 ;;string-blob was converted to stream for persistence
			 ;;have to reset it to string-blob
			 (write-blob file (blob-raw val))
			 (setf final (append final (list key
							 (list :blob%
							       (list :file-type (blob-file-type val)
								     :file-type (blob-file-ext val)
								     :location file)))))))
			
		      (t
		       (setf final (append final (list key val)))))))))
    final))


(defun copy% (item)
  (let ((copy (copy-item item)))
    (setf (item-values copy) (copy-list (item-values item)))
    (setf (item-changes copy) (copy-list (item-changes item)))
    copy))

(defun parse-to-references (item location)
  (let ((ref-item (copy% item)))
    (setf (item-values ref-item)
	  (parse-to-references% item (item-values ref-item) location))
    ref-item))

;;Used to mark change deep in the bowls of the beast.
(defvar *persist-p* nil)

(defun check-no-collection-val (collection val allow-key-change-p)
  (unless (item-changes val)
    (setf (item-values val)
	  (check-item-values% collection
			      (item-values val)
			      allow-key-change-p))
    val)
  
  (when (item-changes val)
    (setf *persist-p* t)
    (setf (item-values val)
	  (check-item-values% collection
			      (item-changes val)
			      allow-key-change-p))
    (setf (item-changes val) nil)
    val)

  (unless (item-hash val)
    (set-hash val))
  
  val)

(defun new-duplicate-item (items)
  (let ((new-duplicate-item ))
	    
    (dolist (item items)
      (when (and (item-p item) (not (item-hash item)))
	(setf new-duplicate-item item)))
    new-duplicate-item))

(defun remove-duplicate-items (collection values)
  (let ((matching-hashes)
	(keyhash (make-hash-table :test 'equalp)))
    
    (dolist (item values)
      (when (item-p item)
	(let ((key-values (key-values collection
				(or (item-changes item) (item-values item)))))
	  
	  (if (gethash key-values keyhash)
	      (setf matching-hashes (push key-values matching-hashes)))
	  (setf (gethash key-values keyhash) (push item (gethash key-values keyhash))))))

    ;;(setf matching-hashes nil)
    (dolist (match matching-hashes)
      (let* ((items (gethash match keyhash))
	     (new-duplicate-item (new-duplicate-item items)))
	(when new-duplicate-item
	  (dolist (item items)
	    (when  (item-p item)
	      (unless (eq item new-duplicate-item)
		(setf (item-changes item) (item-changes new-duplicate-item))))))
	  (setf values (remove new-duplicate-item values))))
    values))

(defun check-item-values% (collection values allow-key-change-p)
  (let ((final)
	(value-pairs (parse-item values)))

    (dolist (pair value-pairs)
      (let ((key (first pair))
	    (val (second pair)))
	
	(if (item-p val) 
	    (setf final (append final
				(list key
				      (if (item-collection val)
					  (persist val :allow-key-change-p allow-key-change-p)
					  (check-no-collection-val collection val allow-key-change-p)))))
	    (if (or (and val (listp val) (listp (first val)))
		    (and val (listp val) (item-p (first val))))
		(let ((children))
		  
		  (dolist (it (remove-duplicate-items (store collection) val))
		    
		    (if (item-p it)
			(setf children
			      (append children 
				      (list
				       (if (item-collection it)
					   (persist it :allow-key-change-p allow-key-change-p)
					   (check-no-collection-val collection it allow-key-change-p)))))
			
			(setf children (append children (list it)))))
		  (setf final (append final (list key children))))
		(setf final (append final (list key val)))))))     
    final))


(defun change-in-item-p (item)
  (not (equalp (item-values item) (item-changes item))))


;;TODO: This needs a serious rewrite, its a cobbled job at best.
(defun check-item-values (item allow-key-change-p)

  (let ((change-p (and (item-changes item) (change-in-item-p item)))
	(lookup-old (or (index-lookup-uuid (item-collection item)
					   (item-hash item))
			(first (index-lookup-values (item-collection item)
						    (item-values item)))))
	(lookup-new (first (index-lookup-values (item-collection item)
						(item-changes item))))
	(final-item))

    (when change-p
      (when lookup-old
	(when lookup-new
	  (when (equalp (item-hash lookup-new) (item-hash lookup-old))

	    (unless (equalp (item-values lookup-new) (item-changes item))

	      (setf (item-values lookup-old)
		    (check-item-values% (item-collection item) (item-changes item) allow-key-change-p))
	      (setf (item-changes item) nil)
	      (setf final-item lookup-old)))

	  (unless (equalp (item-hash lookup-new) (item-hash lookup-old))

	    (unless allow-key-change-p
	      (error
	       (format
		nil
		"Cant change key values causes hash diff ~%~A~%~A~%~A~%~A" 
		(item-hash lookup-old)
		(item-hash item)
		(item-values lookup-old)
		(item-values item))))
	      
	    (when allow-key-change-p
	      (push (item-values lookup-old) (item-versions lookup-old))
	      (setf (item-values lookup-old)
		    (check-item-values% (item-collection item)
					(item-changes item)
					allow-key-change-p))	      
	      (remove-data-object (item-collection item) lookup-old)	      
	      (push item (data-objects (item-collection lookup-old)))
	      
	      (break "shit")
	      
	     ;; (add-index (item-collection item) lookup-old)
	      (setf final-item lookup-old)
	      )))
	
	(unless lookup-new
	  (unless (equalp (key-values (item-collection lookup-old)
				      (item-values lookup-old))
			  (key-values (item-collection lookup-old)
				      (item-changes lookup-old)))

	    (unless allow-key-change-p
	      (error
	       (format
		nil
		"Cant change key values causes hash diff ~%~A~%~A~%~A~%~A" 
		(item-hash lookup-old)
		(item-hash item)
		(item-values lookup-old)
		(item-values item))))
	      
	    (when allow-key-change-p
	      (push (item-values lookup-old) (item-versions lookup-old))
	      (setf (item-values lookup-old)
		    (check-item-values% (item-collection item)
					(item-changes item)
					allow-key-change-p))
	      (remove-data-object (item-collection item) lookup-old)	      
	     ;; (push item (data-objects (item-collection lookup-old)))
	     ;; (add-index (item-collection item) lookup-old)
	      (setf final-item lookup-old)))

	  (when (equalp (key-values  (item-collection lookup-old)
				     (item-values lookup-old))
			(key-values (item-collection lookup-old)
				    (item-changes lookup-old)))
	    (setf (item-values lookup-old)
		  (check-item-values% (item-collection item) (item-changes item)
				      allow-key-change-p))
	    (setf (item-changes item) nil)
	    (setf final-item lookup-old))))
      
      (unless lookup-old

	(when lookup-new
	  
	  (unless (equalp (item-values lookup-new) (item-changes item))
	    (setf (item-values lookup-new)
		  (check-item-values% (item-collection item) (item-changes item)
				      allow-key-change-p))
	    (setf (item-changes lookup-new) nil)
	    (setf final-item lookup-new)))
	(unless lookup-new
	  (setf (item-values item)
		(check-item-values% (item-collection item) (item-changes item)
				    allow-key-change-p))
	  (setf (item-changes item) nil)	   
	 ;; (push item (data-objects (item-collection item)))
	 ;; (add-index (item-collection item) item)
	  (setf final-item item))))

    (unless change-p
      
      (when lookup-old

	(let ((wtf (check-item-values% (item-collection item)
				       (or (item-changes item)
					   (item-values item))
				       allow-key-change-p)))

	  (if (equalp (item-values lookup-old)
		      (or (item-changes item)
			  (item-values item)))
	      (progn
		(when *persist-p*
		  (push (item-values lookup-old) (item-versions lookup-old))
		  (setf (item-values lookup-old) wtf)		
		  (setf (item-changes item) nil)
		  (setf final-item lookup-old))
		
		;;Don't save nothing changed
		(unless *persist-p*
		  (setf (item-changes lookup-old) nil)
		  (setf (item-changes item) nil)
		  (setf final-item nil)))
	      (progn
		(push (item-values lookup-old) (item-versions lookup-old))
		(setf (item-values lookup-old) wtf)		
		(setf (item-changes item) nil)
		(setf final-item lookup-old)))))
      
      (unless lookup-old

	 (setf (item-values item)
	       (check-item-values% (item-collection item)
				   (or (item-changes item)
				       (item-values item))
				   allow-key-change-p))
	 (setf (item-changes item) nil)	   
	 ;;(push item (data-objects (item-collection item)))
	 ;;(add-index (item-collection item) item)
	 (setf final-item item)))
    
    (when final-item
      
      (setf (item-deleted-p final-item) (item-deleted-p item)))
    
    final-item))

(defun parse-persist-object (file item)
  ;;Parse item to persistable format
  (let ((item-to-persist (parse-to-references item (directory-namestring file))))
    (when  item-to-persist
      (setf (item-persisted-p item) nil)	
      (write-to-file file
		     item-to-persist
		     :if-exists :append)
      (setf (item-persisted-p item) t))
    item))





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

      ;;Need to make sure there is really no data in the collection
      ;;else we end up with duplicates
      (unless (loaded-p (item-collection item))
	(load-data (item-collection item)))
      
      (setf derived-file (cl-fad:merge-pathnames-as-file
			  (pathname (location (item-collection item)))
			  (make-pathname :name (name (item-collection item))
					 :type "log"))))
  
    (let ((changed-item (if new-file-p
			    item
			    (check-item-values item allow-key-change-p))))

      
      (cond ((item-deleted-p item)
	     ;;The remove must be done much earlier in the process, take care of it in item persist
	     ;;rewrite.
	     (remove-data-object (item-collection item) item)
	     (parse-persist-object (or file derived-file)
				   item))
	    (changed-item

	     (setf item changed-item)
	     (setf item (add-data-object (item-collection item) item))
	     (parse-persist-object (or file derived-file)
				   item))
	    
	    (t
	     item)))))



#|


;;Started on rewrite

(defgeneric parse-item-to-file-rep (collection line &key &allow-other-keys)
  (:documentation "Parses the raw object read from file to its object reprensentation."))

(defmethod parse-item-to-file-rep ((collection item-collection) object  &key top-level-p &allow-other-keys)
  (cond ((null object)
	 nil)
	 
	(top-level-p
	 (item-to-file-rep collection object))
	
	((and (item-p object) (not (item-collection object))) ;;item with no collection
	 (child-item-to-file-rep collection object))
	
	((and (item-p object) (item-collection object)) ;;reference-object
	 (item-to-file-rep-reference object))
	;;blob
	
	((atom object)
	 object)
	
        ((consp object)
	 (mapcar (lambda (child)
		   (parse-item-to-file-rep collection child))
		 object))
        (t object)))

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
   :store (name (item-store item))
   :collection (name (item-collection item))
   :data-type (if (stringp (item-data-type item))
		  (item-data-type item)
		  (name (item-data-type item)))
   :hash (item-hash item)
   :values (parse-item-to-file-rep
	    collection
	    (or (item-changes item)
		(item-values item)))))

|#
