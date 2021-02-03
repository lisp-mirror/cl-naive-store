(in-package :cl-naive-store)

(defun find-collection-files (collection)
  
  (let ((path (cl-fad:merge-pathnames-as-file (pathname (ensure-location collection))
				   (make-pathname :directory '(:relative :wild-inferiors)
						  :name :wild
						  :type "log")))
	(files (directory
		(cl-fad:merge-pathnames-as-file (pathname (ensure-location collection))
						(make-pathname :directory '(:relative :wild-inferiors)
							       :name :wild
							       :type "log")))))

    (unless files
      (naive-impl::debug-log (frmt "no files on path" ) :file-p t :args path))
    
    files))

;;TODO: only pass shard and not filename ????
(defgeneric load-shard (collection shard filename &key &allow-other-keys)
  (:documentation "Loads documents from file."))

(defvar *load-lock* (bt:make-recursive-lock))

;;TODO: Should we try reload if status is interupted or should we just lock the collection/shard
;;and have the user deal with it?
(defmethod load-shard :around ((collection collection) shard filename &key &allow-other-keys)
  
  (unless (status shard)

   ;; (setf (status shard) :loading)

    (call-next-method)))

;;TODO: Add a catch all error thingy so that the status does not get stuck in :loading

;;Dont try to do compose-document asyncronously because the order in which the documents
;;are loaded in the underlying container matter, for deleted documents and document history!!!!

;;TODO: add a wait with a time out if .lock file exists?
(defmethod load-shard ((collection collection) shard filename &key &allow-other-keys)

  
  (unless (equalp (status shard) :loading)
    (unless (probe-file (format nil "~a.lock" (location shard)))

      (setf (status shard) :loading)

      (naive-impl::debug-log (frmt "load-shard begin " ) :file-p t :args (list shard filename))
      (with-open-file (in (or filename (location shard)) :if-does-not-exist :create)
	(when in
	  (loop for document-form = (read in nil)
		while document-form
		do
		   (naive-impl::compose-document
		    collection shard document-form))
	  (close in)))
      (naive-impl::debug-log (frmt "load-shard end " ) :file-p t :args (list shard filename))
      (setf (status shard) :loaded))))


(defun load-shards (collection shards)
  (do-sequence (shard shards)
    (unless (status shard)
      (load-shard collection shard nil))))



(cl-naive-task-pool:start-task-pool *task-pool*)

(defmethod load-data ((collection collection) &key shard-macs (parallel-p t) &allow-other-keys)
  
  (let ((tasks))

    (unless parallel-p

      (unless (> (length (shards collection)) 0)
	;;(break "hoer ~A" files)

	(let ((files (find-collection-files collection))) 

	  (naive-impl::debug-log (frmt "load-data" ) :file-p t :args files)
	  
	  (do-sequence (filename files :parallel-p nil)
	    (multiple-value-bind (mac file)
		(match-shard filename shard-macs)

	      (break "??????")
	      (unless mac
		(setf mac (pathname-name filename))
		(setf file filename))
	      
	      (when (or (not shard-macs)
			file)
		
		(let ((shard (get-shard collection mac)))

		  (unless shard
		    (break "mother fucker no shard"))
		  
		  (if (or (> (length (documents shard)) 0)
			  (equalp (status shard) :loading)
			  (equalp (status shard) :loaded))                      
		      (load-shard collection shard filename)))))))))
    
    (when parallel-p
      
      (unless (> (length (shards collection)) 0)
	(let ((files (find-collection-files collection)))
	  (when files
            
	    (dolist (filename files)
	      (multiple-value-bind (mac file)
		  (match-shard filename shard-macs)

		(unless mac
		  (setf mac (pathname-name filename))
		  (setf file filename))
		
		(when (or (not shard-macs)
			  file)

		 ;; (break "??? ~A" collection)
		  (let ((shard (get-shard collection mac)))

		    ;;(break "pffft ~A~%~A~%~A~%~A" collection shard (length (documents shard)) (status shard))
		    
		    (unless (or (> (length (documents shard)) 0)
				(equalp (status shard) :loading)
				(equalp (status shard) :loaded))

		      ;;(break "pffft ~A" shard)
		      
		      (naive-impl::debug-log (frmt "submitting load-shard" )
					     :file-p t
					     :args (list shard filename))
		      
		      (push (cl-naive-task-pool:submit-task
			     *task-pool*
			     (lambda ()
			       (load-shard collection shard filename))
			     :name mac
			     :result-p t)
			    tasks))))))


	    (naive-impl::debug-log (frmt "load-data checking tasks" )
				   :file-p t
				   :args tasks)
	    
	    (dolist (task tasks)
	      ;;(break "~A" *task-pool*)
	      (cl-naive-task-pool:task-result *task-pool* task))

            
	    )))

      
      )
    #|  
    

 |#
    
    
    

 
    ;;(break "fuck")
    
    ))

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


