(in-package :cl-naive-store)

(defun find-collection-files (collection)
  (let ((path (cl-fad:merge-pathnames-as-file
	       (pathname (ensure-location collection))
	       (make-pathname :directory '(:relative :wild-inferiors)
			      :name :wild
			      :type "log")))
	(files (directory
		(cl-fad:merge-pathnames-as-file
		 (pathname (ensure-location collection))
		 (make-pathname :directory '(:relative :wild-inferiors)
				:name :wild
				:type "log")))))

    (unless files
      (naive-impl::debug-log (frmt "no files on path") :file-p t :args path))

    files))

(defgeneric load-shard (collection shard filename &key &allow-other-keys)
  (:documentation "Loads documents from file."))

(defmethod load-shard :around ((collection collection) shard filename &key &allow-other-keys)
  (let ((naive-impl:%loading-shard% shard))
    (call-next-method)))

;;TODO: Add a catch all error thingy so that the status does not get stuck in :loading
;;TODO: add a wait with a time out if .lock file exists?

;;Dont try to do compose-document asyncronously because the order in which the documents
;;are loaded in the underlying container matter, for deleted documents and document history!!!!
(defmethod load-shard ((collection collection) shard filename &key &allow-other-keys)

  (unless (equalp (status shard) :loading)
    (unless (probe-file (format nil "~a.lock" (location shard)))
      (let ((sexps))

	(setf (status shard) :loading)

	(naive-impl::debug-log (frmt "load-shard begin ") :file-p t :args (list shard filename))
	(with-open-file (in (or filename (location shard)) :if-does-not-exist :create)
	  (when in

	    ;;TODO: Reconsider this!!!

	    ;;Reading the file and releasing it as soon as possible ... not sure it is a good
	    ;;idea ... there is no difference in spead. It also doubles the memory use!!!
	    (setf sexps
		  (loop :for document-form = (read in nil)
			:while document-form
			:collect document-form))
	    (close in)

            (loop :for document-form in sexps
		  :do (naive-impl::compose-document
		       collection (or shard naive-impl:%loading-shard%) document-form)))))

      (naive-impl::debug-log (frmt "load-shard end ") :file-p t :args (list shard filename))
      (setf (status shard) :loaded))))

(defun load-shards (collection shards)
  (do-sequence (shard shards)
    (unless (status shard)
      (load-shard collection shard nil))))

(defparameter *files* (make-hash-table
                       ;; LispWorks and Clozure CL default with thread safe hash-tables.
		       ;; Other implementations will have to be checked.
		       #+lispworks :single-thread #+lispworks nil
		       #+(or sbcl ecl) :synchronized #+(or sbcl ecl) t
		       ;; This hash table is used from several threads,
		       ;; therefore it must be :shared t in ccl.
		       #+ccl :shared #+ccl t))

(defmethod load-data ((collection collection) &key shard-macs (parallel-p t) &allow-other-keys)
  (let ((tasks))
    (unless parallel-p
      (unless (> (length (shards collection)) 0)
	(let ((files (or (gethash collection *files*)
			 (setf (gethash collection *files*) (find-collection-files collection)))))

	  (naive-impl::debug-log (frmt "load-data") :file-p t :args files)

	  (do-sequence (filename files :parallel-p nil)
	    (multiple-value-bind (mac file)
		(match-shard filename shard-macs)

	      (unless mac
		(setf mac (pathname-name filename))
		(setf file filename))

	      (when (or (not shard-macs) file)
		(let ((shard (get-shard collection mac)))

		  (unless shard
		    (naive-impl::debug-log "loading load-shard"
					   :file-p t
					   :args (list shard filename))

		    (setf shard (make-shard collection mac))
		    (load-shard collection shard filename)
		    (set-shard-cache-safe% collection mac shard)
		    (vector-push-extend shard (shards collection))))))))))

    (when parallel-p
      (unless (> (length (shards collection)) 0)
	(let ((files (or (gethash collection *files*)
			 (setf (gethash collection *files*) (find-collection-files collection)))))
	  (when files
            (let ((channel (lparallel:make-channel)))
	      (dolist (filename files)
		(multiple-value-bind (mac file)
		    (match-shard filename shard-macs)

		  (unless mac
		    (setf mac (pathname-name filename))
		    (setf file filename))

		  (when (or (not shard-macs)
			    file)

		    (let ((shard (get-shard collection mac)))

		      (unless shard
			(setf shard (make-shard collection mac))
			(naive-impl::debug-log "submitting load-shard"
					       :file-p t
					       :args (list shard filename))

			(push (lparallel:submit-task
			       channel
			       (lambda ()
				 (load-shard collection shard filename)

				 (set-shard-cache-safe% collection mac shard)
				 (vector-push-extend shard (shards collection))))
			      tasks))))))

	      (naive-impl::debug-log "load-data checking tasks"
				     :file-p t
				     :args tasks)

	      (dotimes (i (length tasks))
                i
		;;TODO: WTF?
		;;without this it gets stuck on loading naive-documents some time.
		(sleep 0.0001)
		(lparallel:receive-result channel)))))))))

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
