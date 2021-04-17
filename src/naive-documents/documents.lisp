(in-package :cl-naive-documents)

(defmethod deleted-p ((document document))
  (document-deleted-p document))

(defmethod (setf deleted-p) (value (document document) &key &allow-other-keys)
  (setf (document-deleted-p document) value)
  document)

(defun check-keys-and-synq (old new allow-key-change-p)
 
  (if (equalp (key-values (document-collection new) new)
	      (key-values (document-collection old) old))
      (progn
	(setf (document-changes old) (or (document-changes new) (document-elements new)))
	old)	      
      (if allow-key-change-p
	  (progn
	    (setf (document-changes old) (or (document-changes new) (document-elements new)))
	    old)
	  (error (frmt "Attempted key change not allowed ~%~S~%with~%~S" old new)))))

(defun persist-merge-document (existing-document document allow-key-change-p)
  (let ((merged-document))
    (if (eql document existing-document)
	  (setf merged-document document)	  
	  (if (getx document :hash)
	      (if (not (equalp (getx document :hash) (getx existing-document :hash)))
		  (error (frmt "Clobbering ~%~S~%with~%~S" existing-document document))
		  (progn
		    (setf merged-document
			  (check-keys-and-synq existing-document
					       document allow-key-change-p))))
	      (progn
		(setf merged-document
		      (check-keys-and-synq existing-document
					   document allow-key-change-p)))))
    merged-document))

(defun reference-documents-equal-p (original prepped)
  (or (empty-p (getx prepped :hash))
      (not (equalp (getx original :elements)
		   (getx prepped :elements)))))


;;TODO: Find shard file
(defmethod persist-document ((collection document-collection) document
			     &key shard allow-key-change-p delete-p file-name
			       file-stream &allow-other-keys)
  "persist-document for document-collection is leniant in what it takes as a document, it can be of type document or a plist."

  (let ((*persisting-p* nil))
    (declare (special *persisting-p*))

    (unless shard
      (setf shard (get-shard collection (document-shard-mac collection document))))

    (unless shard
      (let* ((mac (document-shard-mac collection document))
	     (shardx 
	       (make-shard collection mac)))

	;;Make sure there is nothing to load.
	(cl-naive-store::load-shard collection shardx (location shardx))

	
	(cl-naive-store::set-shard-cache-safe% collection mac shardx)
	(vector-push-extend shardx (shards collection))

	(setf shard shardx)
	(naive-impl::debug-log "created new shard in add-document" :file-p t :args mac)))
    
    (unless (document-p document)
      (setf document (make-document 
		      :store (store collection)
		      :collection collection
		      :type-def (if (stringp (document-type collection))
				    (document-type (document-type collection))
				    (name (document-type collection)))		
		      :elements document)))

    (unless (document-collection document)
      (setf (document-collection document) collection))

    (unless (document-type-def document)
      (setf (document-type-def document) (document-type collection)))
    
    (let ((file (or file-name (location shard))))
      (cond ((or delete-p (deleted-p document))
	     (naive-impl:persist-delete-document collection shard document file :shard shard))
	    (t	   
	     (let* ((existing-document
		     (existing-document collection document :shard shard))
		    (original-document-parsed
		     (naive-impl:persist-form
                      collection
		      shard
		      document
		      :document))
		    (merged-document
		     (if existing-document
			 (persist-merge-document existing-document
						 document allow-key-change-p)))

		    ;;parsing the documents because its the easiest way to check
		    ;;value equality of documents, especially hierarchical documents.
					
		    (prepped-document-parsed
		     (naive-impl:persist-form
		      collection
		      shard
		      (or merged-document document)
		      :document)))

               

	       (when (or file-name
			 (empty-p (getx (or merged-document document) :hash))
			 (document-changes (or merged-document document))
			 (not existing-document)
			 (not (document-persisted-p (or merged-document document)))
			 (reference-documents-equal-p original-document-parsed
						      prepped-document-parsed))

                 
		 ;;To allow persist after loading stuff, add will cause error about
		 ;;clobbering 
		 (when (not existing-document)
		   (add-document collection (or merged-document document)))                 


		 (if (document-changes (or merged-document document))
		     (setf (document-elements (or merged-document document))
			   (document-changes (or merged-document document))))
		 
		 (setf (document-changes (or merged-document document)) nil)
		 
		 (if file-stream
		     (naive-impl::write-to-stream
		      file-stream
		      (naive-impl:persist-form
		       collection
		       shard
		       (or merged-document document)
		       :document))
		     (naive-impl:write-to-file file (naive-impl:persist-form
						     collection
						     shard
						     (or merged-document document)
						     :document)))

		 (setf (document-persisted-p (or merged-document document)) t))

	       (or merged-document document)))))))
