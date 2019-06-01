(in-package :cl-naive-items)

(defun item-values-p (list)
  "Checks if plist contains :values keyword which would indicate the plist represents an item."
  (find :values list :test #'equalp))

(defmethod parse-object-deleted-p ((collection item-collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (getf object :deleted-p))

(defmethod parse-object-p ((collection item-collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (and (listp object)
       (atom (first object))
       (symbolp (first object))
       (> (length object) 1)
       (item-values-p object)        
       (not (dig object :values :reference%))))

(defmethod parse-reference-object-p ((collection item-collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (and (listp object)
       (atom (first object))
       (symbolp (first object))
       (> (length object) 1)
       (item-values-p object)   
       (dig object :values :reference%)))


(defmethod parse-top-level-data-object ((collection item-collection) object &key &allow-other-keys)
  (let* ((final-item)
	 (ref-values (dig object :values))
	 (resolved-values (and ref-values
			       (parse-data-object collection ref-values)))
	 ;;for backwards compatibility when changed from hash to UUID
	 (looked-up-item  (index-lookup-uuid 
			   collection
			   (dig object :hash))))

    (cond (looked-up-item
	   (unless (parse-object-deleted-p collection object)
	     (unless (equalp (item-values looked-up-item) resolved-values)
	       (push  (item-values looked-up-item) (item-versions looked-up-item))
	       (setf (item-values looked-up-item) resolved-values))
	     (setf final-item looked-up-item))
		    
	   (when (parse-object-deleted-p collection object)
	     (remove-data-object collection looked-up-item)
	     (setf final-item nil)))
		   
	  ((not looked-up-item)
	   (unless (parse-object-deleted-p collection object)
	     (setf final-item
		   (make-item
		    :store (store collection)
		    :collection collection
		    :data-type (dig object :data-type)
		    :hash (frmt "~A" (dig object :hash))
		    :values resolved-values))

	     (push final-item (data-objects collection))
		
	     (add-index collection final-item)

	     (unless (string-equal (frmt "~A" (dig object :hash)) (item-hash final-item))
	       (write-to-file (format nil "~Ashash.err" (location (universe (store collection))))
			      (list
			       (location collection)
			       (format nil "~A"  (item-hash final-item))
			       (format nil "~A" (dig object :hash))))
			  
	       (setf (gethash (dig object :hash)
			      (uuid-index (item-collection final-item)))
		     final-item)))
		    
	   (unless final-item
	     (unless (parse-object-deleted-p collection object)
	       (write-to-file
		(format nil "~Aerror.err" (location (universe (store collection))))	
			      (list (frmt "Could not resolve ~S" object)))
	       nil))))	   
    final-item))


(defmethod parse-child-data-object ((parent-collection item-collection) object &key &allow-other-keys)
  (let* ((ref-values (dig object :values))
	 (resolved-values (and ref-values
			       (parse-data-object parent-collection ref-values))))
 
	   (make-item
	    :data-type (dig object :data-type)
	    :hash (frmt "~A" (dig object :hash))
	    :values resolved-values)))
