(in-package :naive-impl)

(local-time:enable-read-macros)

(defun map-append (fn &rest lists)
  "Non distructive version of mapcan.
Source: On Lisp"
  (apply #'append (apply #'mapcar fn lists)))

(defun maphash-collect (fn hash-table &key append-p)
  "Collects the results of a maphash. Pushes to a list by default, use append-p to append instead. NIL results are not collected."
  (let ((results))
    (maphash

     (lambda (key value)
       (let ((result (funcall fn key value)))
	 (when result
	   (if append-p
	       (setf result (append results (list result)) )
	       (push result results)))))     
     hash-table)
    results))

(declaim (inline frmt))
(defun frmt (control-string &rest args)
  "Short hand for (format nil ..)."
  (apply #'format nil control-string args))

(declaim (inline frmt))
(defun trim-whitespace (string)
  "Removes white spaces from a string."
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))

(defun empty-p (value)
  "Checks if value is null/nil or an empty string.."  
  (or
   (not value)   
   (equal value "")
   (if (stringp value)
       (let ((stripped-value (trim-whitespace value)))
	 (or (string-equal stripped-value "NIL")
	     (string-equal stripped-value "NULL")
	     (equal stripped-value ""))))))

;;TODO: There must be a lisp function that already does this
(defun plist-to-values (values)
  "Returns the values of a plist."
  (loop for (a b) on values by #'cddr 
     :collect b))

(defun plist-to-pairs (values)
  "Returns a list of key value pairs of a plist."
  (loop for (a b) on values by #'cddr 
     :collect (list a b)))

(defparameter *mac-key* 5873965167969913164)

(defgeneric make-mac (value &key key)
  (:documentation "Produces a mac from the value. Mac's should differ for different values.

NOTES:

This is used to create shard filenames. "))

(defmethod make-mac (value &key (key *mac-key*))

  (let* ((mac (ironclad:make-blake2-mac (ironclad:integer-to-octets key)))
	 (array (babel:string-to-octets
		 (frmt
		  "~S" value))))
    
    (ironclad:update-mac mac array)

    (ironclad:byte-array-to-hex-string
     (ironclad:produce-mac
      mac))))

(setf lparallel:*kernel* (lparallel:make-kernel (cl-cpus:get-number-of-processors)
						;;:bindings '((*with-open-file-lock* . nil))
						))

(defparameter *task-pool* (make-instance 'cl-naive-task-pool:task-pool :thread-pool-size 8))

(defvar *lock* (bt:make-lock))

(defgeneric gethash-safe (key hash &key lock recursive-p)
  (:documentation "Puts lock around hash access get and set."))

(defmethod gethash-safe (key hash &key (lock *lock*) (recursive-p nil))
  (if recursive-p
      (progn
	(bt:with-recursive-lock-held
	    (lock)
	  (gethash key hash)))
      (progn
	(bt:with-lock-held
	    (lock)
	  (gethash key hash)))))

(defmethod (setf gethash-safe) (value key hash &key (lock *lock*) (recursive-p nil))
  (if recursive-p 
      (bt:with-recursive-lock-held
	  (lock)	 
	(setf (gethash key hash) value))
      (bt:with-lock-held
	  (lock)	 
	(setf (gethash key hash) value))))

(defun call-do-sequence (thunk with-index-p sequence &key parallel-p )
  (if parallel-p     
      (lparallel:pdotimes (index (length sequence))
			   (let ((element (elt sequence index)))
			     (if with-index-p
				 (funcall thunk element index)
				 (funcall thunk element))))
      (etypecase sequence
	(list 
	 (loop for index from 0
	    for element in sequence
	    do (if with-index-p
		   (funcall thunk element index)
		   (funcall thunk element))))
	(vector         
	 (loop for index from 0
	    for element across sequence
	    do (if with-index-p
		   (funcall thunk element index)
		   (funcall thunk element)))))))

(defmacro do-sequence ((element-var sequence &key index-var (parallel-p nil) )		       
		       &body body)
  "Iterates over the sequence applying body. In the body you can use the element-var and/or the index-var if supplied. 

If you set parallel-p then the body is executed asyncronously. Asyncronous excecution places restraints on how special variables can be used within the body.

From lparallel documentation:

To establish permanent dynamic bindings inside workers (thread-local variables), use the :bindings argument to make-kernel, which is an alist of (var-name . value-form) pairs. Each value-form is evaluated inside each worker when it is created. (So if you have two workers, each value-form will be evaluated twice.)

Notes:

Uses loop or lparallel:pdotimes depending on parallel-p value.

To get the best out of do-sequence use the parallel option if the sequence is large (> 1000) or the body is excecution heavy.

"
  `(call-do-sequence
    (lambda (,element-var
	     ,@(when index-var
		 (list index-var)))
      ,@body)
  ,(when index-var t)
    ,sequence
    :parallel-p ,parallel-p ))
