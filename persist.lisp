(in-package :cl-naive-store)

(defvar *print-readability-p*)

(defmacro set-print-reabability (readability-p)
  "*print-readably* could produce different written formats between different implementations of lisp and could cause errors when then object is read between implementations. cl-naive-store sets *print-readability* to nil by default wor write-object. 

This basically forces you to specialize write-object to create portable read representations for your objects that will play nicely with any lisp implementation.

If you are confident that your data will only ever be read by one version of an implementation then you can set *print-readability-p* to t. 

Made this a macro so that the behavior is set at compile time to save you the heartache of ending up with mixed readable objects in your db."
 
  `(setf *print-readability-p* ,readability-p))

(set-print-reabability nil)

(defmacro print-readability-p ()
  "Check if print-readability is set."
  *print-readability-p*)

;;Pilfered from Giovanni Gigante https://sourceforge.net/p/cl-cookbook/patches/8/
(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while"
  (let ((lock-path (gensym))
	(lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
	    (progn
	      (loop 
		 :for ,lock-file = (open ,lock-path :direction :output
					 :if-exists nil
					 :if-does-not-exist :create)
		 :until ,lock-file
		 :do (sleep ,(or interval 0.1))
		 :finally (close ,lock-file))
	      ,@body)
	 (ignore-errors
	   (delete-file ,lock-path))))))

(defgeneric write-object (object stream)
  (:documentation "Write an object to stream. Specialize this method if you want to influence how the object is physically represented in a file. Don't specialize this method to change what parts are or not written to a file, to do that specialize persist.

If you are writing any data objects to file not using write-object some where in the chain you are working against cl-naive-store.
"))

(defmethod write-object :around (object stream)
  "Enforces *print-readability-p behaviour."
  (if *print-readability-p*
      (call-next-method)
      (let (*print-readably*) 
	(call-next-method))))

(defmethod write-object (object stream)
  (prin1 object stream))

(defmacro wrap-in-list ((stream &key descriptor) &body body)
  (let ((stream% (gensym))
	(descriptor% (gensym)))
    `(let ((,stream% ,stream)
	   (,descriptor% ,descriptor))      
       (write-char #\( ,stream%)
       (when ,descriptor%
	 (write ,descriptor% :stream ,stream%)
	 (write-char #\space ,stream%))
       ,@body
       (write-char #\) ,stream%))))

(defmethod write-object ((object list) stream)
  "Writes out a list form where each item in the list was printed by write-object."
  (wrap-in-list (stream :descriptor nil)
   (do* ((x object (cdr x))
	 (y '() (unless (endp x) (write-char #\Space stream))))   
	((endp x))
     (write-object (car x) stream))))

(defmethod write-object ((object hash-table) stream)
  "Writes out a list form of key value pairs representing a hash table."
  (wrap-in-list (stream :descriptor :#hash-table)
   (maphash (lambda (key value)               
	      (write-object (list :key key :object value) stream)
	      ;;TODO: figure out how to not write space out on last -- effeciently
	      (write-char #\Space stream))
	    object)
   stream))

(defmacro with-file-lock-write ((file &key (if-exists :append)) &body body)
  (let ((file% (gensym))
	(if-exists% (gensym)))
    
    `(let ((,file% ,file)
	   (,if-exists% ,if-exists))
       (with-file-lock (,file%)
	 (with-open-file (out ,file%
			      :direction :output
			      :if-exists ,if-exists%
			      :if-does-not-exist :create)
	   (with-standard-io-syntax
	     ,@body)
	   (close out))))))

(defun write-to-file (file object &key (if-exists :append))
  (with-file-lock-write (file :if-exists if-exists)
    (fresh-line out)
    (write-object object out)
    (fresh-line out)))

(defun write-list-values-to-file (file list &key (if-exists :append))
  (with-file-lock-write (file :if-exists if-exists)
    (dolist (object list)
      (fresh-line out)
      (write-object object out)
      (fresh-line out))))

(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Persist is used to write \"stuff\" to files."))
  
(defmethod persist ((list list) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes a list of data objects to a file as individual items."
  
  (write-list-values-to-file file list :if-exists if-exists))


(defun write-hash-table-values-to-file (file hash &key (if-exists :append))
  (with-file-lock-write (file :if-exists if-exists)
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (fresh-line out)
	       (write-object value out)
	       (fresh-line out))
		   hash)))


(defmethod persist ((hash hash-table) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes the values of a hash table to a file as individual items."
  
  (write-hash-table-values-to-file file hash :if-exists if-exists))



