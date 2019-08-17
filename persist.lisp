(in-package :cl-naive-store)

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

(defun read-file-to-string (file)
  "Reads a file and returns the contents in a string."
  (let ((*read-eval* nil)
	(out (make-string-output-stream))
	(str ""))
    (with-open-file (in file
			:direction :input		     
			:if-does-not-exist nil)
      (when in
	(loop for line = (read-line in nil)
	   while line do (write-line line out) )
	(close in))
      (setf str (get-output-stream-string out))
      (close out))   
    str))

(defgeneric write-object (object stream)
  (:documentation "Write an object to stream. Using this method gives the user the chance to modify the
how a data oject is written by naive-store."))

(defmethod write-object (object stream)
  (pprint object stream))

(defun write-to-file (file object &key (if-exists :append))
  (with-file-lock (file)
    (with-open-file (out file
			 :direction :output
			 :if-exists if-exists
			 :if-does-not-exist :create)
      (with-standard-io-syntax
	;;*print-readably* set to nil so that sbcl writes strings out strings as simple strings.
	(let (*print-readably*) 
	  (write-object object out)))
      (close out))))

(defun write-list-to-file (file list &key (if-exists :append))
  (with-file-lock (file)
      (with-open-file (out file
			   :direction :output
			   :if-exists if-exists
			   :if-does-not-exist :create)
	(with-standard-io-syntax
	  ;;*print-readably* set to nil so that sbcl writes strings out strings as simple strings.
	  (let ((*print-readably*))
	    (dolist (object list)
	      (write-object object out)))
	  (close out)))))


(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Persist is used to write stuff to files. It is used to write naive-store 
structural elements like data-types and collections to file."))

(defmethod persist ((list list) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes a list of data objects to a file as individual items."
  (write-list-to-file file
		      list		      
		      :if-exists if-exists))


(defun write-hash-to-file (file hash &key (if-exists :append))
  (with-file-lock (file)
      (with-open-file (out file
			   :direction :output
			   :if-exists if-exists
			   :if-does-not-exist :create)

	(with-standard-io-syntax
	  ;;*print-readably* set to nil so that sbcl writes strings out strings as simple strings.
	  (let ((*print-readably*))
	    (maphash (lambda (key object)
		       (declare (ignore key))		       
		       (write-object object out))
		     hash))
	  (close out)))))

(defmethod persist ((hash hash-table) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes a list of data objects to a file as individual items."
  (write-hash-to-file file
		      hash		      
		      :if-exists if-exists))



