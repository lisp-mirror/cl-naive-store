(in-package :naive-impl)

(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while.

Source: Giovanni Gigante https://sourceforge.net/p/cl-cookbook/patches/8/|#"
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
		:do (sleep ,(or interval 0.001))
		:finally (close ,lock-file))
	      ,@body)
	 (handler-case
	     (delete-file ,lock-path)
	   (error (err)
	     (naive-impl:write-log nil :error '("While deleting file ~S: ~A" ,lock-path err))))))))


(defun file-to-string (file)
  "Reads a file and returns the contents as a string.

NOTES: You could achieve the same with with-output-to-string, but now you dont have to worry about supplying a string that can be written to."
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

(defmacro with-open-file-lock ((stream file 
				       &key
				       (direction :output)
				       (if-exists :append)				     
				       (if-does-not-exist :create))
			       &body body)
  "Opens a file with a file lock and writes to the file."
  (let ((file% (gensym))
	(direction% (gensym))
	(if-does-not-exist% (gensym))
	(if-exists% (gensym)))
    
    `(let ((,file% ,file)
 	   (,if-exists% ,if-exists)
	   (,direction% ,direction)
	   (,if-does-not-exist% ,if-does-not-exist))
       (with-file-lock (,file%)
	 (with-open-file (,stream ,file%
				    :direction ,direction%
				    :if-exists ,if-exists%
				    :if-does-not-exist ,if-does-not-exist%)
	     ,@body)))))

(defun write-to-file (file object &key (if-exists :append))
  "Writes to file using with-open-file-lock."  
  (with-open-file-lock (stream file :if-exists if-exists)
    (fresh-line stream)
    (write object :stream stream)
    (fresh-line stream)))

(defun write-to-stream (stream object)
  "Writes to stream with fresh-lines."  
  (fresh-line stream)
  (write object :stream stream)
  (fresh-line stream))

(defun write-list-items-to-file (file list &key (if-exists :append))
  "Does not wrap items in ()."
  (with-open-file-lock (stream file :if-exists if-exists)
    (dolist (object list)
      (fresh-line stream)
      (write object :stream stream)
      (fresh-line stream))))


(defun sexp-from-file (pathname)
  (with-open-file-lock (in pathname :if-does-not-exist :create)
    (with-standard-io-syntax
      (read in nil))))

(defun (setf sexp-from-file) (new-sexp pathname)
  (with-open-file-lock (out pathname
			:direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (with-standard-io-syntax
      (let ((*PRINT-CIRCLE* t))
	(prin1 new-sexp out)
	(terpri out)))))
