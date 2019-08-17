(in-package :cl-naive-store)

(defstruct blob
  "Represents a unit of data that is large enough to warent its own file or would cause reading problems for the default naive-store file layout which is essentially plists."
  file-type
  file-ext
  location
  raw)

(defun blob-string-value (blob)
  "Returns the value of a blob as a string."
  (if (blob-p blob)
      (if (empty-p (blob-raw blob))
	  (if (not (empty-p (blob-location blob)))
	      (read-file-to-string (blob-location blob)))
	  (blob-raw blob))
      blob))

(defun blob-ref-p (object)
  (if (listp object)
      (equalp (first object) :blob%)))

(defun blob-ref-values (blob-ref)
  (let ((values (second blob-ref)))
    (if (listp values)
	values
	(list :location values :file-type :text :file-ext "blob"))))

(defun read-blob (blob-ref-values)
  (let ((*read-eval* nil)
	(out (make-string-output-stream))
	(str ""))

    (with-open-file (in (getx blob-ref-values :location)
			:direction :input		     
			:if-does-not-exist nil)
      (when in
	(loop for line = (read-line in nil)
	   while line do (write-line line out) )
	(close in))
      (setf str (get-output-stream-string out))
      (close out))
    
    (make-blob
     :file-type (getx blob-ref-values :file-type)
     :file-ext (getx blob-ref-values :file-ext)
     :location (getx blob-ref-values :location)
     :raw str)))

(defun write-blob (file value)
  (let ((*read-eval* nil)
	(in (make-string-input-stream value)))
    
    (ensure-directories-exist file)
    (with-file-lock (file)
	(with-open-file (out file
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	  (when in
	    (loop for line = (read-line in nil)		 
	       while line do (write-line line out) )
	    (close in))
	  
	  (close out)))))
