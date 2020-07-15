(in-package :cl-naive-store)

(defstruct blob
  "Represents a unit of data that is large enough to warent its own file or would cause reading problems for the default naive-store file layout which is essentially plists."
  file-type
  file-ext
  location
  raw
  parent-accessor)

(defmethod getx ((blob blob) accessor &key &allow-other-keys)
  "getx for blobs"
  (cond ((equalp accessor :file-type)
	 (blob-file-type blob))
	((equalp accessor :file-ext)
	 (blob-file-ext blob))
	((equalp accessor :location)
	 (blob-location blob))
	((equalp accessor :raw)
	 (blob-raw blob))
	((equalp accessor :parent-accessor)
	 (blob-parent-accessor blob))
        (t
	 nil)))

(defmethod (setf cl-getx) (value (blob blob) accessor &key &allow-other-keys)
  (cond ((equalp accessor :file-type)
	 (setf (blob-file-type blob) value))
	((equalp accessor :file-ext)
	 (setf (blob-file-ext blob) value))
	((equalp accessor :location)
	 (setf (blob-location blob) value))
	((equalp accessor :raw)
	 (setf (blob-raw blob) value))
	((equalp accessor :parent-accessor)
	 (setf (blob-parent-accessor blob) value))
        (t
	 nil))
  value)

(defmethod reference-form ((blob blob))
  "Creates a persistable form of the blob reference info."
  (list :blob%
	(list :file-type (blob-file-type blob)
	      :file-type (blob-file-ext blob)
	      :location (blob-location blob))))

(defun blob-string-value (blob)
  "Returns the value of a blob as a string."
  (if (blob-p blob)
      (if (naive-impl::empty-p (blob-raw blob))
	  (if (not (naive-impl::empty-p (blob-location blob)))
	      (naive-impl:file-to-string (blob-location blob)))
	  (blob-raw blob))
      blob))

(defun blob-ref-p (document)
  (if (listp document)
      (equalp (first document) :blob%)))

(defun blob-ref-values (blob-ref)
  (let ((values (second blob-ref)))
    (if (listp values)
	values
	(list :location values :file-type :text :file-ext "blob"))))

(defun read-blob (blob-ref-values)
  "Reads the raw blob contents from file."
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
     :raw str     
     :parent-accessor (getx blob-ref-values :parent-accessor))))

(defun write-blob (file value)
  "Wrties the raw blob contents to file."
  (let ((*read-eval* nil)
	(in (make-string-input-stream value)))
    
    (ensure-directories-exist file)
    (naive-impl:with-file-lock (file)
	(with-open-file (out file
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	  (when in
	    (loop for line = (read-line in nil)		 
	       while line do (write-line line out) )
	    (close in))
	  
	  (close out)))))
