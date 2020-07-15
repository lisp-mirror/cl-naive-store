(in-package :naive-impl)

(defmacro wrap-in-list ((stream &key descriptor) &body body)
  "Wraps write on stream in (...) or (descriptor ...),"
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

(defmacro wrap-in-loader ((stream loader-fn) &body body)
  "Wraps write on stream in #.(loader-fn ...)"
  (let ((stream% (gensym))
	(loader-fn% (gensym)))
    `(let ((,stream% ,stream)
	   (,loader-fn% ,loader-fn))      
       (write-char #\# ,stream%)
       (write-char #\. ,stream%)
       (write-char #\( ,stream%)
       (write ,loader-fn% :stream ,stream%)
       (write-char #\Space ,stream%)
       ,@body
       (write-char #\) ,stream%))))

(defgeneric write-object (object stream &key &allow-other-keys)
  (:documentation "Write an object to stream. Specialize this method if you want to influence how the object is physically represented in a file. Write-object's default behaviour is to not use *print-readable* so that store files are portable between cl implmentations. But write-object implementations supplied by cl-naive-store respects *print-readably*.

NOTES: 

Write object exists because there is no way to specialize print-object to override the cl implementation's handling of *print-readably* for strings, hashtables etc so we need an extra layer between us and the cl implementation to achieve portable readability.

IMPL NOTES:

Respect *print-readably*, use write-object in your specialization to help.
"))

(defmethod write-object (object stream &key &allow-other-keys)
  (if *print-readably*
      (prin1 object stream)
      (write object :stream stream)))

(defmethod write-object ((object list) stream &key &allow-other-keys)
  "Writes out a list form where each object in the list was printed by write-object."
  (wrap-in-list (stream :descriptor nil)
   (do* ((x object (cdr x))
	 (y '() (unless (endp x) (write-char #\Space stream))))   
	((endp x))
     (write-object (car x) stream))))

;;TODO: Do hash table test???
(defmethod write-object ((object hash-table) stream &key &allow-other-keys )
  "Writes out a list form of key value pairs representing a hash table."
  (if *print-readably*
      (prin1 object stream)
      (wrap-in-list (stream :descriptor :|hash-table|)
	(maphash (lambda (key value)               
		   (write-object (list :key key :object value) stream)
		   ;;TODO: figure out how to not write space out on last -- effeciently
		   (write-char #\Space stream))
		 object)
	stream)))
