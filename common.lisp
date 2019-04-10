(in-package :cl-naive-store)

(declaim (inline frmt))
(defun frmt (control-string &rest args)
  "Short hand for (format nil ..)."
  (apply #'format nil control-string args))

(defun trim-whitespace (string)
  "Removes white spaces from a string."
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))

(defun empty-p (value)
  "Checks if value is null or an empty string."
  (or
   (not value)
   (null value)
   (equal value "")
   (if (stringp value)
       (or (string-equal value "NIL")
	   (string-equal value "NULL")))
   (equal (trim-whitespace (princ-to-string value)) "")))

;;TODO: There must be a lisp function that already does this
(defun plist-to-values (values)
  "Returns the values of a plist."
  (loop for (a b) on values by #'cddr 
     :collect b))

(defun plist-to-value-pairs (values)
  "Returns a list of key value pairs of a plist."
  (loop for (a b) on values by #'cddr 
     :collect (list a b)))

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
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      ;;*print-readably* set to nil so that sbcl writes strings out strings as simple strings.
      (let (*print-readably*) 
	(write-object object out)))
    (close out)))

(defun write-list-to-file (file list &key (if-exists :append))
  
  (with-open-file (out file
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      ;*print-readably* set to nil so that sbcl writes strings out strings as simple strings.
      (let ((*print-readably*))
	(dolist (object list)
	  (write-object object out)))
      (close out))))

(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Writes a data object to file. This method is where database like tasks like 
checking for duplicates etc should be done. This is also where reference objects are converted
to a reference% marker instead of writing out the actual object. Perist is also used to write 
naive-store structural elements like data-types and collections to file."))

(defmethod persist ((list list) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes a list of data objects to a file as individual items."
  (write-list-to-file list
		      file
		      :if-exists if-exists))

(defun dig-down (place indicators)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))
    (if indicators
	(dig-down next-place indicators)
	next-place)))

(defun set-dig-down (place indicators value)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))  
    (if indicators	
	(setf (getf place indicator) 
	      (set-dig-down next-place indicators value))
	(setf (getf place indicator) value))
    place))

(defun dig (place &rest indicators)
  "Hierarchical getf."
  (dig-down place indicators))

(defun (setf dig) (value place &rest indicators)
  ;;When digx setf is called I cant use apply because &rest params causes extra list wrapper
  (if (and (listp indicators) (listp (first indicators)))
      (set-dig-down place (first indicators) value)
      (set-dig-down place indicators value)))

(defgeneric getx (object field-name)
  (:documentation "Returns the value of an field in a data object. By using getx and digx instead of 
accessing, object values directly the user has the opportunity to change the underlying structure/type of 
the object without having to rewrite a lot of code."))

(defmethod getx (object field-name)
  (getf object field-name))

(defgeneric (setf getx) (value item field-name &key &allow-other-keys)
  (:documentation "Sets the value of an field in an object."))

(defmethod (setf getx) (value object field-name &key &allow-other-keys)
  (setf (getf object field-name) value))

(defgeneric digx (place &rest indicators)
  (:documentation "Returns the value of an field in a hierarchical data object. By using getx and digx
 instead of accessing, object values directly the user has the opportunity to change the underlying 
structure of the object without having to rewrite a lot of code."))

(defmethod digx (place &rest indicators)
  (apply 'dig place indicators))

(defgeneric (setf digx) (value place &rest indicators))

(defmethod (setf digx) (value place &rest indicators)
  (setf (dig place indicators) value))

(defgeneric exists-p (object field-name)
  (:documentation "Returns t if the data-object has such a field."))

(defmethod exists-p (object field-name)
  (get-properties object (list field-name)))
