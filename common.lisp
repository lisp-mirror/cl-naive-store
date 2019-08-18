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
the object without having to rewrite/change a lot of code."))

(defmethod getx (object field-name)
  (getf object field-name))

(defgeneric (setf getx) (value item field-name &key &allow-other-keys)
  (:documentation "Sets the value of an field in an object."))

;;The innards of this puppy is courtesy of @stassats, thanx dewd. This does not mean
;;that he approves of getx or this library! ;)
(defmethod (setf getx) (value object field-name &key &allow-other-keys)
  "This implementation of getx is basically (setf getf) but it cannot handle object = nil"
  (declare (cons object))
  (loop for cons = object then (cddr cons)
        for (key nil . rest) = cons
        when (eq key field-name)
        do (setf (cadr cons) value)
           (return)
        unless rest
        do (setf (cddr cons) (list field-name value))
       (return))
  value)

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

