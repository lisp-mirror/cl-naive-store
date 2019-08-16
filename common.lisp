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

