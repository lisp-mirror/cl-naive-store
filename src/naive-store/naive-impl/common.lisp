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

