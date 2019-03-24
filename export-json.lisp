(in-package :cl-naive-store)

(defun lambda-p (object)
  (and (listp object)
       (symbolp (first object))
       (string-equal (format nil "~A" (first object)) "lambda")))

(defun write-pair-json (pair stream first-p)
  (unless (or (blob-p (second pair))
	      (lambda-p (second pair)))
    (unless first-p
      (write-string ","  stream))
    
    (write (format nil "~A" (first pair))
	   :stream stream)
    (write-string " : "  stream)

    (if (and (second pair) (listp (second pair)))			    
	(treemap-json (second pair) stream t)
	(if (item-p (second pair))
	    (treemap-json (second pair) stream t)
	    (if (second pair)
		(if (symbolp (second pair))
		    (write (format nil "~S" (second pair)) :stream stream)
		    (write (second pair) :stream stream))
		(write "null" :stream stream))))
    
    (write-char #\Newline stream)))

(defun treemap-json (tree stream first-p)
  
  (cond ((null tree) nil)
	((item-p tree)
	 
	 (unless first-p
	   (write-string ","  stream))
	 
	 (write-string "{"  stream)
	 (write-pair-json (list :hash (format nil "~A" (item-hash tree)))
		     stream
		     t)
	 
	 (write-pair-json (list
		      :data-type (item-data-type tree))
		     stream
		     nil)
	 
	 (dolist (pair (parse-item tree))
	     (write-pair-json pair stream nil))
	 (write-string "}"  stream))
        ((consp tree)
	 
	 (write-string "[" stream)
	 (let ((first-pp t))
	   (mapcar (lambda (child)
		     (treemap-json child stream first-pp)
		     (setf first-pp nil))
		   tree))	 
	 (write-string "]" stream))
        (t tree)))


(defun item-list-to-json (item-list)
  (let ((out (make-string-output-stream))
	(str))
    (treemap-json item-list out t)
     (setf str (get-output-stream-string out))
     (close out)
     str))
