(in-package :cl-naive-documents)


(defun write-pair-csv (pair stream first-p)
  (unless (or (blob-p (second pair))
	      (lambda-p (second pair)))
    (unless first-p
      (write-string "|"  stream))
    
    (write (format nil "~A" (first pair))
	   :stream stream)
    (write-string "|"  stream)

    (if (and (second pair) (listp (second pair)))			    
	(treemap-csv (second pair) stream nil)
	(if (document-p (second pair))
	    (treemap-csv (second pair) stream nil)
	    (if (second pair)
		(if (symbolp (second pair))
		    (write (format nil "~S" (second pair)) :stream stream)
		    (write (second pair) :stream stream))
		(write "null" :stream stream))))))

(defun treemap-csv (tree stream first-p) 
  (cond ((null tree) nil)
	((document-p tree)
	 
	 (unless first-p
	   (write-string "|"  stream))
	 
	 (write-pair-csv (list :hash (format nil "~A" (document-hash tree)))
		     stream
		     t)
	 
	 (write-pair-csv (list
		      :document-type (document-type tree))
		     stream
		     nil)
	 
	 (dolist (pair (parse-document tree))
	     (write-pair-csv pair stream nil)))
        ((consp tree)
	
	 (let ((first-pp t))
	   
	   (mapcar (lambda (child)
		     (when (and (not first-pp)
				first-p)
		       (write-char #\Newline stream))
		     (treemap-csv child stream first-p)
		     (setf first-pp nil))
		   tree)))
        (t tree)))

(defun document-list-to-csv (document-list)
  (let ((out (make-string-output-stream))
	(str))
    (treemap-csv document-list out t)
     (setf str (get-output-stream-string out))
     (close out)
     str)
  
  )
