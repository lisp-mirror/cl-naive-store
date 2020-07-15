(in-package :cl-naive-documents)

(defun lambda-p (document)
  (and (listp document)
       (symbolp (first document))
       (string-equal (format nil "~A" (first document)) "lambda")))

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
	(if (document-p (second pair))
	    (treemap-json (second pair) stream t)
	    (if (second pair)
		(if (symbolp (second pair))
		    (write (format nil "~S" (second pair)) :stream stream)
		    (write (second pair) :stream stream))
		(write "null" :stream stream))))
    
    (write-char #\Newline stream)))

(defun treemap-json (tree stream first-p)
  
  (cond ((null tree) nil)
	((document-p tree)
	 
	 (unless first-p
	   (write-string ","  stream))
	 
	 (write-string "{"  stream)
	 (write-pair-json (list :hash (format nil "~A" (document-hash tree)))
		     stream
		     t)
	 
	 (write-pair-json (list
		      :document-type (document-type tree))
		     stream
		     nil)
	 
	 (dolist (pair (parse-document tree))
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


(defun document-list-to-json (document-list)
  (let ((out (make-string-output-stream))
	(str))
    (treemap-json document-list out t)
     (setf str (get-output-stream-string out))
     (close out)
     str))
