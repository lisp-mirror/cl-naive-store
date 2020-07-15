(in-package :cl-naive-documents)

(defun parse-document (document)
  (naive-impl::plist-to-pairs (document-values document)))

(defun document-pair-to-plist (pair)
  (list (first pair)
	  (if (and (second pair) (listp (second pair)))			    
	      (documents-to-plist (second pair))
	      (if (document-p (second pair))
		  (documents-to-plist (second pair))
		  (if (blob-p (second pair))
		      (blob-raw (second pair))
		      (second pair))))))

(defun documents-to-plist (tree)
  (cond ((null tree) nil)
	((document-p tree)
	 (let ((values))

	   (setf values (append values
				(document-pair-to-plist
				 (list :hash (document-hash tree)))))
	   (setf values (append values
				(document-pair-to-plist
				 (list :document-type (document-type tree)))))
	   (dolist (pair (parse-document tree))
	     (setf values (append values (document-pair-to-plist pair))))
	   values))
	
        ((consp tree)
	 (mapcar (lambda (child)
		     (documents-to-plist child))
		 tree))
        (t tree)))

