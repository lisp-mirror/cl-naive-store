(in-package :cl-naive-items)

(defun item-pair-to-plist (pair)
  (list (first pair)
	  (if (and (second pair) (listp (second pair)))			    
	      (items-to-plist (second pair))
	      (if (item-p (second pair))
		  (items-to-plist (second pair))
		  (if (blob-p (second pair))
		      (blob-raw (second pair))
		      (second pair))))))

(defun items-to-plist (tree)
  (cond ((null tree) nil)
	((item-p tree)
	 (let ((values))

	   (setf values (append values (item-pair-to-plist (list :hash (item-hash tree)))))
	   (setf values (append values (item-pair-to-plist (list :data-type (item-data-type tree)))))
	   (dolist (pair (parse-item tree))
	     (setf values (append values (item-pair-to-plist pair))))
	   values))
	
        ((consp tree)
	 (mapcar (lambda (child)
		     (items-to-plist child))
		 tree))
        (t tree)))

