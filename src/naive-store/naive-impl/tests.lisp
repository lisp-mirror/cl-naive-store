(defpackage :naive-impl-tests (:use :cl :naive-impl))
(in-package :naive-impl-tests)

(defparameter *tests* '())

(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory )
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defmacro run-test (name test-fn)
  (let ((name% (gensym))
	(test-fn% (gensym))
	(test-result% (gensym))
	(info% (gensym)))
    
    `(let ((,name% ,name)
	   (,test-fn% ,test-fn))
       
       (multiple-value-bind (,test-result% ,info%)
	   (funcall ,test-fn%)
	 (if ,test-result%
	     (push (list ,name% T) *tests*)
	     (push (list ,name% nil ,info%) *tests*))))))

(run-test 'empty-p		
	  (lambda ()
	    (let ((content (list nil "" " " "null"
				 (format nil "~A nil ~A~A~A"
					 #\Tab #\Space #\Newline #\Return) )))
	      
	      (values (every #'empty-p content)
		      (list (list nil (empty-p nil))
			    (list "" (empty-p ""))
			    (list " " (empty-p " "))
			    (list (format nil "~A nil ~A~A~A"
					  #\Tab #\Space #\Newline #\Return)
				  (empty-p
				   (format nil "~A nil ~A~A~A"
					   #\Tab #\Space #\Newline #\Return)))
			    (list "null" (empty-p "null")))))))

(run-test 'plist-to-values
	  (lambda ()
	    (let ((result (plist-to-values (list :test 1 :more 2))))	    
	      (values (equalp result
			      (list 1 2))
		      result))))

(run-test 'plist-to-pairs
	  (lambda ()
	    (let ((result (plist-to-pairs (list :test 1 :more 2))))
	      
	      (values
	       (equalp result
		       (list (list :test 1) (list :more 2)))
	       result))))

(run-test 'file-to-string
	  (lambda ()
	    (let ((path
		   (cl-fad:merge-pathnames-as-file
		    (get-temp)
		    (make-pathname :directory '(:relative "naive-impl")
				   :name "file"
				   :type "string")))
		  (content "Testing 1 2 3 ...")
		  (result ))
	      (ensure-directories-exist path)
	      (with-open-file (stream path :direction :output :if-exists :supersede)
		(write-string content stream))

	      (setf result (file-to-string path))
	      (values (equalp result (format nil "~A~%" content))
		      result))))


;;TODO: Expand test to actually check if lock was enforced.
(run-test 'with-file-lock
	  (lambda ()
	    (let* ((folder (cl-fad:merge-pathnames-as-file
			    (get-temp)
			    (make-pathname :directory '(:relative "naive-impl")
					   )))
		   (path
		    (cl-fad:merge-pathnames-as-file
                     folder
		     (make-pathname :name "lock-me"
				    :type "log")))
		   (lock (cl-fad:merge-pathnames-as-file
			   folder
			   (make-pathname :name "lock-me.log"
					  :type "lock")))
		   (lock-found-p nil))

	      (ensure-directories-exist path)
	      
	      (naive-impl:with-file-lock
	       (path)
	       (setf lock-found-p
		     (probe-file lock)))
	      
	      (values lock-found-p (format nil "~A not found." lock)))))

;;TODO: Expand test to check lock and write to file.
(run-test 'with-open-file-lock
	  (lambda ()
	    (let* ((folder (cl-fad:merge-pathnames-as-file
			    (get-temp)
			    (make-pathname :directory '(:relative "naive-impl")
					   )))
		   (path
		    (cl-fad:merge-pathnames-as-file
                     folder
		     (make-pathname :name "lock-me"
				    :type "log")))
		   (lock (cl-fad:merge-pathnames-as-file
			   folder
			   (make-pathname :name "lock-me.log"
					  :type "lock")))
		   (file-found-p nil))

	     ;; (ensure-directories-exist path)
	      (naive-impl:with-open-file-lock
		 (stream path :if-exists :supersede)
		 (write "Testing 1 2 3 ..." :stream stream))

	      (setf file-found-p  (probe-file path))
	      (values file-found-p (format nil "~A not found." lock)))))



;;TODO: This test is dependent on the implementation pretty printing.
;;need to do a proper test of this.
(run-test 'compose-parse.default
	  (lambda ()
	    (let ((sexp
		   '(:NAME "Piet" :SURNAME "Gieter" :ID 123
		     :WIFE  (:NAME "Sannie" :SURNAME "Gieter" :ID 321)
		     :PHOTO (:blob% :location "~/picture.pic")))
                  
		  (result))
                          
	      (setf result (naive-impl::compose-parse nil nil sexp nil))
              
	      (values (equalp (format nil "~S" result)
			      "(:NAME \"Piet\" :SURNAME \"Gieter\" :ID 123 :WIFE
 (:NAME \"Sannie\" :SURNAME \"Gieter\" :ID 321) :PHOTO
 #S(CL-NAIVE-STORE:BLOB
    :FILE-TYPE NIL
    :FILE-EXT NIL
    :LOCATION \"~/picture.pic\"
    :RAW \"\"
    :PARENT-ACCESSOR NIL))")
		      (format nil "~S" result)))))

(pprint *tests*)
