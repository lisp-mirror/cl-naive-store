(in-package :naive-impl)

(defparameter *break-on-error-log* nil
  "Causes a break when logging errors of type :error and :warning.")

;;TODO: Write unit test
(defun write-log (location type message)
  "Writes errors to location. 

Different Types are written to different files,
:error => error.err
:warning => warning.wrn
:debug => debug.dbl
:log => log.lg

Note:

Not writing stuf to .log files because that is what persist uses!!!.
"
  (when *break-on-error-log*
    (when (or (equal type :error)
	       (equal type :warning))     
      (break "~A: ~A" type message)))
  
  (write-to-file
       (cl-fad:merge-pathnames-as-file
	(pathname location)
	
	(cond ((equal type :error)	       
	       (make-pathname :name "error"
			      :type "err"))
	      
	      ((equal type :warning)	       
	       (make-pathname :name "warning"
			      :type "wrn"))
	      ((equal type :debug)       
	       (make-pathname :name "debug"
			      :type "dbl"))
	      (t
	       (make-pathname :name "log"
			      :type "lg"))))      
       message))

