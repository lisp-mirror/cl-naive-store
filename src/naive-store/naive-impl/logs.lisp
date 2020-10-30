(in-package :naive-impl)

(defvar *time-zone* 0)

(defvar *short-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun short-month-name (n)
  (when (array-in-bounds-p *short-months* (1- n))
    (aref *short-months* (1- n))))

(defun format-date-time (year month day hour min sec
                        &optional timezone)
  (declare (ignore timezone))
  (format nil "~d ~a ~d ~@{~2,'0d~^:~}"
          day (short-month-name month) year hour min sec))

(defun format-universal-date-time (universal-date)
  (if (stringp universal-date)
        universal-date
        (multiple-value-bind (sec min hour day month year)
            (decode-universal-time
             (or universal-date (get-universal-time))
             *time-zone*)
          (format-date-time year month day hour min sec))))

(defparameter *std-lock* (bt:make-lock)
  "Used to lod STD to log debug messages safely between threads.")


(defparameter *break-on-debug-log* nil
  "Causes a break when logging errors of type :error and :warning.")

(defparameter *debug-log-p* t
  "Switches debug logging or off for debug-log")


(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory )
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defun debug-log (message &key file-p path args)

  (when *debug-log-p*
    (when *break-on-debug-log*
      (break "~A~%~S" message args))

    (if file-p
	(write-to-file
	 (cl-fad:merge-pathnames-as-file
	  (or path (get-temp))
	  (make-pathname :name "naive-store-debug"
			 :type "log"))
	 (format nil
		 "~A:~% Thread: ~A:~% Message: ~A~%Args: ~S~%Caller: ~A~%"
		 (format-universal-date-time (get-universal-time))
		 (bt:current-thread)
		 message
		 args
		 (second #+sbcl (sb-debug:list-backtrace)
                 #+ccl (ccl::backtrace-as-list)
                 #-(or sbcl ccl) nil)))
	(bt:with-lock-held (*std-lock*)
	  (format t
		  "~A:~% ~A~%~S~%"
		  (bt:current-thread)
		  message
		  args)))))

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

