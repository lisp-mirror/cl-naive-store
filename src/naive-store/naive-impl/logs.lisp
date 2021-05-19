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

(defvar *now* (cons 0 "00000000T000000Z"))
(defun iso-timestamp-now ()
  (let ((now (get-universal-time)))
    (if (= now (car *now*))
        (cdr *now*)
        (setf (car *now*) now
              (cdr *now*)
              (multiple-value-bind (se mi ho da mo ye) (decode-universal-time now 0)
                (format nil "~4,0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
                        ye mo da ho mi se))))))

(defparameter *std-lock* (bt:make-lock)
  "Used to lod STD to log debug messages safely between threads.")

(defparameter *break-on-debug-log* nil
  "Causes a break when logging errors of type :error and :warning.")

(defparameter *debug-log-p* nil
  "Switches debug logging or off for debug-log")

(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory)
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defun debug-log (format-control-string &rest arguments-and-keys)
  ;; arguments-and-keys may end with [:file-p f] [:path p].
  (let ((file-p    nil)
	(path      nil)
	(arguments nil)
	(k         (reverse arguments-and-keys)))
    (loop
      :do (case (second k)
	    (:file-p (setf file-p (pop k)) (pop k))
	    (:path   (setf path   (pop k)) (pop k))
	    (otherwise (loop-finish))))
    (setf arguments (reverse k))

    (when *debug-log-p*
      (when *break-on-debug-log*
	(break "~?" format-control-string arguments))

      (if file-p
	  (write-to-file
	   (cl-fad:merge-pathnames-as-file
	    (or path (get-temp))
	    (make-pathname :name "naive-store-debug"
			   :type "log"))
	   (format nil
		   "~A:~% Thread: ~A:~% Message: ~?~% Caller: ~A~%"
		   (iso-timestamp-now) ; (format-universal-date-time (get-universal-time))
		   (bt:thread-name (bt:current-thread))
		   format-control-string arguments
		   (second #+sbcl (sb-debug:list-backtrace)
			   #+ccl (ccl::backtrace-as-list)
			   #-(or sbcl ccl) nil)))

	  (bt:with-lock-held (*std-lock*)
	    (format *trace-output*
		    "~&~A:~A:~?~%"
		    (iso-timestamp-now)
		    (bt:thread-name (bt:current-thread))
		    format-control-string arguments)
	    (force-output *trace-output*))))))

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

