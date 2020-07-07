(in-package :cl-naive-store)

;;TODO: add functionality to do sanitize when loading data for the first time.

(defgeneric sanitize-data-file (collection &key &allow-other-keys)
  (:documentation "This removes all the deleted data objects from a collection. When a collection is loaded only the active objects are loaded. Does this by simply writing those active objects out to a new file and then replacing the old file."))

(defmethod sanitize-data-file ((collection collection) &key &allow-other-keys)
  (let ((objects (query-data
		  collection))
	(log-file (cl-fad:merge-pathnames-as-file
		   (pathname (location collection))
		   (make-pathname :name (name collection)
				  :type "log")))
	(new-file (cl-fad:merge-pathnames-as-file
		   (pathname (location collection))
		   (make-pathname :name (name collection)
				  :type "new")))
	(old-file (cl-fad:merge-pathnames-as-file
		   (pathname (location collection))
		   (make-pathname :name (name collection)
				  :type "old")))
	(old-old-file (cl-fad:merge-pathnames-as-file
		   (pathname (location collection))
		   (make-pathname :name (name collection)
				  :type "old.old"))))
    (when (probe-file
	   old-file)
      (fad:copy-file old-file old-old-file :overwrite t))

    (fad:copy-file log-file old-file :overwrite t)
    
    (when objects
      (dolist (object objects)
	(cl-naive-store::persist object
				 :file new-file
				 :new-file-p t))
      (fad:copy-file new-file
		     log-file
		     :overwrite t))))
