(in-package :cl-naive-store)

;;TODO: add functionality to do sanitize when loading data for the first time.

(defgeneric sanitize-data-file (collection &key &allow-other-keys)
  (:documentation "This removes all the deleted data documents from a collection. When a collection is loaded only the active documents are loaded. Does this by simply writing those active documents out to a new file and then replacing the old file."))

(defmethod sanitize-data-file ((collection collection) &key &allow-other-keys)
  (let ((documents (query-data
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
    
    (when documents
      (dolist (document documents)
	(cl-naive-store::persist document
				 :file new-file
				 :new-file-p t))
      (fad:copy-file new-file
		     log-file
		     :overwrite t))))