(in-package :cl-naive-documents)

(defmethod (setf getxe) (value (type (eql :document)) element (document document)
			  &key &allow-other-keys)

  (let ((name (getx element :name))
	(final-val))
    (if (not (empty-p value))
	(if (document-p value)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (digx element :db-type :data-spec)))))
    (setf (getx document name) final-val)))


(defmethod (setf getxe) (value (type (eql :contained-document)) element (document document)   
			 &key &allow-other-keys)
  (let ((name (getx element :name))
	(final-val))
    
    (if (document-p value)
	(setf final-val value)
	(if (not (empty-p value))
	    (error (frmt "~S is not of type ~A!" value
			   (digx element :db-type :data-spec)))
	  (setf final-val nil)))
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value (type (eql :collection-contained-document))
			  element (document document)   &key &allow-other-keys)
  (let ((name (getx element :name))
	(final-val))
    
    (if (document-p value)
	(setf final-val value)
	(if (not (empty-p value))
	    (error (frmt "~S is not of type ~A!" value
			   (digx element :db-type :data-spec)))
	  (setf final-val nil)))
    (setf (getx document name) final-val)))



(defmethod getxe ((type (eql :lisp-code)) document element &key &allow-other-keys)
  (cl-naive-document-type-defs::getxe* document element))

(defmethod getxe ((type (eql :java-script)) document element &key &allow-other-keys)
  (cl-naive-document-type-defs::getxe* document element))

(defmethod getxe ((type (eql :css)) document element &key &allow-other-keys)
  (cl-naive-document-type-defs::getxe* document element))

(defmethod getxe ((type (eql :html)) document element &key &allow-other-keys)
  (cl-naive-document-type-defs::getxe* document element))

(defmethod getxe ((type (eql :text-blob)) document element &key &allow-other-keys)
  (cl-naive-document-type-defs::getxe* document element))


;;BLOBS

(defmethod (setf getxe) (value (type (eql :lisp-code)) element document
			  &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(if (not parent-hash)
	    (error "Cannot create blob without parent-hash!")
	    (setf blob (make-blob  :file-type :text
				   :file-ext "lisp"
				   :location ""
				   :raw value                                   
				   :parent-accessor (getf element :name))
		  )))
    (cl-naive-document-type-defs::set-getxe* blob document element)))


(defmethod (setf getxe) (value (type (eql :css)) document element
			  &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(if (not parent-hash)
	    (error "Cannot create blob without parent-hash!")
	    (setf blob (make-blob  :file-type :text
				   :file-ext "css"
				   :location ""
				   :raw value                                   
				   :parent-accessor (getf element :name)))))
    (cl-naive-document-type-defs::set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :html)) document element
			  &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(if (not parent-hash)
	    (error "Cannot create blob without parent-hash!")
	    (setf blob (make-blob  :file-type :text
				   :file-ext "html"
				   :location ""
				   :raw value
				   :parent-accessor (getf element :name)))))
    (cl-naive-document-type-defs::set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :java-script)) document element
			  &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(if (not parent-hash)
	    (error "Cannot create blob without parent-hash!")
	    (setf blob (make-blob  :file-type :text
				   :file-ext "js"
				   :location ""
				   :raw value
				   :parent-accessor (getf element :name)))))
    (cl-naive-document-type-defs::set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :text-blob)) document element
			  &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(if (not parent-hash)
	    (error "Cannot create blob without parent-hash!")
	    (setf blob (make-blob  :file-type :text
				   :file-ext "txt"
				   :location ""
				   :raw value
				   :parent-accessor (getf element :name)))))
    (cl-naive-document-type-defs::set-getxe* blob document element)))
