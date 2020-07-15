(in-package :cl-naive-documents)

(defmethod getfx ((document document) element &key  &allow-other-keys)
  (let ((db-type (db-type-get-set element)))
    (getsfx db-type element document)))


(defmethod (setf getfx) (value (document document) element
			 &key &allow-other-keys)
  
  (let ((db-type (db-type-get-set element)))
     (setf (getsfx db-type element document) value)))

(defmethod (setf getsfx) (value (type (eql :document)) element (document document)
			  &key &allow-other-keys)

  (let ((name (getx element :name))
	(final-val))
    (if (not (empty-p value))
	(if (document-p value)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (digx element :db-type :data-spec)))))
    (setf (getx document name) final-val)))


(defmethod (setf getsfx) (value (type (eql :contained-document)) element (document document)   
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

(defmethod (setf getsfx) (value (type (eql :collection-contained-document))
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



(defmethod getsfx ((type (eql :lisp-code)) element document &key &allow-other-keys)
  (cl-naive-document-type-defs::getsfx* element document))

(defmethod getsfx ((type (eql :java-script)) element document &key &allow-other-keys)
  (cl-naive-document-type-defs::getsfx* element document))

(defmethod getsfx ((type (eql :css)) element document &key &allow-other-keys)
  (cl-naive-document-type-defs::getsfx* element document))

(defmethod getsfx ((type (eql :html)) element document &key &allow-other-keys)
  (cl-naive-document-type-defs::getsfx* element document))

(defmethod getsfx ((type (eql :text-blob)) element document &key &allow-other-keys)
  (cl-naive-document-type-defs::getsfx* element document))


;;BLOBS

(defmethod (setf getsfx) (value (type (eql :lisp-code)) element document
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
    (cl-naive-document-type-defs::set-getsfx* element document blob)))


(defmethod (setf getsfx) (value (type (eql :css)) element document
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
    (cl-naive-document-type-defs::set-getsfx* element document blob)))

(defmethod (setf getsfx) (value (type (eql :html)) element document
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
    (cl-naive-document-type-defs::set-getsfx* element document blob)))

(defmethod (setf getsfx) (value (type (eql :java-script)) element document
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
    (cl-naive-document-type-defs::set-getsfx* element document blob)))

(defmethod (setf getsfx) (value (type (eql :text-blob)) element document
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
    (cl-naive-document-type-defs::set-getsfx* element document blob)))
