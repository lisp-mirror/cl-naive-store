(in-package :cl-naive-items)

(defmethod getfx ((item item) field &key  &allow-other-keys)
  (let ((db-type (db-type-get-set field)))
    (getsfx db-type field item)))

(defmethod (setf getfx) (value (item item) field
			 &key &allow-other-keys)
   (let ((db-type (db-type-get-set field)))
     (setf (getsfx db-type field item) value)))

(defmethod (setf getsfx) (value (type (eql :item)) field item
			  &key &allow-other-keys)

  (let ((name (getf field :name))
	(final-val))
    (if (not (empty-p value))
	(if (item-p value)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec)))))
    (setf (getx item name) final-val)))


(defmethod (setf getsfx) (value (type (eql :contained-item)) field item   
			 &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (if (item-p value)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec))))
    (setf (getx item name) final-val)))

(defmethod (setf getsfx) (value (type (eql :collection-contained-item))
			  field item   &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (if (item-p value)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec))))
    (setf (getx item name) final-val)))


(defmethod getsfx ((type (eql :list-items)) field object &key &allow-other-keys)
  (cl-naive-data-types::getsfx* field object))

(defmethod (setf getsfx) (value (type (eql :list-items)) field object   
			  &key &allow-other-keys)
  (cl-naive-data-types::set-getsfx* field object value))
