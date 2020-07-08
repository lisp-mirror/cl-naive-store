(in-package :cl-naive-items)

(defclass item-collection (indexed-collection-mixin data-type-collection-mixin collection)
  ())

(defclass item-store (data-type-store-mixin store)
  ((collection-class :initarg :collections-class
		:accessor collection-class
		:initform 'item-collection
		:allocation :class
		:documentation "Then class that should be usedS to make collection objects.")))

(defstruct item
  "Data is loaded into these structures from files. Changes slot is used to store setf values when using getx the preffered accessor for values. This helps with comparing of values when persisting."
  store
  collection
  data-type
  hash
  values
  changes
  versions
  deleted-p
  persisted-p)

(defmethod hash ((item item))
  (frmt "~A" (item-hash item)))

(defmethod (setf hash) (value (item item))
  (setf (item-hash item) (frmt "~A" value)))

(defun item-list-p (object)
  "Returns t if the object is a list of type item."
  (and (listp object)
       (item-p (first object))))

(defmethod object-values ((object item))
  (item-values object))

(defmethod murmurhash:murmurhash ((object item) &key)
  (murmurhash:murmurhash (hash object)))

;;TODO:Need to hunt down instances where this function can use instead of the more
;;verbose code lying around.
;;currently not used any where?
(defun item-of-type-p (item data-type)
  "Returns t if the item is of the data type."
  (string-equal data-type (item-data-type item)))

;;TODO: Consider removing these getx* thingies.
(defgeneric getxo (item field-name)
  (:documentation "Gets value ignoring any changes made to the item. IE old value."))

(defmethod getxo ((item item) field-name)
  (getx (item-values item) field-name))

(defgeneric getxn (item field-name)
  (:documentation "Gets changed value made to the item. IE new value."))

(defmethod getxn ((item item) field-name)  
  (getx (item-changes item) field-name))

(defmethod exists-p ((item item) field-name)
  (get-properties (item-values item) (list field-name)))

(defun key-values%% (keys values)
  (let ((keys-values))
    (dolist (key keys)     
      (if (item-p (getx values key))
	    (push (list key (item-hash (getx values key))) keys-values)
	    (push (list key (getx values key)) keys-values)))
    (nreverse keys-values)))

(defmethod key-values ((collection item-collection) object &key &allow-other-keys)
  (if (keys collection)
      (key-values%% (keys collection) (object-values object))))
