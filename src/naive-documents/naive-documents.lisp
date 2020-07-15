(in-package :cl-naive-documents)

(defclass document-collection (indexed-collection-mixin document-type-collection-mixin collection)
  ())

(defclass document-store (document-type-store-mixin store)
  ((collection-class :initarg :collections-class
		:accessor collection-class
		:initform 'document-collection
		:allocation :class
		:documentation "Then class that should be used to make collection documents.")))

(defstruct document
  "Data is loaded into these structures from files. Changes slot is used to store setf values when using getx the preffered accessor for values. This helps with comparing of values when persisting."
  store
  collection
  type-def
  hash
  elements
  changes
  versions
  deleted-p
  persisted-p)

(defmethod hash ((document document))
  (frmt "~A" (document-hash document)))

(defmethod (setf hash) (value (document document))
  (setf (document-hash document) (frmt "~A" value)))

(defun document-list-p (document)
  "Returns t if the document is a list of type document."
  (and (listp document)
       (document-p (first document))))

(defmethod document-values ((document document))
  (document-elements document))

(defmethod murmurhash:murmurhash ((document document) &key)
  (murmurhash:murmurhash (hash document)))

;;TODO:Need to hunt down instances where this function can use instead of the more
;;verbose code lying around.
;;currently not used any where?
(defun document-of-type-p (document document-type)
  "Returns t if the document is of the data type."
  (string-equal document-type (document-type document)))

;;TODO: Consider removing these getx* thingies.
(defgeneric getxo (document element-name)
  (:documentation "Gets value ignoring any changes made to the document. IE old value."))

(defmethod getxo ((document document) element-name)
  (getx (document-elements document) element-name))

(defgeneric getxn (document element-name)
  (:documentation "Gets changed value made to the document. IE new value."))

(defmethod getxn ((document document) element-name)  
  (getx (document-changes document) element-name))

(defmethod exists-p ((document document) element-name)
  (get-properties (document-elements document) (list element-name)))

(defun key-values%% (keys values)
  (let ((keys-values))
    (dolist (key keys)     
      (if (document-p (getx values key))
	    (push (list key (document-hash (getx values key))) keys-values)
	    (push (list key (getx values key)) keys-values)))
    (nreverse keys-values)))

(defmethod key-values ((collection document-collection) document &key &allow-other-keys)
  (if (keys collection)
      (key-values%% (keys collection) (document-elements document))))


(defmethod indexed-impl:index-values ((collection document-collection) document
				      &key &allow-other-keys)
  (let ((index-values))
    (dolist (index (indexes collection))
      (push
       (loop for (a b) on (document-elements document) by #'cddr
	  when (find a index :test 'equalp)
	  :collect (list a b))
       index-values))
    index-values))