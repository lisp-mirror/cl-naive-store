(in-package :cl-naive-documents)

(defclass document-collection (indexed-collection-mixin document-type-collection-mixin collection)
  ()
  (:documentation "Document collection class used to specialize on for cl-naive-documents."))

(defclass document-store (document-type-store-mixin store)
  ()
  (:default-initargs
   :collection-class 'document-collection
    )
  (:documentation "cl-naive-documents specialization of store."))

(defstruct document
  "A basic struct that represents a document object. A struct is used because there is meta data that we want to add to the actual document values and there is additional functionality like being able to know what has changed in the values during updates. 

store = The store that the document comes from.
collection = The collection that the document comes from.
type-def = The document type specification that describes this document.
hash = The hash/UUID that uniquely identifies this document
elements = The actual key value pairs of the doucument.
changes = Is used to store setf values when using getx the preffered accessor for values. This helps with comparing of values when persisting.
versions = older key value pairs that represent older versions of the document
deleted-p = indicates that the document was deleted.
persisted-p = indicates that the document has been peristed.
"
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

(defmethod murmurhash:murmurhash ((timestamp local-time:timestamp) &key)
  (murmurhash:murmurhash (let ((poes)) (local-time:format-timestring poes  timestamp) poes)))

;;TODO:Need to hunt down instances where this function can use instead of the more
;;verbose code lying around.
;;currently not used any where?
(defun document-of-type-p (document document-type)
  "Returns t if the document is of the data type."
  (string-equal document-type (document-type-def document)))

;;TODO: Consider removing these getx* thingies.
(defgeneric getxo (document element-name)
  (:documentation "Gets value ignoring any changes made to the document. IE old value."))

(defmethod getxo ((document document) element-name)
  (getx (document-elements document) element-name))

;;TODO: Is this still required because getx defaults to returning the new value??
(defgeneric getxn (document element-name)
  (:documentation "Gets changed value made to the document. IE new value."))

(defmethod getxn ((document document) element-name)  
  (getx (document-changes document) element-name))

(defmethod cl-getx:place-exists-p ((document document) element-name)
  (cl-getx:place-exists-p (document-elements document) element-name))

(defmethod exists-p ((document document) element-name)
  (cl-getx:place-exists-p document element-name))

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



