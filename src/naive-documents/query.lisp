(in-package :cl-naive-documents)

;;TODO: Changed getx to return changes instead of values when available... dont know what subtle
;;bugs this will create in software currently using naive-store.... making this note as a reminder
;;for when strange thing start happending.
(defmethod getx ((document document) accessor &key &allow-other-keys)
  "getx for documents knows about some of the internals of an document structue so you can get the collection. 

Special accessors:

:hash = document-hash

The convention is to append %% to these accessors, for two reasons. First to show that they are special, accessing meta data not actual values of document. Second to avoid any name classes with actual data members.

:collection%% = document-collection
:store%% = document-store or (store collection)
:universe%% = (universe store)
:type%% = type
:elements%% = document-elements
:changes%% = document-changes
:versions%% = document-versions
:deleted-p%% = document-deleted-p

store and universe using getx."
  
  (cond ((equalp accessor :hash)
	 (document-hash document))
	((equalp accessor :collection%%)
	 (document-collection document))
	((equalp accessor :store%%)
	 (or
	  (document-store document)
	  (and (document-collection document)
	       (store (document-collection document)))))
	((equalp accessor :universe%%)
	 (and (document-collection document)
	      (store (document-collection document))
	      (universe (store (document-collection document)))))
	((equalp accessor :type-def%%)
	 (document-type-def document))
	((equalp accessor :elements%%)
	 (document-elements document))
	((equalp accessor :changes%%)
	 (document-changes document))
	((equalp accessor :deleted-p%%)
	 (document-deleted-p document))
	(t         
	 (or
	  (getf (document-changes document) accessor)
	  (getf (document-elements document) accessor)))))

(defmethod (setf getx) (value (document document) accessor
		  &key (change-control-p t) &allow-other-keys
						       )
  (cond ((equalp accessor :hash)
	 (setf (document-hash document) value))
	((equalp accessor :collection%%)
	 (setf (document-collection document) value))
	((equalp accessor :store%%)
	 (setf (document-store document) value))
	((equalp accessor :universe%%)
	 (error "Not allowed set universe%%."))
	((equalp accessor :type-def%%)
	 (setf (document-type-def document) value))
	((equalp accessor :elements%%)
	 (setf (document-elements document) value))
	((equalp accessor :changes%%)
	 (setf (document-changes document) value))
	((equalp accessor :deleted-p%%)
	 (setf (document-deleted-p document) value))
	(t
	 (when change-control-p    
	   (unless (document-changes document)
	     (setf (document-changes document) (copy-list (document-elements document))))    
	   (setf (getf (document-changes document) accessor) value))
	 
	(unless change-control-p
	  (setf (getf (document-elements document) accessor) value))))
  value)


(defmethod getx ((document document) (element cl-naive-document-types:element) &key &allow-other-keys)
  (let ((db-type (db-type-get-set element)))
    (getxe document element db-type)))

(defmethod (setf getx) (value (document document) (element cl-naive-document-types:element)
			 &key &allow-other-keys)
   (let ((db-type (db-type-get-set element)))
     (setf (getxe document element db-type) value)))

;;TODO: Is this still needed???
(defun naive-dig (place indicators)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (document-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (document-p val)
			 (document-values val)
			 val)))
    
    (if indicators
	(naive-dig next-place indicators)
	(if (document-p place)
	    (getx place indicator)
	    (getf place indicator)))))

(defun set-naive-dig (place indicators value)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (document-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (document-p val)
			 (if (document-changes val)
			     (document-changes val)
			     (setf (document-changes val)
				   (copy-list (document-values val))))
			 val)))
    (if indicators
	(if (document-p val)
	    (set-naive-dig next-place indicators value)
	    (setf (getf place indicator) 
		  (set-naive-dig next-place indicators value)))
	(if (document-p place)
	    (setf (getx place indicator) value)
	    (setf (getf place indicator) value)))
    place))


(defmethod digx ((place document) &rest indicators)
   (naive-dig place indicators))

(defmethod (setf digx) (value (place document) &rest indicators)
  (set-naive-dig place indicators value))

;;((:name arst :value arts) (:name ...))
(defun find-document-by-value (document-list element-values)
  (let ((exists nil))
    (dolist (document document-list)
      (dolist (element element-values)
	(if (equalp (getx document (getf element :name)) (getx element :value)) 
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(return-from find-document-by-value document)))
    exists))

(defun find-equalp-document (document document-list)
  (let ((exists nil))
    (dolist (list-document document-list)
      (when (equalp (document-values list-document) (document-values document))	
	(setf exists document-list)))
    exists))

