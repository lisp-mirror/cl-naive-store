(in-package :cl-naive-items)


;;TODO: Changed getx to return changes instead of values when available... dont know what subtle
;;bugs this will create in software currently using naive-store.... making this note as a reminder
;;for when stranges thing start happending.

(defmethod getx ((item item) field-name)
  (or
   (getf (item-changes item) field-name)
   (getf (item-values item) field-name)))

(defmethod (setf getx) (value (item item) field-name &key (change-control-p t))
  (when change-control-p    
    (unless (item-changes item)
      (setf (item-changes item) (copy-list (item-values item))))
    
    (setf (getf (item-changes item) field-name) value))
  (unless change-control-p
    (setf (getf (item-values item) field-name) value)))

(defun naive-dig (place indicators)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (item-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (item-p val)
			 (item-values val)
			 val)))
    
    (if indicators
	(naive-dig next-place indicators)
	(if (item-p place)
	    (getx place indicator)
	    (getf place indicator)))))

(defun set-naive-dig (place indicators value)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (item-p place)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (item-p val)
			 (if (item-changes val)
			     (item-changes val)
			     (setf (item-changes val)
				   (copy-list (item-values val))))
			 val)))
    (if indicators
	(if (item-p val)
	    (set-naive-dig next-place indicators value)
	    (setf (getf place indicator) 
		  (set-naive-dig next-place indicators value)))
	(if (item-p place)
	    (setf (getx place indicator) value)
	    (setf (getf place indicator) value)))
    place))


(defmethod digx ((place item) &rest indicators)
   (naive-dig place indicators))

(defmethod (setf digx) (value (place item) &rest indicators)
  (set-naive-dig place indicators value))



;;((:name arst :value arts) (:name ...))
(defun find-item-by-value (item-list field-values)
  (let ((exists nil))
    (dolist (item item-list)
      (dolist (field field-values)
	(if (equalp (getx item (getf field :name)) (getf field :value)) 
	    (push t exists)
	    (push nil exists)))
      (unless (position nil exists)
	(return-from find-item-by-value item)))
    exists))

(defun find-equalp-item (item item-list)
  (let ((exists nil))
    (dolist (list-item item-list)
      (when (equalp (item-values list-item) (item-values item))	
	(setf exists item-list)))
    exists))

