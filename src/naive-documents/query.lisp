(in-package :cl-naive-store.naive-documents)

(defmethod getx ((document document) accessor &key &allow-other-keys)
  "Usually getx will return the value of an object/place attribute/key. However when we work with documents we would like to get to the actual element values instead, for convenience sake.

Because we mangled getx to give us element values we need a way to get to the original attributes of a document, we do this with the following accessors

:hash = document-hash (remember that you can not use :hash as an element descriptor it is reserved for cl-naive-store)

The convention is to append %% to these accessors, for two reasons. First to show that they are special, accessing meta data not actual values of document. Second to avoid any name clashes with actual data members.

:collection~ = document-collection
:store~ = document-store or (store collection)
:universe~ = document-universe or (universe store)
:document-type~ = document-document-type
:elements~ = document-elements
:changes~ = document-changes
:versions~ = document-versions
:deleted-p~ = document-deleted-p"
  (case accessor
    (:hash

     (hash document))
    (:collection~
     (document-collection document))
    (:store~
     (or
      (document-store document)
      (and (document-collection document)
           (store (document-collection document)))))
    (:universe~
     (or (document-universe document)
         (and (document-collection document)
              (store (document-collection document))
              (universe (store (document-collection document))))))
    (:document-type~
     (let ((doc-type (or
                      (document-document-type document)
                      (and (document-collection document)
                           (document-type (document-collection document))))))
       (when doc-type
         (if (stringp doc-type)
             (if (document-collection document)
                 (get-multiverse-element
                  :document-type
                  (store (document-collection document))
                  doc-type)
                 doc-type)
             doc-type))))
    (:elements~
     (document-elements document))
    (:changes~
     (document-changes document))
    (:versions~
     (document-versions document))
    (:deleted-p~
     (document-deleted-p document))
    (otherwise
     (or
      (getx (document-changes document) accessor)
      (getx (document-elements document) accessor)))))

(defparameter *change-control-p* t
  "Set change tracking when using setf getx for document values.
By default it is on.")

(defmethod (setf getx) (value (document document) accessor
                        &key (change-control-p *change-control-p*)
                        ;;(use-element-definition-p)
                        &allow-other-keys)

  (case accessor
    (:hash
     (setf (hash document) value))
    (:collection~
     (setf (document-collection document) value))
    (:store~
     (setf (document-store document) value))
    (:universe~
     (setf (document-universe document) value))
    (:document-type~
     (setf (document-document-type document) value))
    (:elements~
     (setf (document-elements document) value))
    (:changes~
     (setf (document-changes document) value))
    (:deleted-p~
     (setf (document-deleted-p document) value))
    (otherwise
     (when change-control-p
       (unless (document-changes document)
         (setf (document-changes document)
               (copy-list (document-elements document))))
       (setf (getx (document-changes document) accessor) value))

     (unless change-control-p
       (setf (getx (document-elements document) accessor) value))))
  value)

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
      (when (equalp (or (document-changes list-document)
                        (document-values list-document))
                    (or (document-changes document)
                        (document-values document)))

        (setf exists document-list)))
    exists))

