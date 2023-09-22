(in-package :documents-impl)

(defmethod naive-impl:type-of-doc-element ((collection document-collection) element)
  (cond ((blob-p element)
         :blob)
        ((and (document-p element) (document-collection element))
         :reference)
        ((and (document-p element) (not (document-collection element)))
         :child-document)
        (t nil)))

;;TODO: Sort out blob paths once and for all!!!!
(defmethod naive-impl:persist-form ((collection document-collection) shard blob (element-type (eql :blob))
                                    &key root parent &allow-other-keys)
  (declare  (ignorable shard root parent))
  (let ((file (or (and (not (empty-p (blob-location blob)))
                       (getx blob :location))

                  (cl-fad:merge-pathnames-as-file
                   (pathname (location collection))
                   (make-pathname :directory
                                  (list :relative (frmt "~A" (getx blob :parent-accessor)))
                                  :name (frmt "~A" (hash parent))
                                  :type (getx blob :file-ext))))))

    (setf (getx blob :location) file)

    ;;TODO: move write to outside of parsing!!!!!
    (write-blob file (blob-raw blob))

    (list :blob%

          (list :file-type (getx blob :file-type)
                :file-ext (getx blob :file-ext)
                :location (getx blob :location)
                :parent-accessor (getx blob :parent-accessor)))))

(defmethod naive-impl:persist-form ((collection document-collection) shard document
                                    (element-type (eql :reference))
                                    &key root parent &allow-other-keys)
  (declare  (ignorable shard root parent))
  (list
   :universe (and (getx document :universe~)
                  (name (getx document :universe~)))
   :store (and (getx document :store~)
               (name (getx document :store~)))
   :collection (and (getx document :collection~)
                    (name (getx document :collection~)))
   :document-type (and (getx document :document-type~)
                       (name (getx document :document-type~)))
   :hash (document-hash document)
   :shard-mac (cl-naive-store.naive-core:document-shard-mac
               (document-collection document) document)
   :elements '(:reference% t)))

(defmethod naive-impl:persist-form ((collection document-collection) shard document
                                    (element-type (eql :child-document))
                                    &key root parent &allow-other-keys)
  (declare  (ignorable root) (ignorable parent))

  (list
   :document-type (and (getx document :document-type~)
                       (name (getx document :document-type~)))
   :hash (document-hash document)
   :elements (naive-impl:persist-parse collection
                                       shard
                                       (or (document-changes document)
                                           (document-elements document))
                                       nil
                                       :root root
                                       :parent document)))

(defmethod naive-impl:persist-form ((collection document-collection) shard document
                                    (element-type (eql :document))
                                    &key root parent &allow-other-keys)
  (declare  (ignorable root) (ignorable parent))

  (list
   :universe (and (getx document :universe~)
                  (name (getx document :universe~)))
   :store (and (getx document :store~)
               (name (getx document :store~)))
   :collection (and (getx document :collection~)
                    (name (getx document :collection~)))
   :document-type (and (or (getx document :document-type~)
                           (document-type collection))
                       (name (or (getx document :document-type~)
                                 (document-type collection))))

   :hash (document-hash document)
   :deleted-p (if (document-deleted-p document)
                  t
                  nil)
   :elements (naive-impl:persist-parse
              collection
              shard
              (or (document-changes document)
                  (document-elements document))
              nil
              :root document)))

(defmethod naive-impl:persist-parse ((collection document-collection) shard element doc
                                     &key root parent &allow-other-keys)

  (cond ((null element)
         (nreverse doc))
        ((consp (car element))
         (naive-impl:persist-parse
          collection
          shard
          (cdr element)
          (cons (naive-impl:persist-parse collection
                                          shard
                                          (car element)
                                          nil
                                          :root root
                                          :parent parent)
                doc)

          :root root
          :parent parent))
        (t
         (naive-impl:persist-parse collection
                                   shard
                                   (cdr element)
                                   (cons
                                    (if (naive-impl:type-of-doc-element collection (car element))
                                        (naive-impl:persist-form
                                         collection
                                         shard
                                         (car element)
                                         (naive-impl:type-of-doc-element collection (car element))
                                         :root root
                                         :parent parent)
                                        (car element))

                                    doc)
                                   :root root
                                   :parent parent))))

#|

;;TODO traverse in pairs and not in values!!!

(defgeneric map-doc (document fn)
(:documentation "Traverses the document applying the function to the values of the document and its childrens values recursively.

Then function must be of the following form.

(lambda (pair place-info)
;;Where pair is (:elment element-name :value value)
;;
;;parent is (:root root :element-name element-name :parent parent :current current)
root is the root doc passed to map-doc
parent is the source of the doc values being traversed
parent-element is the place in the parent where this traversal started
current is the document which this pair is from.)

"))

(defmethod map-doc ((document document) fn)
(labels (traverse (element)
(cond ((null element)
nil)
((document-p element) ;;reference-document
(mapcar fn
(document-values element))
element)
((atom element)
(apply fn element))
((consp element)
(mapcar
(lambda (child)
(if (document-p child)
(mapcar fn
(document-values child))
(apply fn child)))
document))
(t
document)))))

(defun map-doc (document))
|#

