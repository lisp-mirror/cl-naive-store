(in-package :naive-impl)

(defgeneric type-of-doc-element (collection sexp)
  (:documentation "Reports if the sexp represents a special form."))

(defmethod type-of-doc-element (collection element)
  (declare (ignorable collection))
  (cond ((blob-p element)
         :blob)
        ((hash-table-p element)
         :hash-table)
        ((and (listp element)
              (atom (car element))
              (symbolp (car element))
              (cl-getx:getx element :reference%))
         :reference)

        (t nil)))

(defgeneric persist-form (collection shard element element-type &key &allow-other-keys)
  (:documentation "Convert a document element to its list representation.

IMPL NOTES:

specialize element type like this (element-type (eql :blob)). DONT specialize on object
type directly because that will break type-of-doc-element. If you specialize element you need
to supply your own implementation of type-of-doc-element as well."))

(defmethod persist-form (collection shard document (element-type (eql :document))
                         &key &allow-other-keys)
  (declare (ignorable collection shard))
  ;;TODO: WTF???????????????
  (break "persist-form :document")
  document)

;;TODO: Sort out blob paths once and for all!!!!
(defmethod persist-form (collection shard blob (element-type (eql :blob))
                         &key &allow-other-keys)
  (declare (ignorable collection shard))
  (write-blob (getx blob :location) (blob-raw blob))
  (list :blob%
        (list :file-type (getx blob :file-type)
              :file-ext (getx blob :file-ext)
              :location (getx blob :location)
              :parent-accessor (getx blob :parent-accessor))))

(defmethod persist-form (collection shard reference (element-type (eql :reference))
                         &key &allow-other-keys)
  (declare (ignorable collection shard))
  ;;TODO: to get references to work we need to add collection info to the document
  ;;when peristing. But not persist those for top level documents only references.
  ;;still deciding if it would be useful and if naive-documents should not be the only
  ;;thing that can handle references.
  (break "persist-form :reference")
  reference)

;;TODO: Deal with tests that is not just the funciton name
(defmethod persist-form (collection shard hash-table (element-type (eql :hash-table))
                         &key &allow-other-keys)
  (declare (ignorable collection shard))
  (list :|hash-table|
        :|hash-table-test| (hash-table-test hash-table)
        (maphash-collect
         (lambda (key value)
           (list :key key :object value))
         hash-table)))

;;Made this a seperate method so simple units tests can test basic parsing.
(defgeneric persist-parse (collection shard sexp doc &key &allow-other-keys)
  (:documentation "Transcribes document to list form for peristance."))

(defmethod persist-parse (collection shard sexp doc &key &allow-other-keys)
  (cond ((null sexp)
         (nreverse doc))
        ((atom sexp)
         sexp)
        ((consp (car sexp))
         (persist-parse collection shard (cdr sexp)
                        (cons (persist-parse collection
                                             shard
                                             (car sexp)
                                             nil)
                              doc)))
        (t
         (persist-parse collection shard (cdr sexp)
                        (cons
                         (if (type-of-doc-element collection (car sexp))
                             (persist-form collection
                                           shard
                                           (car sexp)
                                           (type-of-doc-element collection (car sexp)))
                             (car sexp))
                         doc)))))

(defgeneric persist-delete-document (collection shard document file &key &allow-other-keys)
  (:documentation "Marks document as deleted."))

(defmethod persist-delete-document (collection shard document file
                                    &key &allow-other-keys)
  (remove-document collection document :key shard)
  (setf (deleted-p document) t)
  (naive-impl:write-to-file file (naive-impl:persist-form
                                  collection
                                  shard
                                  document
                                  :document)))

