(in-package :cl-naive-store.utils)

(defun naive-document-to-sexp (document &key (versions-p t))
  "Converts a naive-documents:document to a sexp."

  (let ((dochash (make-hash-table :test #'equalp)))
    (labels ((parse-values (value)
               (if (listp value)
                   (loop for val in value
                         collect (parse-values val))
                   (if (cl-naive-store.naive-documents:document-p value)
                       (or (gethash (getx value :hash) dochash)
                           (naive-document-to-sexp value))
                       value))))

      (let ((sexp `(:universe ,(name
                                (universe
                                 (store
                                  (cl-naive-store.naive-documents:document-collection
                                   document))))
                    :store ,(name
                             (store
                              (cl-naive-store.naive-documents:document-collection
                               document)))
                    :collection ,(name
                                  (cl-naive-store.naive-documents:document-collection
                                   document))
                    :document-type
                    ,(name
                      (cl-naive-store.naive-documents:document-document-type
                       document))
                    :hash
                    ,(cl-getx:getx document :hash)
                    :elements
                    ,(loop for (key value) on (getx document :elements~)
                           by #'cddr
                           collect key
                           collect (parse-values value))

                    :versions
                    ,(when versions-p
                       (loop for version in (getx
                                             document :versions~)
                             collect (loop for (key value) on version
                                           by #'cddr
                                           collect key
                                           collect (parse-values value))))
                    :deleted-p ,(cl-naive-store.naive-documents:document-deleted-p
                                 document))))
        (setf (gethash (getx document :hash) dochash) sexp)
        sexp))))

(defun naive-documents-to-sexps (documents)
  "Used to prepare docs to be sent over HTTP or shared with some other
system that does not have access to cl-naive-store."

  (loop for document in documents
        collect
        (naive-document-to-sexp document)))

(defun existing-document% (collection document)
  (or (and (cl-naive-store.naive-indexed:hash document)
           (cl-naive-store.naive-indexed:index-lookup-hash
            collection
            (cl-naive-store.naive-indexed:hash document)))
      (let ((key-values
              (key-values collection document)))

        (cl-naive-store.naive-indexed::try-better-value-match
         collection
         (cl-naive-store.naive-indexed:index-lookup-values
          collection key-values)
         key-values))))

(defun sexp-to-naive-document (multiverse sexp)
  "This function converts a sexp to a naive-documents:document. What it does not do is
sync the info with any existing documents in the collection. You will
have to do a perstist-document (or add-document if not a persisted
collection) on this document to sync with any existing documents in
the collection.

If you intend to use add-document just note that it does not sync any
child documents only persist-document does that as well. So if you are
not carefull you can end up with more than one document with the same
UUID and different values for child documents.

The function does check for existing-documents to at least make sure
that the same UUID is used and any child documents that do not have UUID's
already are supplied with UUIDs. Child documents that are reference
documents are also checked for consitent UUID.

The conversion is done on the assumption you have not trashed the document
beyond repair when you possibly hacked the sexp.

"
  ;;Using dochash to not convert child docs that have already been
  ;;converted.  For example country-town in two seperate (physical
  ;;postal) address documents in addresses could be exactly the same
  ;;and are assumed the to be the same.
  (let ((dochash (make-hash-table :test #'equalp)))
    (labels ((parse-values (value)
               (if (listp value)
                   (if (member :hash value)
                       (or (gethash (getx value :hash) dochash)
                           (sexp-to-naive-document multiverse value))
                       (loop for val in value
                             collect (parse-values value)))
                   value)))

      (let* ((universe (and (getx sexp :collection)
                            (cl-naive-store.naive-core:get-multiverse-element
                             :universe multiverse (getx sexp :universe))))
             (store (and (getx sexp :collection)
                         universe
                         (cl-naive-store.naive-core:get-multiverse-element
                          :store universe (getx sexp :store))))
             (collection (and (getx sexp :collection)
                              store
                              (cl-naive-store.naive-core:get-multiverse-element
                               :collection store (getx sexp :collection))))
             (new-doc (apply 'cl-naive-store.naive-documents:make-document
                             `(:hash ,(getx sexp :hash)
                               :store ,store
                               :collection ,collection
                               :deleted-p ,(getx sexp :deleted-p)
                               :elements ,(loop for (key value) on (getx sexp :elements)
                                                by #'cddr
                                                collect key
                                                collect (parse-values value)))))
             (existing-document (and (getx sexp :collection)
                                     collection
                                     (existing-document%
                                      collection
                                      new-doc))))

        (unless (cl-naive-store.naive-indexed:hash new-doc)
          (if existing-document
              (setf (cl-naive-store.naive-indexed:hash new-doc)
                    (cl-naive-store.naive-indexed:hash existing-document))
              (setf (cl-naive-store.naive-indexed:hash new-doc)
                    (uuid:make-v4-uuid))))

        (setf (gethash (cl-naive-store.naive-indexed:hash new-doc) dochash) new-doc)

        new-doc))))
