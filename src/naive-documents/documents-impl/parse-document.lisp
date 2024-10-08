(in-package :documents-impl)

(defun document-values-p (list)
  "Checks if plist contains :elements keyword which would indicate the plist represents an document."
  (find :elements list :test #'equalp))

(defun ensure-document-type (store document-type)
  (cond ((stringp document-type)
         (let ((doc-type (or (get-multiverse-element :document-type store document-type)
                             document-type)))
           ;; (setf (gethash `(:universe doc-type) *multi-elements-cache*) doc-type)
           doc-type))
        (t document-type)))

(defmethod naive-impl:type-of-sexp ((collection document-collection) document-form)
  (cond ((and (listp document-form)
              (equalp (first document-form) :blob%))
         :blob)
        ((and (listp document-form)
              (atom (first document-form))
              (symbolp (first document-form))
              (> (length document-form) 1)
              (document-values-p document-form)
              (not (digx document-form :elements :reference%)))
         :child-document)
        ((and (listp document-form)
              (atom (first document-form))
              (symbolp (first document-form))
              (> (length document-form) 1)
              (document-values-p document-form)
              (digx document-form :elements :reference%))
         :reference)

        (t
         (when (or
                (find :type document-form)
                (find :document-type document-form))
           (naive-impl:write-log
            (location (universe (store collection)))
            :error (list "Parsing is missing a child or reference ~%~A"  document-form)))

         nil)))

(defmethod naive-impl:compose-special ((collection document-collection) shard sexp
                                       (type (eql :document))
                                       &key (handle-duplicates-p t) &allow-other-keys)

  (naive-impl::debug-log "docs:Compose-special :document ~A" (name collection))
  (let* ((resolved-values  (naive-impl:compose-parse
                            collection
                            shard
                            (digx sexp :elements)
                            nil))

         (existing-document (index-lookup-hash
                             collection
                             (digx sexp :hash)
                             :shards (if shard (list shard))))
         (final-document))

    (naive-impl::debug-log "? docs:Compose-special :document ~A" (name collection))

    (if (getx sexp :deleted-p)
        (when existing-document
          (remove-document collection existing-document :shard shard))
        (if existing-document
            (progn
              (unless (equalp (document-elements existing-document) resolved-values)
                ;;(break "push version ~S~%~S"
                ;;       (document-elements existing-document)
                ;;       resolved-values)
                (push (document-elements existing-document)
                      (document-versions existing-document))
                (setf (document-elements existing-document) resolved-values))
              (setf final-document existing-document))
            (progn
              (setf final-document
                    (make-document
                     :universe (ensure-universe
                                (multiverse (universe (store collection)))

                                (or (when (getx sexp :universe)
                                      (if (and (universe (store collection))
                                               (equalp
                                                (name (universe (store collection)))
                                                (getx sexp :universe)))
                                          (universe (store collection))
                                          (getx sexp :universe)))
                                    (universe (store collection))))
                     :store (ensure-store
                             (universe (store collection))
                             (or (getx sexp :store)
                                 (store collection)))
                     :collection (ensure-collection
                                  (store collection)
                                  (or (getx sexp :collection)
                                      collection))
                     :document-type
                     (ensure-document-type
                      (store collection)
                      (or
                       (getx sexp :document-type)
                       (cl-naive-store.document-types:document-type collection)))

                     :hash (frmt "~A" (getx sexp :hash))
                     :elements resolved-values))
              (add-document collection final-document
                            :handle-duplicates-p handle-duplicates-p))))

    (naive-impl::debug-log "END docs:Compose-special :document ~A" (name collection))

    final-document))

(defmethod naive-impl:compose-special ((collection document-collection) shard
                                       sexp (type (eql :child-document))
                                       &key handle-duplicates-p &allow-other-keys)
  (declare (ignore handle-duplicates-p))

  (let* ((ref-universe (ensure-universe
                        (multiverse (universe (store collection)))
                        ;;TODO: This is done for backwards
                        ;;compatibility and should be removed some
                        ;;time.
                        (or (when (getx sexp :universe)
                              (if (and (universe (store collection))
                                       (equalp
                                        (name (universe (store collection)))
                                        (getx sexp :universe)))
                                  (universe (store collection))
                                  (getx sexp :universe)))
                            (universe (store collection)))))
         (ref-store (ensure-store
                     ref-universe
                     (or (getx sexp :store)
                         (store collection))))
         (ref-collection (ensure-collection
                          ref-store
                          (getx sexp :collection))))
    (make-document
     :document-type
     (ensure-document-type
      ref-store
      (or
       (getx sexp :document-type)
       (cl-naive-store.document-types:document-type ref-collection)))
     :document-type (ensure-document-type
                     (store collection)
                     (digx sexp :document-type))
     :hash (frmt "~A" (getx sexp :hash))
     :elements (naive-impl:compose-parse collection
                                         shard
                                         (digx sexp :elements)
                                         nil))))

(defmethod naive-impl:compose-special ((collection document-collection) shard
                                       sexp (type (eql :blob))
                                       &key handle-duplicates-p &allow-other-keys)
  (declare (ignorable collection) (ignorable shard) (ignore handle-duplicates-p))
  ;;TODO: dealing with historical data should remove the check some time
  ;;was most likely to ensure balanced plists, should maybe implement that again
  ;;would make checking for types simpler

  (if (listp (car (cdr sexp)))
      (read-blob (car (cdr sexp)))
      (read-blob (cdr sexp))))

(defmethod naive-impl:compose-document ((collection document-collection)
                                        shard document-form
                                        &key (handle-duplicates-p t) &allow-other-keys)
  ;;(break "compose-doc ~S" document-form)
  (naive-impl:compose-special collection
                              shard
                              document-form
                              :document
                              :handle-duplicates-p handle-duplicates-p))
