(in-package :documents-impl)

(defun document-values-p (list)
  "Checks if plist contains :elements keyword which would indicate the plist represents an document."
  (find :elements list :test #'equalp))

(defun ensure-document-type (store document-type)
  (cond ((stringp document-type)
         (or (get-multiverse-element :document-type store document-type)
             document-type))
        (t document-type)))

(defun ensure-universe (multiverse universe)
  (cond ((stringp universe)
         (let ((uni (get-multiverse-element :universe multiverse universe)))
           ;;TODO: try to load the store from a definition file
           (unless uni (error "Universe does not exist in the multiverse: ~a" universe))
           uni))
        (t universe)))

(defun ensure-store (universe store)
  (cond ((stringp store)
         (let ((sto (get-multiverse-element :store universe store)))
           (unless sto (error "Store does not exist in the multiverse: ~A : ~A"
                              universe  store))
           sto))
        (t store)))

(defun ensure-collection (store collection)
  (cond ((stringp collection)
         (let ((col (get-multiverse-element :collection store collection)))
           (unless col (error "Collection does not exist in the store: ~A : ~A"
                              store collection))
           col))
        (t collection)))

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
                                       (type (eql :document)))

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
                                (or (getx sexp :universe)
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

                     :hash (frmt "~A" (digx sexp :hash))
                     :elements resolved-values))
              (add-document collection final-document))))

    (naive-impl::debug-log "END docs:Compose-special :document ~A" (name collection))

    final-document))

(defmethod naive-impl:compose-special ((collection document-collection) shard
                                       sexp (type (eql :child-document)))

  (let* ((ref-universe (ensure-universe
                        (multiverse (universe (store collection)))
                        (or (getx sexp :universe)
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
                                       sexp (type (eql :blob)))
  (declare (ignorable collection) (ignorable shard))
  ;;TODO: dealing with historical data should remove the check some time
  ;;was most likely to ensure balanced plists, should maybe implement that again
  ;;would make checking for types simpler

  (if (listp (car (cdr sexp)))
      (read-blob (car (cdr sexp)))
      (read-blob (cdr sexp))))

(defmethod naive-impl:compose-document ((collection document-collection)
                                        shard document-form
                                        &key &allow-other-keys)
  ;;(break "compose-doc ~S" document-form)
  (naive-impl:compose-special collection
                              shard
                              document-form
                              :document))
