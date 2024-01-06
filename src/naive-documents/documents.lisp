(in-package :cl-naive-store.naive-documents)

(defmethod deleted-p ((document document))
  (document-deleted-p document))

(defmethod (setf deleted-p) (value (document document) &key &allow-other-keys)
  (setf (document-deleted-p document) value)
  document)

(defun check-keys-and-synq (old new allow-key-change-p)
  (if (equalp (key-values (document-collection new) new)
              (key-values (document-collection old) old))
      (progn
        (setf (document-changes old) (or (document-changes new) (document-elements new)))
        old)
      (if allow-key-change-p
          (progn
            (setf (document-changes old) (or (document-changes new) (document-elements new)))
            old)
          (error (frmt "Attempted key change not allowed ~%~S~%with~%~S" old new)))))

(defun persist-merge-document (existing-document document allow-key-change-p)
  (let ((merged-document))
    (if (eql document existing-document)
        (setf merged-document document)
        (if (not (naive-impl:empty-p (getx document :hash)))
            (if (not (equalp (getx document :hash) (getx existing-document :hash)))
                (error (frmt "Clobbering ~%~S~%with~%~S" existing-document document))
                (progn
                  (setf merged-document
                        (check-keys-and-synq existing-document
                                             document allow-key-change-p))))
            (progn
              (setf merged-document
                    (check-keys-and-synq existing-document
                                         document allow-key-change-p)))))
    merged-document))

(defun reference-documents-equal-p (original prepped)
  (or (empty-p (getx prepped :hash))
      (not (equalp (getx original :elements)
                   (getx prepped :elements)))))

(defmethod add-document :around ((collection document-collection) document
                                 &key (shard naive-impl:%loading-shard%)
                                 (handle-duplicates-p t)
                                 (replace-existing-p t)
                                 (update-index-p t)
                                 &allow-other-keys)

  (unless (document-p document)
    (unless (atom (car document))
      (error "There are issues with the document format it needs to be a plist.~%~%~S"
             document))
    (setf document (make-document
                    :universe (universe (store collection))
                    :store (store collection)
                    :collection collection
                    :document-type (document-type collection)
                    :elements document)))

  (call-next-method collection document :shard shard
                                        :handle-duplicates-p handle-duplicates-p
                                        :replace-existing-p replace-existing-p
                                        :update-index-p update-index-p))

(defmethod persist-document ((collection document-collection) document
                             &key shard (handle-duplicates-p t) delete-p
                             (file-name nil new-file-p)
                             file-stream dont-add-to-collection-p &allow-other-keys)
  "persist-document for document-collection is leniant in what it takes as a document, it can be of type document or a plist."

  (setf shard (cl-naive-store.naive-core::ensure-shard
               shard collection
               (document-shard-mac collection document)
               'persist-document))

  (unless (document-p document)

    (setf document (make-document
                    :universe (universe (store collection))
                    :store (store collection)
                    :collection collection
                    :document-type (document-type collection)
                    :elements document)))

  (let* ((document (if (or delete-p (getx document :deleted-p))
                       (progn
                         (remove-document collection document :shard shard)
                         document)
                       ;;Speads up persisting of docs from an existing
                       ;;collection so shit does not need to be added to
                       ;;collection again.
                       (if dont-add-to-collection-p
                           document
                           (add-document collection document
                                         :handle-duplicates-p handle-duplicates-p
                                         :shard shard))))
         (sexp))

    (unless (document-p document)

      (setf document (make-document
                      :universe (universe (store collection))
                      :store (store collection)
                      :collection collection
                      :document-type (document-type collection)
                      :elements document)))

    (if (document-changes document)
        (setf (document-elements document)
              (document-changes document)))

    (unless (document-collection document)
      (setf (document-collection document) collection))

    (unless (document-store document)
      (setf (document-store document) (store collection)))

    (unless (document-universe document)
      (setf (document-universe document) (universe (store collection))))

    (unless (document-document-type document)
      (setf (document-document-type document) (document-type collection)))

    (setf sexp (naive-impl:persist-form
                collection
                shard
                document
                :document))

    (cond
      (file-stream
       (naive-impl::write-to-stream file-stream sexp))
      (new-file-p
       (naive-impl::write-to-file file-name sexp :if-exists :supersede))
      (t
       (naive-impl:write-to-file (location shard) sexp)))

    (setf (document-persisted-p document) t)

    document))

