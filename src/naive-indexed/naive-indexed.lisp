(in-package :cl-naive-store.naive-indexed)

;;TODO: Doing partial-indexing doubles the time it takes to load a database
;;Try to delay or spool of partial indexing on different thread.
(defparameter *do-partial-indexing* t
  "When this is set to t (which is the default), indexing is done for the individual elements of the indexes as well.")

(defclass indexed-shard (shard)
  ((hash-index :initarg :hash-index
               :accessor hash-index
               :initform nil
               :documentation "Hash table keyed on document uuid for quick retrieval of an document and its actual position in the documents vector of the shard.")
   (key-value-index :initarg :key-value-index
                    :accessor key-value-index
                    :initform nil
                    :documentation "Hash table keyed on document key values for quick retrieval of an document.
Used when doing key value equality comparisons."))

  (:documentation "Extends shards with indexes."))

(defmethod clear-documents ((shard indexed-shard))
  (call-next-method)
  (when (hash-index shard)
    (clrhash (hash-index shard)))
  (when (key-value-index shard)
    (clrhash (key-value-index shard))))

(defclass indexed-collection-mixin ()
  ((indexes :initarg :indexes
            :accessor indexes
            :initform nil
            :documentation "List of index combinations. Also indexes members partially if *partial-indexing* is t, for example '((:emp-no :surname gender)) is indexed as (:emp-no :surname :gender), (:emp-no :surname), :emp-no, :surname and :gender"))
  (:documentation "Collection extension to add very basic indexes."))

(defclass indexed-collection (indexed-collection-mixin collection)
  ())

(defmethod make-shard ((collection indexed-collection-mixin) shard-mac)
  (make-instance 'indexed-shard
                 :mac (or shard-mac (name collection))
                 :location (cl-fad:merge-pathnames-as-file
                            (pathname (ensure-location collection))
                            (make-pathname ;;:directory (list :relative (name collection))
                             :name (or shard-mac (name collection))
                             :type "log"))
                 :key-value-index (make-hash-table
                                   :test 'equalp
                                   #+(or sbcl ecl) :synchronized
                                   #+(or sbcl ecl) nil)
                 :hash-index (make-hash-table
                              :test 'equalp
                              #+(or sbcl ecl) :synchronized
                              #+(or sbcl ecl) nil)
                 :status :new))

(defmethod get-shard ((collection indexed-collection-mixin) shard-mac &key &allow-other-keys)
  (cl-naive-store.naive-core::get-shard-cache-safe% collection shard-mac))

(defgeneric hash (document)
  (:documentation "Returns the hash identifier for a data document. Data documents need a hash identifier to work with naive-store-indexed. naive-store-indexed will edit the document to add a hash identifier when adding documents to a collection. naive-store-indexed uses a UUID in its default implementation."))

(defmethod hash (document)
  (if (getx document :hash)
      (frmt "~A" (getx document :hash))))

(defgeneric (setf hash) (value document))

(defmethod (setf hash) (value document)
  (setf (getx document :hash) (frmt "~A" value)))

(defgeneric index-lookup-values (collection values &key shards &allow-other-keys)
  (:documentation "Looks up document in key value hash index. If you are not using document-types then the order of values matter.

Will use shards to limit the lookup to specific shards."))

(defmethod index-lookup-values ((collection collection) values &key shards &allow-other-keys)
  (declare (ignorable collection values shards))
  (warn "Not implemented!"))

;;TODO: Do parallel processing... this gets called a lot so trying to do parallel
;;slows loading down to a crawl
(defmethod index-lookup-values ((collection indexed-collection-mixin)
                                values &key (shards (and naive-impl:%loading-shard%
                                                         (list naive-impl:%loading-shard%)))
                                &allow-other-keys)
  (let ((final-docs))
    (do-sequence (shard (or shards
                            (shards collection)))

      (unless shard
        (error "No shard for index lookup - ~A"
               (or (and naive-impl:%loading-shard%
                        (list naive-impl:%loading-shard%))
                   shards
                   (shards collection))))

      (let* ((index (key-value-index shard))
             (docs (gethash-safe (cl-murmurhash:murmurhash values) index
                                 :lock (getx (lock shard) :values-index))))
        (when docs
          (setf final-docs (append final-docs docs)))))

    (remove-duplicates final-docs)))

(defgeneric index-lookup-hash (collection hash &key shards &allow-other-keys)
  (:documentation "Looks up document in UUID hash index. If shards is not supplied all loaded shards will be searched.

Returns document and position as values. Position is used in add-document to do replace if needed."))

(defmethod index-lookup-hash ((collection indexed-collection-mixin) hash
                              &key (shards (and naive-impl:%loading-shard%
                                                (list naive-impl:%loading-shard%)))
                              &allow-other-keys)
  (when hash
    (naive-impl::debug-log "index:index-lookup-hash ~A hash ~S"
                           (name collection) hash)
    ;;TODO: Parallel?????????
    (map nil (lambda (shard)
               (naive-impl::debug-log "index:index-lookup-hash ~A shard ~S"
                                      (name collection) (and shard (short-mac shard)))
               (when shard
                 (naive-impl::debug-log "index:index-lookup-hash ~A shard ~S -> ~S"
                                        (name collection) (and shard (short-mac shard))
                                        (gethash hash (hash-index shard)))
                 (let ((doc (gethash hash (hash-index shard))))
                   (when doc
                     (return-from index-lookup-hash (values (getx doc :document)
                                                            (getx doc :position)))))))
         (or shards (shards collection)))))

(defgeneric add-index (collection shard document position &key &allow-other-keys)
  (:documentation "Adds a document to two indexes. The first uses a UUID that will stay with the document for its life time. The UUID is used when persisting the document and is never changed once created. This allows us to change key values without loosing the identify of the original document.

The second is a key value hash index to be used when looking for duplicate documents during persist. If you are not using document-types the order of the keys in the plist matter. To make sure that you dont muck with the order of values/keys in your plists initialize all the possible value pairs with nil so that way the order is set.

A shard must be supplied.

Position saved to use with future add-document to replace actual documents in the document vector if needed. It is done to speed up add-document."))

(defmethod add-index ((collection indexed-collection-mixin) shard document
                      position &key key-values &allow-other-keys)
  (let* ((index-values (indexed-impl:index-values collection document))
         (key-values (or key-values (key-values collection document))))

    (unless shard
      (setf shard naive-impl:%loading-shard%))

    (setf (gethash-safe (hash document)
                        (hash-index shard)
                        :lock (getx (lock shard) :hash-index))
          (list :document document :position position))

    ;;TODO: Check if this is still true???
    ;;Used to do document value comparisons to find index-document
    ;; (setf (gethash (frmt "~S" key-values) (key-value-index collection)) (list document))
    (indexed-impl::push-value-index collection key-values document :shard shard)

    ;;key-values are in effect their own value index and because it could be completely
    ;;different from index-values it is added to value indexes as well.
    (indexed-impl::populate-value-index collection (list key-values) document :shard shard)
    (indexed-impl::populate-value-index collection index-values document :shard shard)))

(defgeneric remove-index (collection shard document &key &allow-other-keys)
  (:documentation "Removes a data document from the UUID and key value indexes.
A shard must be supplied."))

(defmethod remove-index ((collection indexed-collection-mixin)
                         shard document &key &allow-other-keys)
  (let ((key-values (key-values collection document))
        (indexes-values (indexed-impl:index-values collection document)))

    (unless shard
      (error "No shard - ~A"
             collection))

    (indexed-impl:remove-value-index collection shard key-values document)
    (indexed-impl::remove-index-values collection shard (list key-values) document)
    (indexed-impl::remove-index-values collection shard indexes-values document)

    (remhash (hash document) (hash-index shard))))

(defmethod remove-document ((collection indexed-collection-mixin)
                            document &key shard &allow-other-keys)
  (call-next-method)
  (remove-index collection shard document))

;;TODO: Need to get rid of this, sort out the duplicates some other way!!! This is called alot.

;;NOTE: Doing this because murmurhash is creating duplicates when you go beyond 10 million index values
(defun try-better-value-match (collection list key-values)
  (dolist (document list)
    (when (equalp key-values (key-values collection document))
      (return document))))

(defmethod existing-document ((collection indexed-collection-mixin)
                              document
                              &key (shard naive-impl:%loading-shard%)
                              key-values &allow-other-keys)

  (unless shard
    (error "No shard - ~A"
           collection))

  (let* (
         (existing-document)
         (position))

    (multiple-value-bind (doc position-x)
        (and (hash document)
             (index-lookup-hash collection (hash document)
                                :shards (and shard (list shard))))

      (when doc
        (setf existing-document doc)
        (setf position position-x)))

    (unless existing-document
      (let ((key-values
              (or key-values (key-values collection document))))
        (setf existing-document
              (try-better-value-match
               collection
               (index-lookup-values collection key-values
                                    :shards (and shard (list shard)))
               key-values))

        (when existing-document
          (multiple-value-bind (doc position-x)
              (and (hash existing-document)
                   (index-lookup-hash collection (hash existing-document)
                                      :shards (and shard (list shard))))

            (when doc
              (setf existing-document doc)
              (setf position position-x))))))

    (values (and position
                 ;;Getting the actual document just incase the index
                 ;;and doc went out of sync.
                 ;;TODO: Is this really necessary?
                 (elt (documents shard) position))
            position)))

(defmethod add-document ((collection indexed-collection-mixin) document
                         &key (shard naive-impl:%loading-shard%)
                         (handle-duplicates-p t)
                         (replace-existing-p t)
                         (update-index-p t)
                         &allow-other-keys)
  "Duplicates are not allowed for indexed collections!

If the document has no hash and a document with the same keys exists
in the collection the supplied document's hash will be set to that of
the existing document. The existing document will then be replaced
with the supplied document. This is done to maintain hash consistancy
of the store.

If you set replace-existing-p to nil then an existing document wont be
replaced by the supplied document. Basically nothing will be done.

Indexes will be updated by default, if you want to stop index updates
set update-index-p to nil. Just remember that if the document is
really \"new\" to the collection the indexes will be updated in any
case.

Note that if the document have child documents that come from another
collection and you m changed them add-document will not try to sync
those with existing documents! That also means that add wont assign
uuid's to any child documents that don't have them set, you have to
set them yourself.
"
  (let ((mac (document-shard-mac collection document)))
    (setf shard (cl-naive-store.naive-core::ensure-shard
                 shard collection mac 'add-document)))

  (when (listp document)
    (unless (atom (car document))
      (error "There are issues with the document format it needs to be a plist.~%~%~S"
             document)))

  (let ((existing-document%)
        (action-taken)
        (key-values (key-values collection document)))

    (bt:with-lock-held ((getx (lock shard) :docs))
      (if handle-duplicates-p
          (if (keys collection)
              (multiple-value-bind (existing-document position)
                  (existing-document collection document :shard shard)
                (setf existing-document% existing-document)
                (if (and position replace-existing-p)
                    (progn
                      ;; (break "~S ~S" document existing-document)
                      (when (naive-impl:empty-p (hash document))
                        (setf (hash document) (hash existing-document)))
                      (unless (equalp (hash document) (hash existing-document))
                        (error "Clobbering document with different hash.~%Existing~%~S~%New~%~S"
                               existing-document document))
                      (when update-index-p
                        (add-index collection shard document position
                                   :key-values key-values))
                      (setf (elt (documents shard) position) document)
                      (setf action-taken :replaced))
                    (progn

                      (when (naive-impl:empty-p (hash document))
                        (setf (hash document) (uuid:make-v4-uuid)))
                      (add-index collection shard document
                                 (vector-push-extend document (documents shard))
                                 :key-values key-values)

                      (setf action-taken :added))))
              (progn
                (when (naive-impl:empty-p (hash document))
                  (setf (hash document) (uuid:make-v4-uuid)))

                (add-index collection shard document
                           (vector-push-extend document (documents shard))
                           :key-values key-values)

                (setf action-taken :added)))
          (progn
            (when (naive-impl:empty-p (hash document))
              (setf (hash document) (uuid:make-v4-uuid)))
            (add-index collection shard document
                       (vector-push-extend document (documents shard))
                       :key-values key-values)

            (setf action-taken :added-without-duplicate-check))))

    (values
     document
     action-taken
     (if (equalp action-taken :replaced)
         existing-document%))))

