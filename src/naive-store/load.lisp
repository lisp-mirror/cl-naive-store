(in-package :cl-naive-store.naive-core)

;;TODO: Should be replaced with get-definitions
(defun find-collection-files (collection)
  (let ((path (cl-fad:merge-pathnames-as-file
               (pathname (ensure-location collection))
               (make-pathname :directory '(:relative :wild-inferiors)
                              :name :wild
                              :type "log")))
        (files (directory
                (cl-fad:merge-pathnames-as-file
                 (pathname (ensure-location collection))
                 (make-pathname :directory '(:relative :wild-inferiors)
                                :name :wild
                                :type "log")))))

    (unless files
      (naive-impl::debug-log (frmt "no files on path") :file-p t :args path))
    files))

(defgeneric load-shard (collection shard filename &key handle-duplicates-p
                        &allow-other-keys)
  (:documentation "Loads documents from file."))

(defmethod load-shard :around ((collection collection) shard filename
                               &key handle-duplicates-p &allow-other-keys)
  (declare (ignorable filename) (ignorable handle-duplicates-p))
  (let ((naive-impl:%loading-shard% shard))
    (call-next-method)))

;;TODO: Add a catch all error thingy so that the status does not get stuck in :loading
;;TODO: add a wait with a time out if .lock file exists?

;;Dont try to do compose-document asyncronously because the order in
;;which the documents are loaded in the underlying container matter,
;;for deleted documents and document history (aka versions) both!!!!
(defmethod load-shard ((collection collection) shard filename &key
                       (handle-duplicates-p t) &allow-other-keys)

  ;;TODO: Added a check for :loaded as well else it reloads the shard
  ;;when reference collections are found and that causes crap versions.
  ;;Now need to see if lazy loading still works with this change.
  (unless (or (equalp (status shard) :loading)
              (equalp (status shard) :loaded))

    (unless (probe-file (format nil "~a.lock" (location shard)))
      ;;(break "file ~S~%~S~%~S" filename (location shard) (status shard))

      (let ((sexps))

        (setf (status shard) :loading)

        (naive-impl::debug-log (frmt "load-shard begin ")
                               :file-p t :args (list shard filename))
        (with-open-file (in (or filename (location shard)) :if-does-not-exist :create)
          (when in

            ;;TODO: Reconsider this!!!

            ;;Reading the file and releasing it as soon as possible
            ;;... not sure it is a good idea ... there is no
            ;;difference in speed. It also doubles the memory use!!!
            (setf sexps
                  (loop :for document-form = (read in nil)
                        :while document-form
                        :collect document-form))
            (close in)

            (loop :for document-form in sexps
                  :do
                  (progn
                    (naive-impl::compose-document
                     collection (or shard naive-impl:%loading-shard%) document-form
                     :handle-duplicates-p handle-duplicates-p))))))

      (naive-impl::debug-log (frmt "load-shard end ") :file-p t :args (list shard filename))
      (setf (status shard) :loaded))))

(defun load-shards (collection shards)
  (do-sequence (shard shards)
    (unless (status shard)
      (load-shard collection shard nil))))

(defparameter *files* (make-hash-table
                       ;; LispWorks and Clozure CL default with thread safe hash-tables.
                       ;; Other implementations will have to be checked.
                       #+lispworks :single-thread #+lispworks nil
                       #+(or sbcl ecl) :synchronized #+(or sbcl ecl) t
                       ;; This hash table is used from several threads,
                       ;; therefore it must be :shared t in ccl.
                       #+ccl :shared #+ccl t))

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

(defun ensure-structure (collection &optional create-multiverse-p)
  (if (store collection)
      (if (get-multiverse-element :collection (store collection) (name collection))
          (if (universe (store collection))
              (if (get-multiverse-element :store (universe (store collection))
                                          (name (store collection)))
                  (if (multiverse (universe (store collection)))
                      (if (get-multiverse-element
                           :universe
                           (multiverse (universe (store collection)))
                           (name (universe (store collection))))
                          t
                          (error "The collection parent store's universe is not registered with the parent multiverse."))
                      (if create-multiverse-p
                          (add-multiverse-element (make-instance 'multiverse

                                                                 :name "backwards-multiverse")
                                                  (universe (store collection)))
                          (error "The collection parent store's universe has no parent multiverse.")))
                  (error "The collection prarent store is not registered with its set universe."))
              (error "The colection parent store has no parent universe."))
          (error "The collection is not registered with its set parent store. "))
      (error "The collection has no parent store.")))

(defmethod load-data ((collection collection) &key shard-macs (parallel-p t)
                      (handle-duplicates-p t)
                      &allow-other-keys)

  (ensure-structure collection t)

  (let ((tasks))
    (unless parallel-p
      (unless (> (length (shards collection)) 0)
        (let ((files (or (gethash collection *files*)
                         (setf (gethash collection *files*)
                               (find-collection-files collection)))))

          (naive-impl::debug-log (frmt "load-data") :file-p t :args files)

          (do-sequence (filename files :parallel-p nil)
            (multiple-value-bind (mac file)
                (match-shard filename shard-macs)

              (unless mac
                (setf mac (pathname-name filename))
                (setf file filename))

              (when (or (not shard-macs) file)
                (let ((shard (get-shard collection mac)))

                  (unless shard
                    (naive-impl::debug-log "loading load-shard"
                                           :file-p t
                                           :args (list shard filename))

                    (setf shard (make-shard collection mac))
                    (set-shard-cache-safe% collection mac shard)
                    (vector-push-extend shard (shards collection))
                    (load-shard collection shard filename
                                :handle-duplicates-p handle-duplicates-p)))))))))

    (when parallel-p
      (unless (> (length (shards collection)) 0)
        (let ((files (or (gethash collection *files*)
                         (setf (gethash collection *files*)
                               (find-collection-files collection)))))
          (when files
            (let ((channel (lparallel:make-channel)))
              (dolist (filename files)
                (multiple-value-bind (mac file)
                    (match-shard filename shard-macs)

                  (unless mac
                    (setf mac (pathname-name filename))
                    (setf file filename))

                  (when (or (not shard-macs)
                            file)

                    (let ((shard (get-shard collection mac)))

                      (unless shard
                        (setf shard (make-shard collection mac))
                        (naive-impl::debug-log "submitting load-shard"
                                               :file-p t
                                               :args (list shard filename))

                        (push (lparallel:submit-task
                               channel
                               (lambda ()
                                 (set-shard-cache-safe% collection mac shard)
                                 (vector-push-extend shard (shards collection))
                                 (load-shard collection shard filename
                                             :handle-duplicates-p handle-duplicates-p)))
                              tasks))))))

              (naive-impl::debug-log "load-data checking tasks"
                                     :file-p t
                                     :args tasks)

              (dotimes (i (length tasks))
                i
                ;;TODO: WTF?
                ;;without this it gets stuck on loading naive-documents some time.
                (sleep 0.0001)
                (lparallel:receive-result channel)))))))))
