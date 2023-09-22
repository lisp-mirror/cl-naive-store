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

(defgeneric load-shard (collection shard filename &key &allow-other-keys)
  (:documentation "Loads documents from file."))

(defmethod load-shard :around ((collection collection) shard filename &key &allow-other-keys)
  (declare (ignorable filename))
  (let ((naive-impl:%loading-shard% shard))
    (call-next-method)))

;;TODO: Add a catch all error thingy so that the status does not get stuck in :loading
;;TODO: add a wait with a time out if .lock file exists?

;;Dont try to do compose-document asyncronously because the order in
;;which the documents are loaded in the underlying container matter,
;;for deleted documents and document history (aka versions) both!!!!
(defmethod load-shard ((collection collection) shard filename &key &allow-other-keys)
  ;;(break "file ~S~%~S~%~S" filename (location shard) (status shard))

  ;;TODO: Added a check for :loaded as well else it reloads the shard
  ;;when reference collections are found and that causes crap versions.
  ;;Now need to see if lazy loading still works with this change.
  (unless (or (equalp (status shard) :loading)
              (equalp (status shard) :loaded))
    (unless (probe-file (format nil "~a.lock" (location shard)))
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
                    ;;(break "load ~S" document-form)
                    (naive-impl::compose-document
                     collection (or shard naive-impl:%loading-shard%) document-form))))))

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

(defmethod load-data ((collection collection) &key shard-macs (parallel-p t)
                      &allow-other-keys)
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
                    (load-shard collection shard filename)
                    (set-shard-cache-safe% collection mac shard)
                    (vector-push-extend shard (shards collection))))))))))

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
                                 (load-shard collection shard filename)

                                 (set-shard-cache-safe% collection mac shard)
                                 (vector-push-extend shard (shards collection))))
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
