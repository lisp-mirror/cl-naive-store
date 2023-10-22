(ignore-errors (delete-package :test-basic-persisted))

(defpackage :test-basic-persisted
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core))

(in-package :test-basic-persisted)

(defun count-lines-in-file (path)
  (with-input-from-string (file-content (naive-impl::file-to-string path))
    (loop for line = (read-line file-content nil)
          while line count line)))

(defparameter *store* nil)

(defparameter *collection* nil)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-basic-persisted)))
  (persist *multiverse* :definitions-only-p t)

  (setf *store*
        (add-multiverse-element
         *universe*
         (make-instance (store-class *universe*)
                        :name "simple-store"
                        :collection-class 'collection)))

  (setf *collection*
        (add-multiverse-element
         *store*
         (make-instance (collection-class *store*)
                        :name "simple-collection"
                        ;; Specifying the key element, else its :key
                        :keys '(:id)))))

(cl-naive-tests:define-suite (:test-basic-persisted)
  (cl-naive-tests:testcase
   :count-1
   :expected 1
   :actual (progn
             (persist-document *collection*
                               (list :name "Piet"
                                     :surname "Gieter"
                                     :id 123))
             (length (documents *collection*))))
  (cl-naive-tests:testcase
   :count-2
   :expected 2
   :actual (progn
             (persist-document *collection*
                               (list :name "Sannie"
                                     :surname "Gieter"
                                     :id 321))
             (length (documents *collection*))))
  (cl-naive-tests:testcase
   :count-3
   :expected 3
   :actual (progn
             (persist-document *collection*
                               (list :name "Koos"
                                     :surname "Van"
                                     :id 999))
             (length (documents *collection*))))
  (cl-naive-tests:testcase
   :test-update
   :expected '(3 "Koos Snr")
   :actual (let ((document (persist-document *collection*
                                             (list :name "Koos Snr"
                                                   :surname "Van"
                                                   :id 999))))
             (list
              (length (documents *collection*))
              (getx document :name))))
  (cl-naive-tests:testcase
   :clear-collection
   :expected 0
   :actual (progn
             (clear-collection *collection*)
             (length (documents *collection*))))
  (cl-naive-tests:testcase
   :test-lazy-loading
   :expected 2
   :actual (length (query-data
                    *collection*
                    :query (lambda (document)
                             (<= (getx document :id) 900)))))
  (cl-naive-tests:testcase
   :count-lines-in-file
   :expected 4
   :actual (count-lines-in-file (location *collection*)))
  (cl-naive-tests:testcase
   :delete
   :expected (list 1 6)
   :actual (let ((results (query-data *collection*
                                      :query (lambda (document)
                                               (<= (getx document :id) 900)))))
             (mapcar #'(lambda (doc)
                         (delete-document *collection* doc))
                     results)

             ;;Delete should remove 2 of the 3 documents from the
             ;;collection and write the deleted documents to file.
             (list
              (length (documents *collection*))
              (count-lines-in-file (location *collection*)))))

  (cl-naive-tests:testcase
   :sanitize-data-file
   :expected 1
   :actual (progn
             (sanitize-data-file *collection*
                                 :if-does-not-exist :create)
             ;;Should only contain 1 document now as sanitize
             ;;persists only what is live in the *collection* in a
             ;;new file that replaces the old.
             (count-lines-in-file (location *collection*)))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-basic-persisted)))
  (setf *collection* nil)
  (setf *store* nil))

;;(cl-naive-tests:run :suites :test-basic-persisted)
