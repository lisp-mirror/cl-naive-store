(ignore-errors (delete-package :test-basic))

(defpackage :test-basic
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core))

(in-package :test-basic)

(defparameter *store* nil)

(defparameter *collection* nil)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-basic)))
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
                        :keys '(:id))))

  ;; Add some documents to the collection
  (add-document *collection* (list :name "Piet" :surname "Gieter" :id 123))
  (add-document *collection* (list :name "Sannie" :surname "Gieter" :id 321))
  (add-document *collection* (list :name "Koos" :surname "Van" :id 999))

  ;; Update Van
  (add-document *collection* (list :name "Koos Snr" :surname "Van" :id 999))

  ;; Duplicates are handled by default, so this will not cause a duplicate document
  (add-document *collection* (list :name "Piet" :surname "Gieter" :id 123)))

(cl-naive-tests:define-suite (:test-basic)
  (cl-naive-tests:testcase :test-query-and-duplicates
                           :expected 2
                           :actual (let ((results (query-data
                                                   *collection*
                                                   :query (lambda (document)
                                                            (<= (getx document :id)
                                                                900)))))
                                     (length results)))
  (cl-naive-tests:testcase :test-all-elements
                           :expected '(:name "Piet" :surname "Gieter" :id 123)
                           :actual (let ((document (query-document
                                                    *collection*
                                                    :query (lambda (document)
                                                             (= (getx document :id)
                                                                123)))))

                                     document))
  (cl-naive-tests:testcase :test-update-and-query-document
                           :expected "Koos Snr"
                           :actual (let ((document (query-document
                                                    *collection*
                                                    :query (lambda (document)
                                                             (= (getx document :id)
                                                                999)))))
                                     (getx document :name))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-basic)))
  (setf *collection* nil)
  (setf *store* nil))

;;(cl-naive-tests:run :suites :test-basic)

