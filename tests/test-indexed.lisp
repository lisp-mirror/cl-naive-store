(ignore-errors (delete-package :test-indexed))

(defpackage :test-indexed
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core
        :cl-naive-store.naive-indexed))

(in-package :test-indexed)

(defparameter *store* nil)

(defparameter *collection* nil)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-indexed)))
  (setf *store*
        (add-multiverse-element
         *universe*
         (make-instance (store-class *universe*)
                        :name "simple-store"
                        :collection-class 'indexed-collection)))

  (setf *collection*
        (add-multiverse-element
         *store*
         (make-instance (collection-class *store*)
                        :name "simple-collection"
                        ;; Specifying the key element, else its :key
                        :keys '(:id)
                        ;; Specifying the elements to set up indexes for.
                        :indexes '((:name :surname)))))

  (persist *multiverse* :definitions-only-p t)

  ;; Add some documents to the collection
  (persist-document *collection* (list :name "Piet" :surname "Gieter" :id 123))
  (persist-document *collection* (list :name "Koos" :surname "Van" :id 999))
  (persist-document *collection* (list :name "Frikkie" :surname "Frikkedel" :id 1001))
  (persist-document *collection* (list :name "Tannie" :surname "Frikkedel" :id 1002))
  ;; Update Van (making sure index stays in sync)
  (persist-document *collection* (list :name "Koos Snr" :surname "Van" :id 999)))

(cl-naive-tests:define-suite (:test-indexed)
  (cl-naive-tests:testcase :test-update-and-query-document
                           :expected "Koos Snr"
                           :actual (let ((document (query-document
                                                    *collection*
                                                    :query (lambda (document)
                                                             (= (getx document :id)
                                                                999)))))
                                     (getx document :name)))
  (cl-naive-tests:testcase :test-query
                           :expected 3
                           :actual (length
                                    (query-data
                                     *collection*
                                     :query (lambda (document)
                                              (<= (getx document :id) 1001)))))
  (cl-naive-tests:testcase
   :test-index-lookup-values-1
   :expected (list 1 999)
   :actual (let ((results (index-lookup-values
                           *collection*
                           '((:name "Koos") (:surname "Van")))))
             (list (length results)
                   (getx
                    (first results)
                    :id))))
  (cl-naive-tests:testcase
   :test-index-lookup-values-2
   :expected (list 2 1002 1001)
   :actual (let ((results (index-lookup-values
                           *collection*
                           '(:surname "Frikkedel"))))

             (list
              (length results)
              (getx (first results) :id)
              (getx (second results) :id)))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-indexed)))
  (setf *collection* nil)
  (setf *store* nil))

;;(cl-naive-tests:run :suites :test-indexed)
