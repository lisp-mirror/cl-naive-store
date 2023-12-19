(ignore-errors (delete-package :test-naive-documents))

(defpackage :test-naive-documents
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core
        :cl-naive-store.naive-indexed :cl-naive-store.document-types
        :cl-naive-store.naive-documents))

(in-package :test-naive-documents)

;; Create a document-type definition for an employee
;; Will use it to manually add document type to store
(defparameter *employee-document-type*
  '(:document-type (:name "employee"
                    :label "Employee"
                    :elements
                    ((:element
                      (:name :emp-no
                       :label "Employee No"
                       :key-p t
                       :concrete-type :string
                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))))
                     (:element
                      (:name :name
                       :label "Name"
                       :concrete-type :string
                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))))
                     (:element
                      (:name :surname
                       :label "Surname"
                       :concrete-type :string
                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t))))))
                    :documentation "This type represents a simple employee master.")))

(defparameter *store* nil)

(defparameter *collection* nil)

(defmethod get-store-class ((test-name (eql :test-naive-documents)))
  'document-store)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-naive-documents)))
  (let ((document-type
          (make-instance
           'document-type
           :name (getf
                  (getf *employee-document-type* :document-type)
                  :name)
           :label (getf
                   (getf *employee-document-type* :document-type)
                   :label)
           :elements (mapcar
                      (lambda (element)
                        (make-instance
                         'element
                         :name(getf (getf element :element) :name)
                         :key-p (getf (getf element :element) :key-p)
                         :concrete-type (getf (getf element :element) :concrete-type)
                         :attributes (getf (getf element :element) :attributes)))
                      (getf
                       (getf *employee-document-type* :document-type)
                       :elements)))))

    (setf *store*
          (add-multiverse-element
           *universe*
           (make-instance (store-class *universe*)
                          :name "simple-store"
                          :collection-class
                          'cl-naive-store.naive-documents:document-collection)))

    (cl-naive-store.naive-core:add-multiverse-element *store* document-type)

    (setf *collection*
          (add-multiverse-element
           *store*
           (make-instance (collection-class *store*)
                          :name "simple-collection"
                          :document-type document-type
                          ;; Not specifying the keys to show
                          ;; that they are retrieved from the document-type
                          ;; if if no key is set.
                          ;; :keys ...
                          ;; Specifying the elements to set up indexes for.
                          :indexes '((:name :surname)))))

    (persist *multiverse* :definitions-only-p t)

    ;; Add some documents to the *collection*
    (persist-document *collection*
                      (make-document
                       :store (store *collection*)
                       :collection *collection*
                       :document-type "employee"
                       :elements (list :name "Piet" :surname "Gieter" :emp-no 123)))

    (persist-document *collection*
                      (make-document
                       :store (store *collection*)
                       :collection *collection*
                       :document-type "employee"
                       :elements (list :name "Sannie" :surname "Gieter" :emp-no 321)))

    (persist-document *collection*
                      (make-document
                       :store (store *collection*)
                       :collection *collection*
                       :document-type "employee"
                       :elements (list :name "Koos" :surname "Van" :emp-no 999)))

    (persist-document *collection*
                      (make-document
                       :store (store *collection*)
                       :collection *collection*
                       :document-type "employee"
                       :elements (list :name "Frikkie" :surname "Frikkedel"
                                       :emp-no 1001)))

    ;; Update 999 to Koos Snr
    (persist-document *collection*
                      (make-document
                       :store (store *collection*)
                       :collection *collection*
                       :document-type "employee"
                       :elements (list :name "Koos Snr" :surname "Van" :emp-no 999)))))

(cl-naive-tests:define-suite (:test-naive-documents)
  (cl-naive-tests:testcase :test-index-lookup-values-1
                           :expected 1
                           :actual (length
                                    (index-lookup-values *collection*
                                                         '((:name "Koos")
                                                           (:surname "Van")))))
  (cl-naive-tests:testcase :test-index-lookup-values-2
                           :expected 2
                           :actual (length
                                    (index-lookup-values *collection*
                                                         '(:surname "Gieter"))))
  (cl-naive-tests:testcase :test-query
                           :expected 2
                           :actual (length
                                    (query-data
                                     *collection*
                                     :query (lambda (document)
                                              (<= (getx document :emp-no) 900)))))
  (cl-naive-tests:testcase
   :test-changes
   :expected '("Pot"
               (:NAME "Sannie" :SURNAME "Gieter" :EMP-NO 321)
               (:NAME "Sannie" :SURNAME "Pot" :EMP-NO 321))
   :actual (let ((document (query-document
                            *collection*
                            :query (lambda (document)
                                     (= (getx document :emp-no) 321)))))
             (setf (getx document :surname) "Pot")
             (list
              (getx document :surname)
              (getx document :elements~)
              (getx document :changes~))))
  (cl-naive-tests:testcase
   :test-lazy-loading
   :expected 2
   :actual (progn
             (clear-collection *collection*)
             (length (query-data
                      *collection*
                      :query (lambda (document)
                               (<= (getx document :emp-no) 900))))))
  (cl-naive-tests:testcase
   :test-old-versions
   :expected '((:NAME "Koos" :SURNAME "Van" :EMP-NO 999))
   :actual (let ((document (query-document
                            *collection*
                            :query (lambda (document)
                                     (= (getx document :emp-no) 999)))))
             (getx document :versions~))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-naive-documents)))
  (setf *collection* nil)
  (setf *store* nil))

;;(cl-naive-tests:run :suites :test-naive-documents)
