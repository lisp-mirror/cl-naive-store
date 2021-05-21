(require 'cl-naive-store.naive-core)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl
   :cl-getx :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed :cl-naive-store.document-types
   :cl-naive-store.document-type-defs :cl-naive-store.naive-documents))
(in-package :naive-examples)

(defvar *universe* nil)

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "test-universe"))))

(defun tear-down-universe ()
  "Deletes any peristed data from examples."
  (cl-fad:delete-directory-and-files
   (if *universe*
       (location *universe*)
       (test-location))
   :if-does-not-exist :ignore))

(defparameter *universe*
  (progn
    (tear-down-universe)
    (make-instance 'universe
		   :location (test-location)
		   :store-class 'document-store)))

;; Create a data definition for an employee
;; It looks like a lot but dont panic its simple.
(defparameter *employee-document-type*
  '(:name "employee"
    :label "Employee"
    :elements ((:name :emp-no
		:label "Employee No"
		:concrete-type :string
		:key-p t
		:attributes (:display t :editable t))
	       (:name :name
		:label "Name"
		:concrete-type :string
		:attributes (:display t :editable t))
	       (:name :surname
		:label "Surname"
		:concrete-type :string
		:attributes (:display t :editable t)))
    :documentation "This type represents a simple employee master."))

(defparameter *store*
  (add-store   *universe* (make-instance (store-class *universe*)
					 :name "simple-store"
       					 :collection-class 'document-collection)))

(defparameter *elements*
  (mapcar (lambda (element)
	    (make-instance 'element
			   :name (getf element :name)
			   :key-p (getf element :key-p)
			   :concrete-type (getf element :concrete-type)
			   :attributes (getf element :attributes)))
	  (getf *employee-document-type* :elements)))

(defparameter *document-type*
  (add-document-type *store* (make-instance 'document-type
					    :name (getf *employee-document-type* :name)
					    :label (getf *employee-document-type* :label)
					    :elements *elements*)))

(defparameter *collection*
  (add-collection *store* (make-instance (collection-class *store*)
					 :name "simple-collection"
					 :document-type *document-type*
					 ;; Not specifying the keys to show
					 ;; that they are retrieved from the document-type
					 ;; if if no key is set.
					 ;; :keys ...
					 ;; Specifying the elements to set up indexes for.
					 :indexes '((:name :surname)))))

;; Add some documents to the *collection*
(persist-document *collection*
		  (make-document
		   :store (store *collection*)
		   :collection *collection*
		   :document-type "employee"
		   :elements (list :name "Piet" :surname "Gieter" :emp-no 123)))
(assert (= 1 (length (documents *collection*))))

(persist-document *collection*
		  (make-document
		   :store (store *collection*)
		   :collection *collection*
		   :document-type "employee"
		   :elements (list :name "Sannie" :surname "Gieter" :emp-no 321)))
(assert (= 2 (length (documents *collection*))))

(persist-document *collection*
		  (make-document
		   :store (store *collection*)
		   :collection *collection*
		   :document-type "employee"
		   :elements (list :name "Koos" :surname "Van" :emp-no 999)))
(assert (= 3 (length (documents *collection*))))

(persist-document *collection*
		  (make-document
		   :store (store *collection*)
		   :collection *collection*
		   :document-type "employee"
		   :elements (list :name "Frikkie" :surname "Frikkedel" :emp-no 1001)))
(assert (= 4 (length (documents *collection*))))

(persist-document *collection*
		  (make-document
		   :store (store *collection*)
		   :collection *collection*
		   :document-type "employee"
		   :elements (list :name "Tannie" :surname "Frikkedel" :emp-no 1001)))
;; employe no 1001 is updated, not added:
(assert (= 4 (length (documents *collection*))))

(let ((results '()))

  ;; Lookup koos using index values and add it to results
  (push (index-lookup-values *collection* (list (list :name "Koos")
						(list :surname "Van")))
	results)
  (assert (first results))
  (assert (= 1 (length results)))

  ;; Lookup Frikkedel using index values and add it to results
  (push (index-lookup-values *collection* (list :surname "Frikkedel"))
	results)
  (assert (first results))
  (assert (= 2 (length results)))

  ;; Query the *collection*, query-data will load the data from file if the *collection* is empty,
  ;; and add it to the results
  (push (query-data *collection* :query (lambda (document)
					  (<= (getx document :emp-no) 900)))
	results)
  (assert (first results))
  (assert (= 3 (length results)))

  (let ((sannie (first (index-lookup-values *collection*
					    (list (list :name "Sannie")
						  (list :surname "Gieter"))))))

    (setf (getx sannie :surname) "Potgieter")
    ;; TODO: check the updated document is saved
    (push sannie results))

  (print :success)
  (pprint (reverse results)))

;;;; THE END ;;;;
