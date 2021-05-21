;; Setup to use cl-naive-store
(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-getx :cl-naive-store.naive-core :cl-naive-store.naive-indexed))
(in-package :naive-examples)

(defclass indexed-collection (indexed-collection-mixin collection)
  ())

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
		   :store-class 'store)))

(defparameter *store*
  (add-store   *universe* (make-instance (store-class *universe*)
					 :name "simple-store"
       					 :collection-class 'indexed-collection)))

(defparameter *collection*
  (add-collection *store* (make-instance (collection-class *store*)
					 :name "simple-collection"
					 ;; Specifying the key element, else its :key
					 :keys '(:id)
					 ;; Specifying the elements to set up indexes for.
					 :indexes '((:name :surname)))))

;; Add some documents to the collection

(persist-document *collection* (list :name "Piet" :surname "Gieter" :id 123))
(assert (= 1 (length (documents *collection*))))
(persist-document *collection* (list :name "Sannie" :surname "Gieter" :id 321))
(assert (= 2 (length (documents *collection*))))
(persist-document *collection* (list :name "Koos" :surname "Van" :id 999))
(assert (= 3 (length (documents *collection*))))
(persist-document *collection* (list :name "Frikkie" :surname "Frikkedel" :id 1001))
(assert (= 4 (length (documents *collection*))))
(persist-document *collection* (list :name "Tannie" :surname "Frikkedel" :id 1001))
(assert (= 4 (length (documents *collection*)))) ; updated 1001

(let ((results '()))

  ;; Lookup koos using index values and add it to results
  (push (index-lookup-values *collection* '((:name "Koos") (:surname "Van")))
	results)
  (assert (first results))
  (assert (= 1 (length results)))

  ;; Lookup Frikkedel using index values and add it to results
  (push (index-lookup-values *collection* '(:surname "Frikkedel"))
	results)
  (assert (first results))
  (assert (= 2 (length results)))

  ;; Query the collection, query-data will load the data from file if the *collection* is empty,
  ;; and add it to the results

  (push (query-data *collection* :query (lambda (document)
					  (<= (getx document :id) 900)))
	results)
  (assert (first results))
  (assert (= 3 (length results)))

  (print :success)
  (pprint (reverse results)))

