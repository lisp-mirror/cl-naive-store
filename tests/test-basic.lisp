(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-getx :cl-naive-store.naive-core))
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
		   :store-class 'store)))

(defparameter *store*
  (add-store   *universe* (make-instance (store-class *universe*)
					 :name "simple-store"
       					 :collection-class 'collection)))

(defparameter *collection*
  (add-collection *store* (make-instance (collection-class *store*)
					 :name "simple-collection"
					 :keys '(:id)))) ; Specifying the key element, else its :key

;; Add some documents to the collection
(add-document *collection* (list :name "Piet"   :surname "Gieter" :id 123))
(add-document *collection* (list :name "Sannie" :surname "Gieter" :id 321))
(add-document *collection* (list :name "Koos"   :surname "Van"    :id 999))

;; Duplicates are handled by default, so this will not cause a duplicate document
(add-document *collection* (list :name "Piet"   :surname "Gieter" :id 123))

;; Query the collection
(let ((results  (query-data *collection* :query (lambda (document) (<= (getx document :id) 900)))))
  (assert (= 2 (length results)))
  (print :success)
  (pprint results))

