(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-getx :cl-naive-store.naive-core))
(in-package :naive-examples)

(defvar *multiverse* nil)

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "test-multiverse"))))

(defun tear-down-multiverse ()
  "Deletes any peristed data from examples."
  (cl-fad:delete-directory-and-files
   (if *multiverse*
       (location *multiverse*)
       (test-location))
   :if-does-not-exist :ignore))

;;Create multiverse
(defparameter *multiverse*
  (progn (tear-down-multiverse)
         (make-instance
          'multiverse
          :name "multiverse"
          :location (test-location) ;Setting the location on disk.
          :universe-class 'universe)))

(defparameter *universe*
  (make-instance 'universe
                 :name "universe"
                 :store-class 'store))

(add-multiverse-element *multiverse* *universe*)

(defparameter *store*
  (add-multiverse-element *universe* (make-instance (store-class *universe*)
                                                    :name "simple-store"
                                                    :collection-class 'collection)))

(defparameter *collection*
  (add-multiverse-element *store* (make-instance (collection-class *store*)
                                                 :name "simple-collection"
                                                 :keys '(:id)))) ; Specifying the key element, else its :key

(persist *multiverse* :definitions-only-p t)

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

;;(cl-naive-store.tests:test-all)
