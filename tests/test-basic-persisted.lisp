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
          :universe-class 'universe)))

(add-multiverse-element *multiverse* *universe*)

(defun count-lines-in-file (path)
  (with-input-from-string (file-content (naive-impl::file-to-string path))
    (loop for line = (read-line file-content nil)
          while line count line)))

(defparameter *universe*
  (make-instance 'universe
                 :name "universe"
                 :store-class 'store))

(defparameter *store*
  (add-multiverse-element *universe* (make-instance (store-class *universe*)
                                                    :name "simple-store"
                                                    :collection-class 'collection)))

(defparameter *collection*
  (add-multiverse-element *store* (make-instance (collection-class *store*)
                                                 :name "simple-collection"
                                                 :keys '(:id)))) ; Specifying the key element, else its :key

(persist *multiverse* :definitions-only-p t)

;; Add some documents to the *collection*
(persist-document *collection* (list :name "Piet"   :surname "Gieter" :id 123))
(assert (= 1 (length (documents *collection*))))
(persist-document *collection* (list :name "Sannie" :surname "Gieter" :id 321))
(assert (= 2 (length (documents *collection*))))
(persist-document *collection* (list :name "Koos"   :surname "Van"    :id 999))
(assert (= 3 (length (documents *collection*))))

;; Clear the *collection*, ie unload documents from memory so we can show that it has been persisted.
(clear-collection *collection*)
(assert (= 0 (length (documents *collection*))))

;; Query the *collection*, query-data will load the data from file if the *collection* is empty
(let ((results (query-data *collection* :query (lambda (document) (<= (getx document :id) 900)))))
  (assert (= 3 (length (documents *collection*)))
          ()
          "(length (documents *collection*)) = ~S" (length (documents *collection*)))
  (assert (= 2 (length results))
          ()
          "(length results) = ~S"  (length results))
  ;; (pprint results)
  ;; test maintenance functionality - sanitize collection
  (mapcar #'(lambda (doc) (delete-document *collection* doc)) results)
  (assert (= 1 (length (documents *collection*)))
          ()
          "(length (documents *collection*)) = ~S"  (length (documents *collection*)))
  ;; now main log file contain information about deleted documents
  (assert (= 5 (count-lines-in-file (location *collection*)))
          ()
          "(count-lines-in-file (location *collection*)) = ~S" (count-lines-in-file (location *collection*)))
  (sanitize-data-file *collection* :if-does-not-exist :create)
  ;; now collection log file contains just one document as *collection* do
  (assert (= 1 (count-lines-in-file (location *collection*)))
          ()
          "(count-lines-in-file (location *collection*)) = ~S" (count-lines-in-file (location *collection*)))
  (assert (= 1 (length (documents *collection*)))
          ()
          "(length (documents *collection*)) = ~S"  (length (documents *collection*)))
  ;; (pprint (documents *collection*))
  (print :success)
  (print '()))
;;;; THE END ;;;;
