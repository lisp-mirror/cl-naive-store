(ignore-errors (delete-package :test-sharding-simple))

(defpackage :test-sharding-simple
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core)
  (:use  :cl-naive-store.naive-indexed :cl-naive-store.document-types
         :cl-naive-store.naive-documents))

(in-package :test-sharding-simple)

;; To be able to have reproducible and checkable tests, we'll use a
;; deterministic way to generate employee attributes.  For this, we
;; use those two vectors, and we ensure that the GCD of their length
;; is 1 for maximal period.
;; So we can generate:
;; (* (length *countries*) (length  *surnames*))
;; = 1365 distinct combinations.

(defparameter *surnames*
  #("Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller")
  "A vector of surnames.")

(defparameter *store* nil)

(defparameter *collection* nil)

;;Cannot go below 202! The tests will fail.
(defparameter *size* 10000)

(defparameter *expected-shard-count* 0)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-sharding-simple)))

  (setf *expected-shard-count* 0)
  (setf *store*
        (add-multiverse-element
         *universe*
         (make-instance (store-class *universe*)
                        :name "simple-store"
                        :collection-class
                        'collection)))

  (setf *collection*
        (add-multiverse-element
         *store*
         (make-instance (collection-class *store*)
                        :name "simple-collection"
                        :keys '(:emp-no)

                        ;; Creating shards based on the country that the employee
                        ;; belongs to.  It is a bad example you should not shard on
                        ;; any value that could change in the future!
                        :shard-elements '(:country))))

  (persist *multiverse* :definitions-only-p t)

  (let ((emp-country  0)
        (emp-surname  0)
        (unique-countries (make-hash-table :test 'equalp)))

    (format *trace-output* "~&Adding ~D documents to collection~%" *size*)
    (time
     (dotimes (emp-no *size*)
       ;; We create employees country per country and loop again if we need more:
       (let ((country  (aref *countries* emp-country))
             (surname  (aref *surnames* emp-surname)))

         (unless (gethash country unique-countries)
           (incf *expected-shard-count*)
           (setf (gethash country unique-countries) country))

         (incf emp-surname)

         ;;We only have only 7 surnames start again on surnames
         (when (<= (length *surnames*) emp-surname)
           (setf emp-surname 0)
           ;;move on to next country
           ;;(setf emp-country (mod (1+ emp-country) (length *countries*)))
           (if (< emp-country (- (length *countries*) 1))
               (incf emp-country)
               (setf emp-country 0)))

         (add-document *collection*
                       (list
                        :country country
                        :surname surname
                        :name (format nil "Slave No ~A" emp-no)
                        :emp-no emp-no)
                       :handle-duplicates-p nil))))

    ;; Bulk perist documents
    (format *trace-output* "~&Persisting ~D collection documents~%" *size*)
    (time
     (persist-collection *collection*))))

(cl-naive-tests:define-suite (:test-sharding-simple)
  (cl-naive-tests:testcase :test-gcd
                           :expected 1
                           :actual (gcd (length *countries*) (length  *surnames*)))
  (cl-naive-tests:testcase :test-size
                           :expected t
                           :actual (>= *size* 202))
  (cl-naive-tests:testcase :expected-shard-count
                           :expected *expected-shard-count*
                           :actual (length (shards *collection*)))
  (cl-naive-tests:testcase :shar-file-count
                           :expected *expected-shard-count*
                           :actual
                           (length (uiop:directory-files
                                    (cl-fad:merge-pathnames-as-directory
                                     (location *store*)
                                     (make-pathname :directory
                                                    (list :relative
                                                          (name *collection*)))))))
  (cl-naive-tests:testcase
   :test-gcd
   :expected 202
   :actual (let ((data
                   (time
                    (query-data
                     *collection*
                     :query (lambda (document)

                              (or (and ; 51
                                   (>= (getx document :emp-no) 50)
                                   (<= (getx document :emp-no) 100))
                                  (and ; 101
                                   (>= (getx document :emp-no) (/ *size* 2))
                                   (<= (getx document :emp-no) (+ (/ *size* 2) 100)))
                                  ;; 50 : we don't have an emp-no =
                                  ;; *size*, max is (*size* - 1)
                                  (and
                                   (>= (getx document :emp-no) (- *size* 50))
                                   (<= (getx document :emp-no) *size*))))))))

             (length data)))
  (cl-naive-tests:testcase
   :how-many-davises
   :expected (multiple-value-bind (n r)
                 (truncate *size* (length *surnames*))
               (+ n
                  (if (<= r (position "Davis" *surnames*
                                      :test (function string-equal)))
                      0
                      1)))
   :actual (time (length
                  (query-data
                   *collection*
                   :query (lambda (document)
                            (equalp (getx document :surname) "Davis")))))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-sharding-simple)))
  (setf *collection* nil)
  (setf *store* nil))

;;(cl-naive-tests:run :suites :test-sharding-simple)
