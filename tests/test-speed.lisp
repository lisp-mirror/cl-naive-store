(ignore-errors (delete-package :test-speed-simple))

(defpackage :test-speed-simple
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core))

(in-package :test-speed-simple)

;; Giving a double margin for timing to account for different machine
;; sizes. You should check the time print outs to see how your system
;; is performing on the the tests.

(defparameter *surnames*
  #("Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia" "Miller" "Davis" "Rodriguez" "Martinez" "Hernandez" "Lopez" "Gonzales" "Wilson" "Anderson" "Thomas" "Taylor" "Moore" "Jackson" "Martin" "Lee" "Perez" "Thompson" "White" "Harris" "Sanchez" "Clark" "Ramirez" "Lewis" "Robinson" "Walker" "Young" "Allen" "King" "Wright" "Scott" "Torres" "Nguyen" "Hill" "Flores" "Green" "Adams" "Nelson" "Baker" "Hall" "Rivera" "Campbell" "Mitchell" "Carter" "Roberts" "Gomez" "Phillips" "Evans" "Turner" "Diaz" "Parker" "Cruz" "Edwards" "Collins" "Reyes" "Stewart" "Morris" "Morales" "Murphy" "Cook" "Rogers" "Gutierrez" "Ortiz" "Morgan" "Cooper" "Peterson" "Bailey" "Reed" "Kelly" "Howard" "Ramos" "Kim" "Cox" "Ward" "Richardson" "Watson" "Brooks" "Chavez" "Wood" "James" "Bennet" "Gray" "Mendoza" "Ruiz" "Hughes" "Price" "Alvarez" "Castillo" "Sanders" "Patel" "Myers" "Long" "Ross" "Foster" "Jimenez"))

(defparameter *size* 1000000)
(defparameter *monster-size* 10000000)

(defparameter *store* nil)

(defparameter *collection* nil)

(defun time% (form &key pre-text post-text)
  (with-output-to-string (*trace-output*)
    (format *trace-output* "~A~%~%" pre-text)
    (time form)
    (format *trace-output* "~A~%~%" post-text)))

(defmacro time-in-secs (form)

  `(let ((start (get-universal-time))
         (end)
         (result))
     (setf result (time ,form))
     (setf end (get-universal-time))
     (values result (- end start))))

;;(time-in-secs (dotimes (x 1000000) (+ 1 x)))

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-speed-simple)))

  (setf *store*
        (add-multiverse-element
         *universe*
         (make-instance (store-class *universe*)
                        :name "simple-store"
                        :collection-class 'collection)))

  (setf *collection*
        (add-multiverse-element
         *store*
         (make-instance 'collection
                        :name "simple-collection"
                        ;; Specifying the key element, else its :key
                        :keys '(:emp-no)))))

(cl-naive-tests:define-suite (:test-speed-simple)
  (cl-naive-tests:testcase
   :adding-documents
   :equal '>=
   :expected 2
   :actual
   (let ((emps))

     (loop with x = *size*
           repeat x
           do
           (let ((xx (decf x)))
             (push
              (list
               :country (elt cl-naive-store.tests::*countries* (random 194))
               :surname (elt *surnames* (random 99))
               :name (format nil "Slave No ~A" xx)
               :emp-no xx)
              emps)))

     (format *trace-output* "Adding ~A documents.~%~%" *size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))
     (multiple-value-bind (result time-taken)
         (time-in-secs
          (dolist (emp emps)
            (cl-naive-store.naive-core:add-document *collection*
                                                    emp
                                                    :handle-duplicates-p nil)))
       (declare (ignore result))
       (format *trace-output* "Time after add.~%~A~%~%" (get-universal-time))
       time-taken)))

  (cl-naive-tests:testcase
   :perist-documents
   :equal '>=
   :expected 8
   :actual
   (progn
     (format *trace-output* "Persisting ~A documents.~%~%" *size*)
     (format *trace-output* "Time before perist.~%~A~%" (get-universal-time))

     (multiple-value-bind (result time-taken)
         (time-in-secs
          (persist-collection *collection*))
       (declare (ignore result))
       (format *trace-output* "Time after perist.~%~A~%~%" (get-universal-time))
       time-taken)))
  (cl-naive-tests:testcase
   :load-documents-from-disk
   :equal '>=
   :expected *size*
   :actual
   (progn
     (format *trace-output* "Clearing and loading ~A documents from disk.~%~%" *size*)

     (clear-collection *collection*)
     (format *trace-output* "Time before load.~%~A~%" (get-universal-time))

     (multiple-value-bind (result time-taken)
         (time-in-secs
          ;; Setting handle-duplicates-p to nil to speed up loading
          ;; of data. Un indexed data is always going to be terribly
          ;; slow when loading if you have to check for duplicates.
          ;; This test is more about how a simple log collection
          ;; would work because there is no fear of duplicates.
          (load-data *collection* :handle-duplicates-p nil :parallel-p nil))
       (declare (ignore result time-taken))
       (format *trace-output* "Time after load.~%~A~%~%" (get-universal-time))
       (length (documents *collection*)))))
  (cl-naive-tests:testcase
   :adding-documents-monster
   :equal '>=
   :expected 50
   :actual
   (let ((emps))

     ;;Clear collection, deletes all data from disk and removes
     ;;collection from store.
     (cl-naive-store.naive-core:remove-multiverse-element
      *store* *collection*
      :remove-data-from-disk-p t)
     ;;adding clean collection back to store to repopulate with new monster data.
     (add-multiverse-element
      *store*
      *collection*)
     ;; (break "check delete")
     (setf naive-impl:*debug-log-p* t)

     (loop with x = *monster-size*
           repeat x
           do
           (let ((xx (decf x)))
             (push
              (list
               :country (elt cl-naive-store.tests::*countries* (random 194))
               :surname (elt *surnames* (random 99))
               :name (format nil "Big Slave No ~A" xx)
               :emp-no xx)
              emps)))

     (setf naive-impl:*debug-log-p* nil)
     (format *trace-output* "Adding ~A documents.~%~%" *monster-size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))
     ;; (break "poes")
     (multiple-value-bind (result time-taken)
         (time-in-secs
          (dolist (emp emps)
            (add-document *collection*
                          emp
                          :handle-duplicates-p nil)))
       (declare (ignore result))
       (format *trace-output* "Time after add.~%~A~%~%" (get-universal-time))
       time-taken)))
  (cl-naive-tests:testcase
   :perist-documents-monster
   :equal '>=
   :expected 50
   :actual
   (progn
     (format *trace-output* "Persisting ~A documents.~%~%" *monster-size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))
     (multiple-value-bind (result time-taken)
         (time-in-secs
          (persist-collection *collection*))
       (declare (ignore result))
       (format *trace-output* "Time after add.~%~A~%~%" (get-universal-time))
       time-taken)))
  (cl-naive-tests:testcase
   :load-documents-from-disk-monster
   :equal '>=
   :expected *monster-size*
   :actual
   (progn
     (format *trace-output* "Clearing and loading ~A documents from disk.~%~%" *size*)
     (clear-collection *collection*)
     (format *trace-output* "Time before load.~%~A~%" (get-universal-time))

     (multiple-value-bind (result time-taken)
         (time-in-secs
          (load-data *collection* :handle-duplicates-p nil))
       (declare (ignore result time-taken))
       (format *trace-output* "Time after load.~%~A~%~%" (get-universal-time))
       (length (documents *collection*))))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-speed-simple)))
  (setf *collection* nil)
  (setf *store* nil))

;; (cl-naive-tests:run :suites :test-speed-simple)

