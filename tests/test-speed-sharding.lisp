(ignore-errors (delete-package :test-speed-sharding))

(require :sb-sprof)

(defpackage :test-speed-sharding
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core
        :cl-naive-store.naive-indexed :cl-naive-store.document-types
        :cl-naive-store.naive-documents))

(in-package :test-speed-sharding)

;; Giving a double margin for timing to account for different machine
;; sizes. You should check the time print outs to see how your system
;; is performing on the the tests.

;;The type of indexing you do has a big influence on the
;;speeds. Partial indexing is slower because a lot more indexing is
;;done.

(defparameter *surnames*
  #("Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia" "Miller" "Davis" "Rodriguez" "Martinez" "Hernandez" "Lopez" "Gonzales" "Wilson" "Anderson" "Thomas" "Taylor" "Moore" "Jackson" "Martin" "Lee" "Perez" "Thompson" "White" "Harris" "Sanchez" "Clark" "Ramirez" "Lewis" "Robinson" "Walker" "Young" "Allen" "King" "Wright" "Scott" "Torres" "Nguyen" "Hill" "Flores" "Green" "Adams" "Nelson" "Baker" "Hall" "Rivera" "Campbell" "Mitchell" "Carter" "Roberts" "Gomez" "Phillips" "Evans" "Turner" "Diaz" "Parker" "Cruz" "Edwards" "Collins" "Reyes" "Stewart" "Morris" "Morales" "Murphy" "Cook" "Rogers" "Gutierrez" "Ortiz" "Morgan" "Cooper" "Peterson" "Bailey" "Reed" "Kelly" "Howard" "Ramos" "Kim" "Cox" "Ward" "Richardson" "Watson" "Brooks" "Chavez" "Wood" "James" "Bennet" "Gray" "Mendoza" "Ruiz" "Hughes" "Price" "Alvarez" "Castillo" "Sanders" "Patel" "Myers" "Long" "Ross" "Foster" "Jimenez"))

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

(defparameter *size* 1000000)

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

(defmethod get-store-class ((test-name (eql :test-speed-sharding)))
  'document-store)

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-speed-sharding)))
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
                         :name (getf (getf element :element) :name)
                         :key-p (getf (getf element :element) :key-p)
                         :concrete-type (getf (getf element :element) :concrete-type)
                         :attributes (getf (getf element :element) :attributes)))
                      (getf
                       (getf *employee-document-type* :document-type)
                       :elements)))))

    (setf *store*
          (add-multiverse-element
           *universe*
           (make-instance
            (store-class *universe*)
            :name "simple-store"
            :collection-class 'cl-naive-store.naive-documents:document-collection)))

    (cl-naive-store.naive-core:add-multiverse-element *store* document-type)

    (setf *collection*
          (add-multiverse-element
           *store*
           (make-instance (collection-class *store*)
                          :name "simple-collection"
                          :document-type document-type
                          ;; Specifying the key element, else its :key
                          :keys '(:emp-no)
                          ;; Specifying the elements to set up indexes for.
                          :indexes '((:name :surname))
                          :shard-elements '(:country))))

    (persist *multiverse* :definitions-only-p t)))

(cl-naive-tests:define-suite (:test-speed-sharding)
  (cl-naive-tests:testcase
   :adding-documents
   :equal '>=
   :expected 18
   :actual
   (let ((emps))

     (loop with x = *size*
           repeat x
           do
           (let ((xx (decf x)))
             (push
              (make-document
               :universe *universe*
               :store (store *collection*)
               :collection *collection*
               :document-type "employee"
               :elements (list
                          :country (elt cl-naive-store.tests::*countries* (random 194))
                          :surname (elt *surnames* (random 99))
                          :name (format nil "Slave No ~A" xx)
                          :emp-no xx))
              emps)))

     (format *trace-output* "Adding ~A documents.~%~%" *size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))

     (setf *do-partial-indexing* nil)

     (multiple-value-bind (result time-taken)
         (time-in-secs
          (dolist (emp emps)
            (cl-naive-store.naive-core:add-document *collection*
                                                    emp
                                                    :handle-duplicates-p nil)))
       (declare (ignore result))

       (format *trace-output* "Time after add.~%~A~%" (get-universal-time))
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
       (format *trace-output* "Time after perist.~%~A~%" (get-universal-time))
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
          ;; of data. This test is more about how a simple log
          ;; collection would work because there is no fear of
          ;; duplicates.
          ;;Parallel loading should be reserved for shards.
          (load-data *collection* :handle-duplicates-p t :parallel-p t))
       (declare (ignore result time-taken))
       (format *trace-output* "Time after load.~%~A~%" (get-universal-time))
       (length (documents *collection*)))))

  (cl-naive-tests:testcase
   :adding-documents-partial-indexing
   :equal '>=
   :expected 20
   :actual
   (let ((emps))
     ;;Clear collection, deletes all data from disk and removes
     ;;collection from store.
     (cl-naive-store.naive-core:remove-multiverse-element
      *store* *collection*
      :remove-data-from-disk-p t)
     ;;adding clean collection back to store to repopulate with new
     ;;pratially indexed data.
     (add-multiverse-element
      *store*
      *collection*)
     ;; (break "check delete")
     (setf naive-impl:*debug-log-p* t)

     (loop with x = *size*
           repeat x
           do
           (let ((xx (decf x)))
             (push
              (make-document
               :universe *universe*
               :store (store *collection*)
               :collection *collection*
               :document-type "employee"
               :elements
               (list
                :country (elt cl-naive-store.tests::*countries* (random 194))
                :surname (elt *surnames* (random 99))
                :name (format nil "Big Slave No ~A" xx)
                :emp-no xx))
              emps)))

     (setf naive-impl:*debug-log-p* nil)
     (format *trace-output* "Adding ~A documents.~%~%" *size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))

     (setf *do-partial-indexing* t)

     (multiple-value-bind (result time-taken)
         (time-in-secs
          (dolist (emp emps)
            (add-document *collection*
                          emp
                          :handle-duplicates-p nil)))
       (declare (ignore result))
       (format *trace-output* "Time after add.~%~A~%" (get-universal-time))
       time-taken)))
  (cl-naive-tests:testcase
   :perist-documents-partial-indexing
   :equal '>=
   :expected 7
   :actual
   (progn
     (format *trace-output* "Persisting ~A documents.~%~%" *size*)
     (format *trace-output* "Time before add.~%~A~%" (get-universal-time))
     (multiple-value-bind (result time-taken)
         (time-in-secs
          (persist-collection *collection*))
       (declare (ignore result))
       (format *trace-output* "Time after add.~%~A~%" (get-universal-time))
       time-taken)))
  (cl-naive-tests:testcase
   :load-documents-from-disk-partial-indexing
   :equal '>=
   :expected *size*
   :actual
   (progn
     (format *trace-output* "Clearing and loading ~A documents from disk.~%~%" *size*)
     (clear-collection *collection*)
     (format *trace-output* "Time before load.~%~A~%" (get-universal-time))

     (multiple-value-bind (result time-taken)
         (time-in-secs
          (load-data *collection* :handle-duplicates-p t))
       (declare (ignore result time-taken))
       (format *trace-output* "Time after load.~%~A~%" (get-universal-time))
       (length (documents *collection*))))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-speed-sharding)))
  (setf *do-partial-indexing* t)
  (setf *collection* nil)
  (setf *store* nil))

;; (cl-naive-tests:run :suites :test-speed-sharding)
