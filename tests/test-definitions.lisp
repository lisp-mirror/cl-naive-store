(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-getx :cl-naive-store.definitions))
(in-package :naive-examples)

(defvar *universe* nil)

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "test-universe"))))

(defvar *universe-definitions*
  `((:universe
     (:name "test-universe"
      :location ,(test-location)
      :universe-class cl-naive-store.naive-core:universe
      :store-class cl-naive-store.naive-documents:document-store
      :collection-class cl-naive-store.naive-documents:document-collection
      :stores ((:name "human-resources"
                :collections ((:name "laptop"
                               :label "Laptop"
                               :document-type "laptop")
                              (:name "employees"
                               :label "Employees"
                               :document-type "employee"))
                :document-types
                ((:document-type
                  (:name "laptop"
                   :label "Laptop"
                   :elements ((:name :id
                               :label "Serial No"
                               :key-p t
                               :concrete-type (:type :string)
                               :attributes (:display t :editable t)
                               :documentation "Unique no that identifies the laptop.")
                              (:name :make
                               :label "Manufaturer"
                               :concrete-type (:type :string)
                               :attributes (:display t :editable t)
                               :documentation "Then manufaturer of the laptop.")
                              (:name :model
                               :label "Model"
                               :concrete-type (:type :string)
                               :attributes (:display t :editable t)
                               :documentation "Model of the laptop."))
                   :attributes ()
                   :documentation "List of laptops the company owns."))

                 (:document-type
                  (:name "child"
                   :label "Child"
                   :elements ((:name :name
                               :label "Name"
                               :key-p t
                               :concrete-type (:type :string)
                               :attributes (:display t :editable t)
                               :documentation "Name of child")
                              (:name :sex
                               :label "Gender"
                               :concrete-type (:type :keyword)
                               :attributes (:display t
                                            :editable t
                                            :value-list (:male :female))
                               :documentation "Gender of the child, can only be male or female.")
                              (:name :age
                               :label "Age"
                               :concrete-type (:type :number)
                               :attributes (:display t :editable t
                                                     ;;setf-validate-is called for (setf getxe)
                                            :setf-validate
                                                     (lambda (age)
                                                       (if (<= age 21)
                                                           (values t nil)
                                                           (values nil "Child is to old"))))
                               :documentation "How old the child is"))
                   :attributes ()
                   :documentation "List of laptops the company owns."))

                 (:document-type
                  (:name "employee"
                   :label "Employee"
                   :elements ((:name :emp-
                               :label "Employee Number"
                               :key-p t
                               :concrete-type (:type :number)
                               :attributes (:display t :editable t)
                               :documentation "Unique identifier of employee.")
                              (:name :name
                               :label "Name"
                               :concrete-type (:type :string)
                               :attributes (:display t :editable t)
                               :documentation "Name of employee")
                              (:name :sex
                               :label "Gender"
                               :concrete-type (:type :keyword)
                               :attributes (:display t
                                            :editable t
                                            :value-list (:male :female))
                               :documentation "Gender of the child, can only be male or female.")
                              (:name :dependents
                               :label "Children"
                               :concrete-type (:type :list
                                               :spec (:type :document
                                                      :spec (:type "child"
                                                             :accessor (:name))))

                               :attributes (:display t :editable t)
                               :documentation "List of the employees children")
                              (:name :laptop
                               :label "Laptop"
                               :concrete-type (:type :document
                                               :spec (:type "laptop"
                                                      :collection "laptop-collection"
                                                      :accessor (:id)))

                               :attributes (:display t :editable t)
                               :documentation "Laptop allocated to employee")
                              (:name :first-born
                               :label "First Born Child"
                               :concrete-type (:type :document
                                               :spec (:type "child"
                                                      :collection "employees"
                                                      :accessor (:emp-no :dependents :name)))

                               :attributes (:display t :editable t)
                               :documentation "List of the employees children"))
                   :attributes ()
                   :documentation "List of laptops the company owns.")))))))))

(defparameter *universes* nil)

(defun tear-down-universe ()
  "Deletes any peristed data from examples."
  (cl-fad:delete-directory-and-files
   "~/temp/test-universe/"
   :if-does-not-exist :ignore))

(let ((results))
  (setf *universes* (cl-naive-store.definitions::create-multiverse
                     *universe-definitions* t))

  (assert (= 1 (length *universes*)))

  (push *universes* results)
  ;;TODO: Add more tests to check if directories exist with definition files etc.

  (print :success)
  (pprint (reverse results)))
