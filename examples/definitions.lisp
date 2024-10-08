(ignore-errors (delete-package :naive-examples))

(require 'cl-naive-store.naive-documents)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
(ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-naive-store.naive-documents))

(in-package :naive-examples)

(defun example-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "multiverse"))))

;;Deleting existing example database
(cl-fad:delete-directory-and-files
 "~/multiverse/universe/simple-store"
 :if-does-not-exist :ignore)

(defvar *multiverse-definition*
  `(:multiverse
    (:name "multiverse"
     :universe-class cl-naive-store.naive-core:universe
     :location ,(example-location)
     :universes

     ((:universe
       (:name "universe"
        :location ,(cl-fad:merge-pathnames-as-directory
                    (example-location)
                    (make-pathname :directory (list :relative "universe")))
        ;;Universe classes can be specific to a universe.
        :universe-class cl-naive-store.naive-core:universe
        :store-class cl-naive-store.naive-documents:document-store
        :collection-class cl-naive-store.naive-documents:document-collection
        :document-type-class cl-naive-store.document-types:document-type
        :stores
        ((:store
          (:name
           "human-resources"
           :collections
           ((:collection
             (:name "laptops"
              :label "Laptops"
              :document-type "laptop"))
            (:collection
             (:name "employees"
              :label "Employees"
              :document-type "employee")))
           :document-types
           ((:document-type
             (:name
              "laptop"
              :label
              "Laptop"
              :elements
              ((:element
                (:name :id
                 :label "Serial No"
                 :key-p t
                 :concrete-type (:type :string)
                 :attributes ((:attribute
                               (:name :display
                                :value t))
                              (:attribute
                               (:name :editable
                                :value t)))
                 :documentation
                       "Unique no that identifies the laptop."))
               (:element
                (:name :make
                 :label "Manufaturer"
                 :concrete-type (:type :string)
                 :attributes ((:attribute
                               (:name :display
                                :value t))
                              (:attribute
                               (:name :editable
                                :value t)))
                 :documentation "Then manufaturer of the laptop."))
               (:element (:name :model
                          :label "Model"
                          :concrete-type (:type :string)
                          :attributes ((:attribute
                                        (:name :display
                                         :value t))
                                       (:attribute
                                        (:name :editable
                                         :value t)))
                          :documentation "Model of the laptop.")))
              :attributes ()
              :documentation "List of laptops the company owns."))

            (:document-type
             (:name
              "child"
              :label
              "Child"
              :elements
              ((:element
                (:name :name
                 :label "Name"
                 :key-p t
                 :concrete-type (:type :string)
                 :attributes ((:attribute
                               (:name :display
                                :value t))
                              (:attribute
                               (:name :editable
                                :value t)))
                 :documentation "Name of child"))
               (:element
                (:name :sex
                 :label "Gender"
                 :concrete-type (:type :keyword)
                 :attributes ((:attribute
                               (:name :display
                                :value t))
                              (:attribute
                               (:name :editable
                                :value t))
                              (:attribute
                               (:name :value-list
                                :value (:male :female))))
                 :documentation
                       "Gender of the child, can only be male or female."))
               (:element
                (:name :age
                 :label "Age"
                 :concrete-type (:type :number)
                 :attributes
                       ((:attribute
                         (:name :display
                          :value t))
                        (:attribute
                         (:name :editable
                          :value t))
                        (:attribute
                         (:name :setf-validate
                          :value
                                (lambda (age)
                                  (if (<= age 21)
                                      (values t nil)
                                      (values nil "Child is to old"))))))
                 :documentation "How old the child is")))
              :attributes ()
              :documentation "List of laptops the company owns."))

            (:document-type
             (:name "employee"
              :label "Employee"
              :elements
                    ((:element
                      (:name :emp-
                       :label "Employee Number"
                       :key-p t
                       :concrete-type (:type :number)
                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))
                       :documentation "Unique identifier of employee."))
                     (:element (:name :name
                                :label "Name"
                                :concrete-type (:type :string)
                                :attributes ((:attribute
                                              (:name :display
                                               :value t))
                                             (:attribute
                                              (:name :editable
                                               :value t)))
                                :documentation "Name of employee"))
                     (:element
                      (:name :sex
                       :label "Gender"
                       :concrete-type (:type :keyword)
                       :attributes
                             ((:attribute
                               (:name :display
                                :value t))
                              (:attribute
                               (:name :editable
                                :value t))
                              (:attribute
                               (:name :value-list
                                :value (:male :female))))

                       :documentation
                             "Gender of the child, can only be male or female."))
                     (:element
                      (:name :dependents
                       :label "Children"
                       :concrete-type (:type :list
                                       :spec (:type :document
                                              :spec (:type "child"
                                                     :accessor (:name))))

                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))
                       :documentation "List of the employees children"))
                     (:element
                      (:name :laptop
                       :label "Laptop"
                       :concrete-type (:type :document
                                       :spec (:type "laptop"
                                              :collection "laptop-collection"
                                              :accessor (:id)))

                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))
                       :documentation "Laptop allocated to employee"))
                     (:element
                      (:name :first-born
                       :label "First Born Child"
                       :concrete-type
                             (:type :document
                              :spec (:type "child"
                                     :collection "employees"
                                     :accessor (:emp-no :dependents :name)))

                       :attributes ((:attribute
                                     (:name :display
                                      :value t))
                                    (:attribute
                                     (:name :editable
                                      :value t)))
                       :documentation "List of the employees children")))
              :attributes ()
              :documentation "List of laptops the company owns."))))))))))))

(defparameter *multiverse* nil)

(setf *multiverse* (cl-naive-store.naive-core:load-from-definition
                    nil :multiverse *multiverse-definition* :with-children-p t))

;;Interogate multiverse
(break "~S" *multiverse*)
