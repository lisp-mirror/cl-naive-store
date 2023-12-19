;;(ignore-errors (delete-package :test-definitions))

(defpackage :test-definitions
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core))

(in-package :test-definitions)

(defparameter *multiverse-definition* nil)

(defun make-clean-definition ()
  `(:multiverse
    (:name "multiverse"
     :universe-class cl-naive-store.naive-core:universe
     :location ,(test-location)
     :universes

     ((:universe
       (:name "universe"
        :location ,(cl-fad:merge-pathnames-as-directory
                    (test-location)
                    (make-pathname :directory (list :relative "universe")))
        ;;Universe classes can be specific to a universe.
        :universe-class cl-naive-store.naive-core:universe
        :store-class cl-naive-store.naive-documents:document-store
        :collection-class cl-naive-store.naive-documents:document-collection
        :document-type-class cl-naive-store.document-types:document-type
        :stores ((:store
                  (:name "human-resources"
                   :collections ((:collection
                                  (:name "laptops"
                                   :label "Laptops"
                                   :document-type "laptop"))
                                 (:collection
                                  (:name "employees"
                                   :label "Employees"
                                   :document-type "employee")))
                   :document-types
                         ((:document-type
                           (:name "laptop"
                            :label "Laptop"
                            :elements ((:element
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
                                         :documentation "Unique no that identifies the laptop."))
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
                           (:name "child"
                            :label "Child"
                            :elements ((:element
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
                                         :documentation "Gender of the child, can only be male or female."))
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
                            :elements ((:element
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

                                         :documentation "Gender of the child, can only be male or female."))
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
                                         :concrete-type (:type :document
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

(defmethod cl-naive-tests:setup-suite ((test-name (eql :test-definitions)))
  (setf *universe* nil)
  (setf *multiverse* nil)

  (setf *multiverse-definition* (make-clean-definition))
  (setf *multiverse* (cl-naive-store.naive-core:load-from-definition
                      nil
                      :multiverse
                      *multiverse-definition*
                      :with-children-p t
                      :with-data-p nil)))
;;TODO: Add more tests to check if directories exist with definition files etc.

(defun make-clean-def-1 ()
  (list :multiverse
        (list :name "multiverse"
              :universe-class 'cl-naive-store.naive-core:universe
              :location (test-location)
              :universes
              (list
               (list :universe
                     (list :name "marvel"
                           :stores
                           (list
                            (list :store
                                  (list :name "wolverine"
                                        :collections
                                        (list (list :collection
                                                    (list :name "stats"
                                                          :label "Statistics"
                                                          :document-type "statistics")))
                                        :document-types
                                        (list (list :document-type
                                                    (list :name "statistics"
                                                          :label "Statistics"
                                                          :elements
                                                          (list (list :element
                                                                      (list :name :id
                                                                            :label "Id"
                                                                            :concrete-type :string
                                                                            :attributes (list (list :attribute
                                                                                                    (list :name :display
                                                                                                          :value t))
                                                                                              (list :attribute
                                                                                                    (list :name :editable
                                                                                                          :value t))))))
                                                          :documentation "Cool statistics."))))))))))))

(defun make-clean-def-2 ()
  (list :multiverse
        (list :name "multiverse"
              :universe-class 'cl-naive-store.naive-core:universe
              :location (test-location)
              :universes
              (list (list :universe
                          (list :name "marvel"
                                :stores (list (list :store
                                                    (list :name "wolverine"
                                                          :collections
                                                          (list (list :collection
                                                                      (list :name "stats"
                                                                            :label "Statistics"
                                                                            :document-type "statistics")))
                                                          :document-types
                                                          (list (list :document-type (list :name "statistics"
                                                                                           :label "Statistics"
                                                                                           :elements
                                                                                           (list (list :element
                                                                                                       (list :name :id
                                                                                                             :label "Id"
                                                                                                             :concrete-type :string
                                                                                                             :attributes (list (list :attribute
                                                                                                                                     (list :name :display
                                                                                                                                           :value t))
                                                                                                                               (list :attribute
                                                                                                                                     (list :name :editable
                                                                                                                                           :value t))))))
                                                                                           :documentation "Cool statistics."))))))))
                    (list :universe
                          (list :name "movies"
                                :stores
                                (list (list :store (list :name "super-heros"
                                                         :collections
                                                         (list (list :collection (list :name "movies"
                                                                                       :label "Movies"
                                                                                       :document-type "movie")))
                                                         :document-types
                                                         (list (list :document-type (list :name "movie"
                                                                                          :label "Movies"
                                                                                          :elements
                                                                                          (list (list :element
                                                                                                      (list :name :id
                                                                                                            :label "Id"
                                                                                                            :concrete-type :string
                                                                                                            :attributes (list (list :attribute
                                                                                                                                    (list :name :display
                                                                                                                                          :value t))
                                                                                                                              (list :attribute
                                                                                                                                    (list :name :editable
                                                                                                                                          :value t))))))
                                                                                          :documentation "Cool movie."))))))))))))

(defun make-clean-def-3 ()
  (list :multiverse
        (list :name "multiverse"
              :universe-class 'cl-naive-store.naive-core:universe
              :location (test-location)
              :universes
              (list (list :universe
                          (list :name "marvel"
                                :stores
                                (list (list :store
                                            (list :name "wolverine"
                                                  :collections
                                                  (list (list :collection
                                                              (list :name "stats"
                                                                    :label "Statistics"
                                                                    :document-type "statistics")))
                                                  :document-types
                                                  (list (list :document-type
                                                              (list :name "statistics"
                                                                    :label "Statistics"
                                                                    :elements
                                                                    (list (list :element
                                                                                (list :name :id
                                                                                      :label "Id"
                                                                                      :concrete-type :string
                                                                                      :attributes (list (list :attribute
                                                                                                              (list :name :display
                                                                                                                    :value t))
                                                                                                        (list :attribute
                                                                                                              (list :name :editable
                                                                                                                    :value t))))))
                                                                    :documentation "Cool statistics."))))))))))))

(defun make-clean-def-4 ()
  (list :multiverse
        (list :name "multiverse"
              :universe-class 'cl-naive-store.naive-core:universe
              :location (test-location)
              :universes
              (list (list :universe
                          (list :name "marvel"
                                :stores (list (list :store
                                                    (list :name "wolverine"
                                                          :collections
                                                          (list (list :collection
                                                                      (list :name "stats"
                                                                            :label "Statistics"
                                                                            :document-type "statistics")))
                                                          :document-types
                                                          (list (list :document-type (list :name "statistics"
                                                                                           :label "Statistics"
                                                                                           :elements
                                                                                           (list (list :element
                                                                                                       (list :name :id
                                                                                                             :label "Id"
                                                                                                             :concrete-type :string
                                                                                                             :attributes (list (list :attribute
                                                                                                                                     (list :name :display
                                                                                                                                           :value t))
                                                                                                                               (list :attribute
                                                                                                                                     (list :name :editable
                                                                                                                                           :value t))))))
                                                                                           :documentation "Cool statistics."))))))))))))

(defun make-clean-def-5 ()
  (list :multiverse
        (list :name "multiverse"
              :universe-class 'cl-naive-store.naive-core:universe
              :location (test-location)
              :universes
              (list (list :universe
                          (list :name "marvel"
                                :stores
                                (list (list :store
                                            (list :name "wolverine"
                                                  :collections
                                                  (list (list :collection (list :name "stats"
                                                                                :label "Statistics"
                                                                                :document-type "statistics")))
                                                  :document-types
                                                  (list (list :document-type (list :name "statistics"
                                                                                   :label "Statistics"
                                                                                   :elements
                                                                                   (list (list :element
                                                                                               (list :name :id
                                                                                                     :label "Id"
                                                                                                     :concrete-type :string
                                                                                                     :attributes (list (list :attribute
                                                                                                                             (list :name :display
                                                                                                                                   :value t))
                                                                                                                       (list :attribute
                                                                                                                             (list :name :editable
                                                                                                                                   :value t))))))
                                                                                   :documentation "Cool statistics."))))))))))))

(cl-naive-tests:define-suite (:test-definitions)
  (cl-naive-tests:testcase
   :load-multiverse-definition
   :expected t
   :actual (progn
             (setf *multiverse* (cl-naive-store.naive-core:load-from-definition
                                 nil
                                 :multiverse
                                 *multiverse-definition*
                                 :with-children-p t
                                 :with-data-p nil))
             (persist *multiverse* :definitions-only-p t)
             (when *multiverse*
               t)))
  (cl-naive-tests:testcase
   :probe-multiverse
   :expected t
   :actual (when (probe-file
                  (cl-fad:merge-pathnames-as-directory
                   (location *multiverse*)
                   (make-pathname
                    :name (name *multiverse*)
                    :type "multiverse")))
             t))
  (cl-naive-tests:testcase
   :probe-store
   :expected t
   :actual
   (let* ((universe (get-multiverse-element
                     :universe *multiverse* "universe"))
          (store (get-multiverse-element
                  :store universe "human-resources")))

     (when (probe-file
            (cl-fad:merge-pathnames-as-directory
             (location store)
             (make-pathname
              :name (name store)
              :type "store")))
       t)))
  (cl-naive-tests:testcase
   :probe-collection-1
   :expected t
   :actual
   (let* ((universe (get-multiverse-element
                     :universe *multiverse* "universe"))
          (store (get-multiverse-element
                  :store universe "human-resources"))
          (collection (get-multiverse-element
                       :collection store "employees")))

     ;;Not testing for log files just definitions
     (when (probe-file
            (cl-fad:merge-pathnames-as-directory
             (location store)
             (make-pathname
              :name (name collection)
              :type "col")))
       t)))
  (cl-naive-tests:testcase
   :probe-collection-2
   :expected t
   :actual
   (let* ((universe (get-multiverse-element
                     :universe *multiverse* "universe"))
          (store (get-multiverse-element
                  :store universe "human-resources"))
          (collection (get-multiverse-element
                       :collection store "laptops")))
     ;;Not testing for log files just definitions
     (when (probe-file
            (cl-fad:merge-pathnames-as-directory
             (location store)
             (make-pathname
              :name (name collection)
              :type "col")))
       t)))
  (cl-naive-tests:testcase
   :probe-document-type
   :expected t
   :actual
   (let* ((universe (get-multiverse-element
                     :universe *multiverse* "universe"))
          (store (get-multiverse-element
                  :store universe "human-resources"))
          (document-type (get-multiverse-element
                          :document-type store "employee")))
     (when (probe-file
            (cl-fad:merge-pathnames-as-directory
             (location store)
             (make-pathname
              :name (name document-type)
              :type "type")))
       t)))

  (cl-naive-tests:testcase
   :add-universe
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics."))))))))
       (:UNIVERSE
        (:NAME "movies" :STORES
               ((:STORE
                 (:NAME "super-heros" :COLLECTIONS
                        ((:COLLECTION (:NAME "movies" :LABEL "Movies" :DOCUMENT-TYPE "movie")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "movie" :LABEL "Movies" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T)))))
                                  :DOCUMENTATION "Cool movie."))))))))))))
   :actual
   (let ((definition (make-clean-def-1)))
     (setf *multiverse* nil)
     (setf *universe* nil)
     (setf *multiverse* (make-instance
                         'multiverse
                         :name "multiverse"
                         :location (test-location) ;Setting the location on disk.
                         :universe-class 'universe))

     (cl-naive-store.naive-core:add-definition-element
      :universe
      definition
      '(:universe
        (:name "movies"
         :stores ((:store
                   (:name "super-heros"
                    :collections
                    ((:collection
                      (:name "movies"
                       :label "Movies"
                       :document-type "movie")))
                    :document-types
                    ((:document-type (:name "movie"
                                      :label "Movies"
                                      :elements
                                            ((:element
                                              (:name :id
                                               :label "Id"
                                               :concrete-type :string
                                               :attributes ((:attribute
                                                             (:name :display
                                                              :value t))
                                                            (:attribute
                                                             (:name :editable
                                                              :value t)))))
                                             :documentation "Cool movie.")))))))))
      :name-path '((:multiverse "multiverse")))))
  (cl-naive-tests:testcase
   :remove-universe
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))))))))
   :actual
   (cl-naive-store.naive-core::remove-definition-element
    :universe
    (make-clean-def-2)
    "movies"
    :name-path '((:multiverse "multiverse"))))
  (cl-naive-tests:testcase
   :add-store
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))
                (:STORE
                 (:NAME "super-heros" :COLLECTIONS
                        ((:COLLECTION (:NAME "movies" :LABEL "Movies" :DOCUMENT-TYPE "movie")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "movie" :LABEL "Movies" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool movie.")))))))))))
   :actual
   (cl-naive-store.naive-core:add-definition-element
    :store
    (make-clean-def-3)
    '(:store (:name "super-heros"
              :collections
              ((:collection (:name "movies"
                             :label "Movies"
                             :document-type "movie")))
              :document-types
              ((:document-type (:name "movie"
                                :label "Movies"
                                :elements
                                ((:element
                                  (:name :id
                                   :label "Id"
                                   :concrete-type :string
                                   :attributes ((:attribute
                                                 (:name :display
                                                  :value t))
                                                (:attribute
                                                 (:name :editable
                                                  :value t))))))
                                :documentation "Cool movie.")))))
    :name-path '((:universe "marvel"))))
  (cl-naive-tests:testcase
   :remove-store
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))))))))
   :actual
   (cl-naive-store.naive-core::remove-definition-element
    :store
    `(:multiverse
      (:name "multiverse"
       :universe-class cl-naive-store.naive-core:universe
       :location ,(test-location)
       :universes
       ((:universe
         (:name "marvel"
          :stores
                ((:store
                  (:name "wolverine"
                   :collections
                         ((:collection
                           (:name "stats"
                            :label "Statistics"
                            :document-type "statistics")))
                   :document-types
                         ((:document-type (:name "statistics"
                                           :label "Statistics"
                                           :elements
                                                 ((:element
                                                   (:name :id
                                                    :label "Id"
                                                    :concrete-type :string
                                                    :attributes ((:attribute
                                                                  (:name :display
                                                                   :value t))
                                                                 (:attribute
                                                                  (:name :editable
                                                                   :value t))))))
                                           :documentation "Cool statistics.")))))
                 (:store (:name "super-heros"
                          :collections
                                ((:collection (:name "movies"
                                               :label "Movies"
                                               :document-type "movie")))
                          :document-types
                                ((:document-type (:name "movie"
                                                  :label "Movies"
                                                  :elements
                                                        ((:element
                                                          (:name :id
                                                           :label "Id"
                                                           :concrete-type :string
                                                           :attributes ((:attribute
                                                                         (:name :display
                                                                          :value t))
                                                                        (:attribute
                                                                         (:name :editable
                                                                          :value t))))))
                                                  :documentation "Cool movie.")))))))))))
    "super-heros"
    :name-path '((:universe "marvel"))))
  (cl-naive-tests:testcase
   :add-document-type
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics."))
                         (:DOCUMENT-TYPE
                          (:NAME "relation" :LABEL "Relation" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Who's who.")))))))))))
   :actual
   (cl-naive-store.naive-core:add-definition-element
    :document-type
    (make-clean-def-5)
    '(:document-type (:name "relation"
                      :label "Relation"
                      :elements
                      ((:element
                        (:name :id
                         :label "Id"
                         :concrete-type :string
                         :attributes ((:attribute
                                       (:name :display
                                        :value t))
                                      (:attribute
                                       (:name :editable
                                        :value t))))))
                      :documentation "Who's who."))
    :name-path '((:universe "marvel") (:store "wolverine"))))
  (cl-naive-tests:testcase
   :remove-document-type
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))))))))
   :actual
   (cl-naive-store.naive-core::remove-definition-element
    :document-type
    `(:multiverse
      (:name "multiverse"
       :universe-class cl-naive-store.naive-core:universe
       :location ,(test-location)
       :universes
       ((:universe
         (:name "marvel"
          :stores ((:store
                    (:name "wolverine"
                     :collections
                           ((:collection
                             (:name "stats"
                              :label "Statistics"
                              :document-type "statistics")))
                     :document-types
                           ((:document-type (:name "statistics"
                                             :label "Statistics"
                                             :elements
                                                   ((:element
                                                     (:name :id
                                                      :label "Id"
                                                      :concrete-type :string
                                                      :attributes ((:attribute
                                                                    (:name :display
                                                                     :value t))
                                                                   (:attribute
                                                                    (:name :editable
                                                                     :value t))))))
                                             :documentation "Cool statistics."))
                            (:document-type (:name "relation"
                                             :label "Relation"
                                             :elements
                                                   ((:element
                                                     (:name :id
                                                      :label "Id"
                                                      :concrete-type :string
                                                      :attributes ((:attribute
                                                                    (:name :display
                                                                     :value t))
                                                                   (:attribute
                                                                    (:name :editable
                                                                     :value t))))))
                                             :documentation "Who's who.")))))))))))

    "relation"
    :name-path '((:universe "marvel") (:store "wolverine"))))
  (cl-naive-tests:testcase
   :add-collection
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics"))
                         (:COLLECTION
                          (:NAME "attitude" :LABEL "Attitude" :DOCUMENT-TYPE "attitude")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))))))))
   :actual
   (cl-naive-store.naive-core:add-definition-element
    :collection
    (make-clean-def-4)
    '(:collection (:name "attitude" :label "Attitude" :document-type "attitude"))
    :name-path '((:universe "marvel") (:store "wolverine"))))
  (cl-naive-tests:testcase
   :remove-collection
   :expected
   `(:MULTIVERSE
     (:NAME "multiverse" :UNIVERSE-CLASS CL-NAIVE-STORE.NAIVE-CORE:UNIVERSE
      :LOCATION ,(test-location) :UNIVERSES
      ((:UNIVERSE
        (:NAME "marvel" :STORES
               ((:STORE
                 (:NAME "wolverine" :COLLECTIONS
                        ((:COLLECTION
                          (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics")))
                  :DOCUMENT-TYPES
                        ((:DOCUMENT-TYPE
                          (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                                 ((:ELEMENT
                                   (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                          ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                           (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                           :DOCUMENTATION "Cool statistics.")))))))))))
   :actual
   (cl-naive-store.naive-core::remove-definition-element
    :collection
    `(:multiverse
      (:name "multiverse"
       :universe-class cl-naive-store.naive-core:universe
       :location ,(test-location)
       :universes
       ((:universe
         (:name "marvel"
          :stores ((:store
                    (:name "wolverine"
                     :collections
                           ((:collection (:name "stats"
                                          :label "Statistics"
                                          :document-type "statistics"))
                            (:collection (:name "attitude"
                                          :label "Attitude"
                                          :document-type "attitude")))
                     :document-types
                           ((:document-type (:name "statistics"
                                             :label "Statistics"
                                             :elements
                                                   ((:element
                                                     (:name :id
                                                      :label "Id"
                                                      :concrete-type :string
                                                      :attributes ((:attribute
                                                                    (:name :display
                                                                     :value t))
                                                                   (:attribute
                                                                    (:name :editable
                                                                     :value t))))))
                                             :documentation "Cool statistics.")))))))))))
    "attitude"
    :name-path '((:universe "marvel") (:store "wolverine")))))

(defmethod cl-naive-tests:tear-down-suite ((test-name (eql :test-definitions)))
  (setf *universe* nil)
  (setf *multiverse* nil)
  (setf *multiverse-definition* (make-clean-definition)))

;;(cl-naive-tests:run :suites :test-definitions)
