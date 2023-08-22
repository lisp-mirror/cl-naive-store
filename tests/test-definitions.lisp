(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl :cl-getx :cl-naive-store.definitions :cl-naive-store.naive-documents))
(in-package :naive-examples)

(defvar *universe* nil)

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "test-universes"))))

(defvar *universe-definitions*
  `(:multiverse
    ((:universe
      (:name "test-universe"
       :location ,(test-location)
       :universe-class cl-naive-store.naive-core:universe
       :store-class cl-naive-store.naive-documents:document-store
       :collection-class cl-naive-store.naive-documents:document-collection
       :stores ((:store
                 (:name "human-resources"
                  :collections ((:collection
                                 (:name "laptop"
                                  :label "Laptop"
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
                           :documentation "List of laptops the company owns.")))))))))))

(defparameter *universes* nil)

(defun tear-down-universe ()
  "Deletes any peristed data from examples."
  (cl-fad:delete-directory-and-files
   "~/temp/test-universe/"
   :if-does-not-exist :ignore))

(defun test-something (test-lambda expected-result error-message results
                       &key (test #'equalp))
  (let ((result (funcall test-lambda)))
    (assert (funcall test result expected-result)
            ()
            (format nil "~A~%~S" error-message result))
    (push results result)
    results))

(let ((results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:add-universe
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
       '(:multiverse
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
                                               :documentation "Cool statistics."))))))))))))
    '(:MULTIVERSE
      ((:UNIVERSE
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
                            :DOCUMENTATION "Cool movie.")))))))))
       (:UNIVERSE
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
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to add universe."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:remove-universe
       "movies"
       '(:multiverse
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
                                               :documentation "Cool statistics."))))))))
          (:universe
           (:name "movies"
            :stores ((:store (:name "super-heros"
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
                                                      :documentation "Cool movie."))))))))))))
    '(:MULTIVERSE
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
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to remove universe."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:add-store
       "marvel"
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
       '(:multiverse
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
                                               :documentation "Cool statistics."))))))))))))
    '(:MULTIVERSE
      ((:UNIVERSE
        (:NAME "marvel" :STORES
         ((:STORE
           (:NAME "super-heros" :COLLECTIONS
                  ((:COLLECTION (:NAME "movies" :LABEL "Movies" :DOCUMENT-TYPE "movie")))
            :DOCUMENT-TYPES
                  ((:DOCUMENT-TYPE
                    (:NAME "movie" :LABEL "Movies" :ELEMENTS
                           ((:ELEMENT
                             (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                    ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                     (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                     :DOCUMENTATION "Cool movie.")))))
          (:STORE
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
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to add store."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:remove-store
       "marvel"
       "super-heros"
       '(:multiverse
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
                                                      :documentation "Cool movie."))))))))))))
    '(:MULTIVERSE
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
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to remove store."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:add-document-type
       "marvel" "wolverine"
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
       '(:multiverse
         ((:universe
           (:name "marvel"
            :stores ((:store
                      (:name "wolverine"
                       :collections
                             ((:collection (:name "stats"
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
                                               :documentation "Cool statistics."))))))))))))
    '(:MULTIVERSE
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
                     :DOCUMENTATION "Cool statistics."))))
           :DOCUMENT-TYPES
           ((:DOCUMENT-TYPE
             (:NAME "relation" :LABEL "Relation" :ELEMENTS
                    ((:ELEMENT
                      (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                             ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                              (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
              :DOCUMENTATION "Who's who.")))))))))
    "Failed to add document-type."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:remove-document-type
       "marvel" "wolverine" "relation"
       '(:multiverse
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
                                               :documentation "Who's who."))))))))))))
    '(:MULTIVERSE
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
                     :DOCUMENTATION "Who's who."))))
           :DOCUMENT-TYPES NIL))))))
    "Failed to remove document-type."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:add-collection
       "marvel" "wolverine"
       '(:collection (:name "attitude" :label "Attitude" :document-type "attitude"))
       '(:multiverse
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
                                               :documentation "Cool statistics."))))))))))))
    '(:MULTIVERSE
      ((:UNIVERSE
        (:NAME "marvel" :STORES
         ((:STORE
           (:NAME "wolverine" :COLLECTIONS
                  ((:COLLECTION
                    (:NAME "stats" :LABEL "Statistics" :DOCUMENT-TYPE "statistics"))
                   :COLLECTION
                   (:NAME "attitude" :LABEL "Attitude" :DOCUMENT-TYPE "attitude"))
            :DOCUMENT-TYPES
                  ((:DOCUMENT-TYPE
                    (:NAME "statistics" :LABEL "Statistics" :ELEMENTS
                           ((:ELEMENT
                             (:NAME :ID :LABEL "Id" :CONCRETE-TYPE :STRING :ATTRIBUTES
                                    ((:ATTRIBUTE (:NAME :DISPLAY :VALUE T))
                                     (:ATTRIBUTE (:NAME :EDITABLE :VALUE T))))))
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to add collection."
    results))

  (setf
   results
   (test-something
    (lambda ()
      (cl-naive-store.definitions:remove-collection
       "marvel" "wolverine" "attitude"
       '(:multiverse
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
                                               :documentation "Cool statistics."))))))))))))
    '(:MULTIVERSE
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
                     :DOCUMENTATION "Cool statistics."))))))))))
    "Failed to add collection."
    results))

  ;;TODO: Add more tests to check if directories exist with definition files etc.
  (setf *universes* (cl-naive-store.definitions::create-multiverse
                     *universe-definitions* t))

  (assert (= 1 (length *universes*)))

  (push *universes* results)

  (print :success)
  (pprint (reverse results)))

