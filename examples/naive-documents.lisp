(ignore-errors (delete-package :naive-examples))

;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use
                             :cl
                             :cl-getx :cl-naive-store.naive-core
                             :cl-naive-store.naive-indexed
                             :cl-naive-store.document-types
                             :cl-naive-store.naive-documents))

(in-package :naive-examples)

;;Required to correctly initialize lparallel:*kernel*.
(initialize)

;;Deleting existing example database
(cl-fad:delete-directory-and-files
 "~/multiverse/universe/simple-store"
 :if-does-not-exist :ignore)

;;Create a data definition for an employee
;;It looks like a lot but dont panic its simple.
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

(let* (;;Create Multiverse
       (multiverse
         (make-instance
          'multiverse
          :name "multiverse"
          :location "~/multiverse/" ;Setting the location on disk.
          :universe-class 'universe))
       ;;Add universe to multiverse
       (universe
         (add-multiverse-element
          multiverse
          (make-instance
           'universe
           :name "universe"
           :multiverse multiverse
           :location "~/multiverse/universe/" ;Setting the location on disk.
           :store-class 'document-store)))
       (store
         (add-multiverse-element
          universe
          (make-instance (store-class universe)
                         :name "simple-store"
                         :collection-class
                         'cl-naive-store.naive-documents:document-collection)))
       (document-type
         (load-from-definition
          store
          :document-type
          *employee-document-type*
          :with-children-p t))
       (collection (add-multiverse-element
                    store
                    (make-instance (collection-class store)
                                   :name "simple-collection"
                                   :document-type document-type
                                   ;; Not specifying the keys to show
                                   ;; that they are retrieved from the document-type
                                   ;; if if no key is set.
                                   ;; :keys ...
                                   ;; Specifying the elements to set up indexes for.
                                   :indexes '((:name :surname)))))
       ;;Add doc to collection
       (doc (persist-document collection
                              (make-document
                               :store (store collection)
                               :collection collection
                               :document-type "employee"
                               :elements (list :name "Piet" :surname "Gieter"
                                               :emp-no 123))))
       (results))

  ;;Persist store definition with its document type and collection.
  (persist multiverse :definitions-only-p t)

  ;;Add some documents to the collection

  (persist-document collection
                    (make-document
                     :store (store collection)
                     :collection collection
                     :document-type "employee"
                     :elements (list :name "Sannie" :surname "Gieter" :emp-no 321)))

  (persist-document collection
                    (make-document
                     :store (store collection)
                     :collection collection
                     :document-type "employee"
                     :elements (list :name "Koos" :surname "Van" :emp-no 999)))

  (persist-document collection
                    (make-document
                     :store (store collection)
                     :collection collection
                     :document-type "employee"
                     :elements (list :name "Frikkie" :surname "Frikkedel" :emp-no 1001)))

  (persist-document collection
                    (make-document
                     :store (store collection)
                     :collection collection
                     :document-type "employee"
                     :elements (list :name "Tannie" :surname "Frikkedel" :emp-no 1002)))

  ;;Look up piet by hash
  (push (list :desc "Looked up Piet using index-lookup-hash."
              :value (index-lookup-hash collection (getx doc :hash)))
        results)

  ;;Lookup koos using index values and add it to results
  (push
   (list :desc "Koos that we looked up using index-lookup-values and the index values of Koos and Van."
         :value
         (index-lookup-values collection (list (list :name "Koos")
                                               (list :surname "Van"))))
   results)

  ;;Lookup Frikkedel using index values and add it to results
  (push
   (list :desc "A list of both Frikie and Tannie that we looked up using index-lookup-values and the surname. This is called a partial index lookup. You can enable or disable partial indexes."
         :value
         (index-lookup-values collection (list :surname "Frikkedel")))
   results)

  ;;Query the collection, query-data will load the data from file if
  ;;the collection is empty, and add it to the results
  (push
   (list :desc "Queried all id's <= 900 using query-data. The query will use indexes internally when possible."
         :value
         (query-data collection :query (lambda (document)
                                         (<= (getx document :emp-no) 900))))
   results)

  (let ((sannie (first (index-lookup-values
                        collection
                        (list (list :name "Sannie")
                              (list :surname "Gieter"))))))

    (setf (getx sannie :surname) "Potgieter")

    (push (list :desc "To change a value for an employee you just set the value using getx. For example lets change Sannie's surname to Potgieter. We did not persist the change. If we persisted the document elements will be set to changes and changes set to nil."
                :value sannie)
          results))

  (reverse results))

