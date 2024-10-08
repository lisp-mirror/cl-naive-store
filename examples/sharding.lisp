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

;;Required to correctly initialize lparallel:*kerel*.
(initialize)

(defparameter *surnames*
  #("Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller")
  "A vector of surnames.")

(defparameter *countries*
  #("Afghanistan" "Albania" "Algeria" "Andorra" "Angola"
    "Antigua and Barbuda" "Argentina" "Armenia" "Australia" "Austria"
    "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus"
    "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia and Herzegovina"
    "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina Faso" "Burundi"
    "Côte d'Ivoire" "Cabo Verde" "Cambodia" "Cameroon" "Canada"
    "Central African Republic" "Chad" "Chile" "China" "Colombia" "Comoros"
    "Congo" "Costa Rica" "Croatia" "Cuba" "Cyprus"
    "Czechia" "Democratic Republic of the Congo"
    "Denmark" "Djibouti" "Dominica" "Dominican Republic" "Ecuador" "Egypt"
    "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia"
    "Eswatini" "Ethiopia" "Fiji" "Finland" "France"
    "Gabon" "Gambia" "Georgia" "Germany" "Ghana" "Greece" "Grenada"
    "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Holy See"
    "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq"
    "Ireland" "Israel" "Italy" "Jamaica" "Japan" "Jordan" "Kazakhstan"
    "Kenya" "Kiribati" "Kuwait" "Kyrgyzstan" "Laos" "Latvia" "Lebanon"
    "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg"
    "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta"
    "Marshall Islands" "Mauritania" "Mauritius" "Mexico" "Micronesia"
    "Moldova" "Monaco" "Mongolia" "Montenegro" "Morocco" "Mozambique"
    "Myanmar" "Namibia" "Nauru" "Nepal" "Netherlands"
    "New Zealand" "Nicaragua" "Niger" "Nigeria" "North Korea"
    "North Macedonia" "Norway" "Oman" "Pakistan" "Palau" "Palestine State"
    "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland"
    "Portugal" "Qatar" "Romania" "Russia" "Rwanda" "Saint Kitts and Nevis"
    "Saint Lucia" "Saint Vincent and the Grenadines" "Samoa" "San Marino"
    "Sao Tome and Principe" "Saudi Arabia" "Senegal" "Serbia" "Seychelles"
    "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Islands"
    "Somalia" "South Africa" "South Korea" "South Sudan" "Spain"
    "Sri Lanka" "Sudan" "Suriname" "Sweden" "Switzerland" "Syria"
    "Tajikistan" "Tanzania" "Thailand" "Timor-Leste" "Togo" "Tonga"
    "Trinidad and Tobago" "Tunisia" "Turkey" "Turkmenistan" "Tuvalu"
    "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom"
    "United States of America" "Uruguay" "Uzbekistan" "Vanuatu"
    "Venezuela" "Vietnam" "Yemen" "Zambia" "Zimbabwe")
  "A vector of country names.")

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

;;Cannot go below 202! The tests will fail.
(defparameter *size* 10000)

(defparameter *expected-shard-count* 0)

;;Doing this so you can rerun the code from here onwards.
(progn
  ;;Deleting existing example database
  (cl-fad:delete-directory-and-files
   "~/multiverse/universe/simple-store"
   :if-does-not-exist :ignore)

  (let* ((multiverse
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
                          :name(getf (getf element :element) :name)
                          :key-p (getf (getf element :element) :key-p)
                          :concrete-type (getf (getf element :element) :concrete-type)
                          :attributes (getf (getf element :element) :attributes)))
                       (getf
                        (getf *employee-document-type* :document-type)
                        :elements))))
         (collection
           (add-multiverse-element
            store
            (make-instance (collection-class store)
                           :name "simple-collection"
                           :keys '(:emp-no)
                           :document-type document-type
                           ;; Creating shards based on the country that the employee
                           ;; belongs to.  It is a bad example you should not shard on
                           ;; any value that could change in the future!
                           :shard-elements '(:country))))
         (emp-country  0)
         (emp-surname  0)
         (unique-countries (make-hash-table :test 'equalp)))

    (setf *expected-shard-count* 0)

    (cl-naive-store.naive-core:add-multiverse-element store document-type)

    (persist multiverse :definitions-only-p t)

    (unless (data-loaded-p collection)
      ;;Populate the collection.
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

          (add-document collection
                        (list
                         :country country
                         :surname surname
                         :name (format nil "Slave No ~A" emp-no)
                         :emp-no emp-no)
                        :handle-duplicates-p nil)))

      ;; Bulk perist documents
      (persist-collection collection))

    ;;We are just going to investigate the shards and not do a lot data
    ;;lookups like in other examples.
    (list (list :desc "Count of shards in collection."
                :value (length (shards collection)))
          (list :desc "The count of physical shard files for the collection."
                :value (length (uiop:directory-files
                                (cl-fad:merge-pathnames-as-directory
                                 (location store)
                                 (make-pathname :directory
                                                (list :relative
                                                      (name collection))))))))))
