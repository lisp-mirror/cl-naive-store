(require 'cl-naive-store)

;; SBCL is idiotic again, it signals an error when compiling a file
;; containing this delete-package form.  You'll have to delete the
;; package yourself between the various examples or tests loads.
#-sbcl (ignore-errors (delete-package :naive-examples))

(defpackage :naive-examples
  (:use :cl
   :cl-getx :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed :cl-naive-store.document-types
   :cl-naive-store.document-type-defs :cl-naive-store.naive-documents))
(in-package :naive-examples)

;; To be able to have reproducible and checkable tests, we'll use a
;; deterministic way to generate employee attributes.  For this, we
;; use those two vectors, and we ensure that the GCD of their length
;; is 1 for maximal period.
;; So we can generate:
;; (* (length *countries*) (length  *surnames*))
;; = 1365 distinct combinations.

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

(defparameter *surnames*
  #("Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller")
  "A vector of surnames.")

(eval-when (:load-toplevel :execute)
  (assert (= 1 (gcd (length *countries*) (length  *surnames*)))))

;;;
;;; Universe
;;;

(defvar *universe* nil)

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (user-homedir-pathname)
   (make-pathname :directory (list :relative "test-universe"))))

(defun tear-down-universe ()
  "Deletes any peristed data from examples."
  (cl-fad:delete-directory-and-files
   (if *universe*
       (location *universe*)
       (test-location))
   :if-does-not-exist :ignore))

(defparameter *universe*
  (progn
    (tear-down-universe)
    (make-instance 'universe
		   :location (test-location)
		   :store-class 'document-store)))

;;;
;;; Utilities
;;;

(defun element (name label &key (type :string) (keyp nil))
  "A little helper function to generate simple document types elements."
  `(:name ,name
    :label ,label
    :concrete-type ,type
    :key-p ,keyp
    :attributes (:display t :editable t)))

(defun mkelements (document-type-description)
  "Convert the elements of the document-type into a list of ELEMENT instances."
  (mapcar (lambda (element)
	    (make-instance 'element
			   :name       (getf element :name)
			   :key-p      (getf element :key-p)
			   :concrete-type   (getf element :concrete-type)
			   :attributes (getf element :attributes)))
	  (getf document-type-description :elements)))

(defun make-document-type (document-type-description elements)
  "Convert the DOCUMENT-TYPE-DESCRIPTION and its ELEMENTS into a DOCUMENT-TYPE instance."
  (make-instance 'document-type
		 :name  (getf document-type-description :name)
		 :label (getf document-type-description :label)
		 :elements elements))

;;;
;;; Store
;;;

(defparameter *store*
  (add-store *universe*
	     (make-instance (store-class *universe*)
			    :name "simple-store"
       			    :collection-class 'document-collection)))

;;;
;;; Employees
;;;

(defparameter *employee-document-type-description*
  `(:name "employee"
    :label "Employee"
    :elements ,(list
		(element :emp-no   "Employee No"   :keyp   t)
		(element :country  "Country")
		(element :name     "Name")
		(element :surname  "Surname")
		(element :asset    "Asset"
			 :type '(:type :document
				 :complex-type :document
				 :elements)))
    :documentation "This type represents a simple employee master.")
  "Data definition for an employee.")

(defparameter *employee-elements*
  (mkelements *employee-document-type-description*))

(defparameter *employee-document-type*
  (add-document-type *store*
		     (make-document-type *employee-document-type-description*
					 *employee-elements*)))

(defparameter *employee-collection*
  (add-collection *store*
		  (make-instance (collection-class *store*)
				 :name "simple-collection"
				 :document-type *employee-document-type*
				 :keys '(:emp-no)
				 :indexes '((:surname))
				 ;; Creating shards based on the country that the employee
				 ;; belongs to.  It is a bad example you should not shard on
				 ;; any value that could change!
				 :shard-elements (list :country))))

;;;
;;; Assets
;;;

(defparameter *asset-document-type-description*
  `(:name "asset-register"
    :label "Asset Register"
    :elements ,(list (element :asset-no "Asset No" :keyp t)
		     (element :description "Description"))
    :documentation "This type represents a simple employee master.")
  "Data definition for an asset. Assets are linked to employees")

(defparameter *asset-elements*
  (mkelements *asset-document-type-description*))

(defparameter *asset-document-type*
  (add-document-type *store*
		     (make-document-type *asset-document-type-description*
					 *asset-elements*)))

(defparameter *asset-collection*
  (add-collection *store* (make-instance (collection-class *store*)
					 :name "asset-collection"
					 :document-type *asset-document-type*
					 :keys '(:asset-no))))

(let ((results      '())
      (emp-country  0)
      (emp-surname  0)
      (size         100000))

  ;; Try to load the data first, maybe it has been persisted before.
  (format *trace-output* "~&Loading Existing Data.~%")
  (time (load-data *employee-collection*))

  ;; If the data was peristed before and successfully loaded dont add it again.
  (unless (data-loaded-p *employee-collection*)

    ;; Adding documents without persisting will do a bulk persist later which is much faster.
    (format *trace-output* "~&Adding ~D documents to collections~%" size)
    (time
     (dotimes (emp-no size)
       ;; We create employees country per country and loop again if we need more:
       (let ((country  (aref *countries* emp-country))
	     (surname  (aref *surnames*  emp-surname)))
	 (incf emp-surname)
	 (when (<= (length *surnames*) emp-surname)
	   (setf emp-surname 0)
	   (setf emp-country (mod (1+ emp-country) (length *countries*))))
	 (add-document *employee-collection*
		       (make-document
			:store (store *employee-collection*)
			:collection *employee-collection*
			:document-type *employee-document-type*
			:elements (list
				   :asset (add-document *asset-collection*
							(make-document
							 :store (store *asset-collection*)
							 :collection *asset-collection*
							 :document-type *asset-document-type*
							 :elements (list :description emp-no
									 :asset-no emp-no)))
				   :country country
				   :surname surname
				   :name (format nil "Slave No ~A" emp-no)
				   :emp-no emp-no))))))

    ;; Bulk Persist assets
    (format *trace-output* "~&Persisting ~D assets to collections~%" size)
    (time (persist *asset-collection*))

    ;; Bulk Persist employees
    (format *trace-output* "~&Persisting ~D employees to collections~%" size)
    (time (persist *employee-collection*)))

  (format *trace-output* "~&Doing a straight up query that touches each record.~%")
  (let ((data (time (query-data *employee-collection*
				:query (lambda (document)
					 (or (and ; 51
					      (>= (getx document :emp-no) 50)
					      (<= (getx document :emp-no) 100))
					     (and ; 101
					      (>= (getx document :emp-no) (/ size 2))
					      (<= (getx document :emp-no) (+ (/ size 2) 100)))
					     (and ; 50 : we don't have an emp-no = size, max is size-1
					      (>= (getx document :emp-no) (- size 50))
					      (<= (getx document :emp-no) size))))))))
    (assert (= 202 (length data)))
    (push (list :query-all (length data)) results))

  (format *trace-output* "~&Fetching an index set.~%")
  (let ((data (time (query-data *employee-collection*
				:index-values (list (list :surname "Davis")))))
	(expected (multiple-value-bind (n r) (truncate size (length *surnames*))
		    (+ n
		       (if (<= r (position "Davis" *surnames* :test (function string-equal)))
			   0
			   1)))))	; 14285
    (assert (= expected (length data)))
    (push (list :how-many-davises? (length data)) results))

  (format *trace-output* "~&Doing a query against an index set.~&")
  (let ((data (time (query-data *employee-collection*
				:query (lambda (emp)
					 (string-equal (getx emp :country) "Chile"))
				:index-values (list (list :surname "Davis")))))
	(expected (multiple-value-bind (n r) (truncate size (* (length *countries*) (length *surnames*)))
		    (+ n
		       (if (< r
			      (+ (* (length *surnames*) (position "Chile" *countries* :test (function string-equal)))
				 (position "Davis" *surnames* :test (function string-equal))))
			   0
			   1)))))	; 74
    (assert (= expected (length data)))
    (push (list :how-many-davises-in-chile? (length data)) results))

  (print :success)
  (pprint results))

