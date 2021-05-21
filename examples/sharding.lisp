;;Setup to use cl-naive-store
(require 'cl-naive-store)
(defpackage :naive-examples (:use
			     :cl
			     :cl-getx :cl-naive-store.naive-core
			     :cl-naive-store.naive-indexed
			     :cl-naive-store.document-types
			     :cl-naive-store.document-type-defs
			     :cl-naive-store.naive-documents))
(in-package :naive-examples)

;;A helper function
(defun random-from-list (list)
  (nth (random (- (length list) 1)) list))

;;Create a data definition for an asset. Assets are linked to employees
;;It looks like a lot but dont panic its simple.
(defparameter *asset-document-type*
  '(:name "asset-register"
    :label "Asset Register"
    :elements ((:name :asset-no
		:label "Asset No"
		:concrete-type :string
		:key-p t
		:attributes (:display t :editable t))
	       (:name :description
		:label "Description"
		:concrete-type :string
		:attributes (:display t :editable t)))
    :documentation "This type represents a simple employee master."))

;;Create a data definition for an employee
;;It looks like a lot but dont panic its simple.
(defparameter *employee-document-type*
  '(:name "employee"
    :label "Employee"
    :elements ((:name :emp-no
		:label "Employee No"
		:concrete-type :string
		:key-p t
		:attributes (:display t :editable t))
	       (:name :country
		:label "Country"
		:concrete-type :string
		:attributes (:display t :editable t))
	       (:name :name
		:label "Name"
		:concrete-type :string
		:attributes (:display t :editable t))
	       (:name :surname
		:label "Surname"
		:concrete-type :string
		:attributes (:display t :editable t))
	       (:name :asset
		:label "Asset"
		:concrete-type (:type :document
				:complex-type :document
				:elements)
		:attributes (:display t :editable t)))
    :documentation "This type represents a simple employee master."))

;;Create a universe
(defparameter *universe* (make-instance
			  'universe
			  :location "~/data-universe/" ;Setting the location on disk.
			  :store-class 'store))

(let* (;;Create a store and add it to the universe
       (store (add-store *universe*
			 (make-instance 'document-store
					:name "simple-store"
       					:collection-class 'collection)))
       (employee-collection)
       (asset-collection)
       (employee-elements)
       (employee-document-type)
       (asset-elements)
       (asset-document-type)
       (results))

  ;;initialize the data employee data definition.
  (dolist (element (getf *employee-document-type* :elements))
    (setf
     employee-elements
     (append employee-elements
	     (list (make-instance
		    'element
		    :name (getf element :name)
		    :key-p (getf element :key-p)
		    :concrete-type (getf element :concrete-type)
		    :attributes (getf element :attributes))))))

  (setf employee-document-type (add-document-type
				store
				(make-instance
				 'document-type
				 :name (getf *employee-document-type* :name)
				 :label (getf *employee-document-type* :label)
				 :elements employee-elements)))

  ;;initialize the data asset data definition.
  (dolist (element (getf *asset-document-type* :elements))
    (setf
     asset-elements
     (append asset-elements
	     (list (make-instance
		    'element
		    :name (getf element :name)
		    :key-p (getf element :key-p)
		    :concrete-type (getf element :concrete-type)
		    :attributes (getf element :attributes))))))

  (setf asset-document-type (add-document-type
			     store
			     (make-instance
			      'document-type
			      :name (getf *asset-document-type* :name)
			      :label (getf *asset-document-type* :label)
			      :elements asset-elements)))

  ;;Create a collection and add it to the store
  (setf employee-collection
	(add-collection store
			(make-instance 'document-collection ;;using documents collection.
				       :name "simple-collection"
				       :document-type employee-document-type
				       :keys '(:emp-no)
				       :indexes '((:surname))
				       ;;Creating shards based on the country that the employee
				       ;;belongs to. It is a bad example you should not shard on
				       ;;any value that could change!!!!!
				       :shard-elements (list :country))))

  ;;Create a collection and add it to the store
  (setf asset-collection
	(add-collection store
			(make-instance 'document-collection
				       :name "asset-collection"
				       :document-type asset-document-type
				       :keys '(:asset-no))))

  ;;Add some documents to the collections
  (let ((emp-country '("Afghanistan"
		       "Albania"
		       "Algeria"
		       "Andorra"
		       "Angola"
		       "Antigua and Barbuda"
		       "Argentina"
		       "Armenia"
		       "Australia"
		       "Austria"
		       "Azerbaijan"
		       "Bahamas"
		       "Bahrain"
		       "Bangladesh"
		       "Barbados"
		       "Belarus"
		       "Belgium"
		       "Belize"
		       "Benin"
		       "Bhutan"
		       "Bolivia"
		       "Bosnia and Herzegovina"
		       "Botswana"
		       "Brazil"
		       "Brunei"
		       "Bulgaria"
		       "Burkina Faso"
		       "Burundi"
		       "Côte d'Ivoire"
		       "Cabo Verde"
		       "Cambodia"
		       "Cameroon"
		       "Canada"
		       "Central African Republic"
		       "Chad"
		       "Chile"
		       "China"
		       "Colombia"
		       "Comoros"
		       "Congo (Congo-Brazzaville)"
		       "Costa Rica"
		       "Croatia"
		       "Cuba"
		       "Cyprus"
		       "Czechia (Czech Republic)"
		       "Democratic Republic of the Congo"
		       "Denmark"
		       "Djibouti"
		       "Dominica"
		       "Dominican Republic"
		       "Ecuador"
		       "Egypt"
		       "El Salvador"
		       "Equatorial Guinea"
		       "Eritrea"
		       "Estonia"
		       "Eswatini (fmr. \"Swaziland\")"
		       "Ethiopia"
		       "Fiji"
		       "Finland"
		       "France"
		       "Gabon"
		       "Gambia"
		       "Georgia"
		       "Germany"
		       "Ghana"
		       "Greece"
		       "Grenada"
		       "Guatemala"
		       "Guinea"
		       "Guinea-Bissau"
		       "Guyana"
		       "Haiti"
		       "Holy See"
		       "Honduras"
		       "Hungary"
		       "Iceland"
		       "India"
		       "Indonesia"
		       "Iran"
		       "Iraq"
		       "Ireland"
		       "Israel"
		       "Italy"
		       "Jamaica"
		       "Japan"
		       "Jordan"
		       "Kazakhstan"
		       "Kenya"
		       "Kiribati"
		       "Kuwait"
		       "Kyrgyzstan"
		       "Laos"
		       "Latvia"
		       "Lebanon"
		       "Lesotho"
		       "Liberia"
		       "Libya"
		       "Liechtenstein"
		       "Lithuania"
		       "Luxembourg"
		       "Madagascar"
		       "Malawi"
		       "Malaysia"
		       "Maldives"
		       "Mali"
		       "Malta"
		       "Marshall Islands"
		       "Mauritania"
		       "Mauritius"
		       "Mexico"
		       "Micronesia"
		       "Moldova"
		       "Monaco"
		       "Mongolia"
		       "Montenegro"
		       "Morocco"
		       "Mozambique"
		       "Myanmar (formerly Burma)"
		       "Namibia"
		       "Nauru"
		       "Nepal"
		       "Netherlands"
		       "New Zealand"
		       "Nicaragua"
		       "Niger"
		       "Nigeria"
		       "North Korea"
		       "North Macedonia"
		       "Norway"
		       "Oman"
		       "Pakistan"
		       "Palau"
		       "Palestine State"
		       "Panama"
		       "Papua New Guinea"
		       "Paraguay"
		       "Peru"
		       "Philippines"
		       "Poland"
		       "Portugal"
		       "Qatar"
		       "Romania"
		       "Russia"
		       "Rwanda"
		       "Saint Kitts and Nevis"
		       "Saint Lucia"
		       "Saint Vincent and the Grenadines"
		       "Samoa"
		       "San Marino"
		       "Sao Tome and Principe"
		       "Saudi Arabia"
		       "Senegal"
		       "Serbia"
		       "Seychelles"
		       "Sierra Leone"
		       "Singapore"
		       "Slovakia"
		       "Slovenia"
		       "Solomon Islands"
		       "Somalia"
		       "South Africa"
		       "South Korea"
		       "South Sudan"
		       "Spain"
		       "Sri Lanka"
		       "Sudan"
		       "Suriname"
		       "Sweden"
		       "Switzerland"
		       "Syria"
		       "Tajikistan"
		       "Tanzania"
		       "Thailand"
		       "Timor-Leste"
		       "Togo"
		       "Tonga"
		       "Trinidad and Tobago"
		       "Tunisia"
		       "Turkey"
		       "Turkmenistan"
		       "Tuvalu"
		       "Uganda"
		       "Ukraine"
		       "United Arab Emirates"
		       "United Kingdom"
		       "United States of America"
		       "Uruguay"
		       "Uzbekistan"
		       "Vanuatu"
		       "Venezuela"
		       "Vietnam"
		       "Yemen"
		       "Zambia"
		       "Zimbabwe"))
	(emp-surnames '("Smith"
			"Johnson"
			"Williams"
			"Jones"
			"Brown"
			"Davis"
			"Miller")))

    ;;Try to load the data first, maybe it has been persisted before.
    (print "Loading Existing Data.")
    (time
     (load-data employee-collection))

    ;;If the data was peristed before and successfully loaded dont add it again.
    (unless (data-loaded-p employee-collection)

      ;;Adding documents without persisting will do a bulk persist later which is much faster.
      (print "Adding 200000 documents to collections")
      (time
       (dotimes (x 100000)

	 (add-document employee-collection
		       (make-document
			:store (store employee-collection)
			:collection employee-collection
			:document-type employee-document-type
			:elements (list
				   :asset (add-document asset-collection
							(make-document
							 :store (store asset-collection)
							 :collection asset-collection
							 :document-type asset-document-type
							 :elements (list :description x :asset-no x)))
				   :country (random-from-list emp-country)
				   :surname (random-from-list emp-surnames)
				   :name (format nil "Slave No ~A" x)
				   :emp-no x)))))

      (print "Persisting 100000 assets to collections")
      (time
       ;;Bulk Persist assets
       (persist asset-collection))

      (print "Persisting 100000 employees to collections")
      (time
       ;;Bulk Persist employees
       (persist employee-collection)))

    (print "Doing a straight up query that touches each record.")
    (time
     (push (list :query-all
		 (length (query-data employee-collection :query
				     (let ((size 100000))
				       (lambda (document)

					 (or (and
					      (>= (getx document :emp-no) 50)
					      (<= (getx document :emp-no) 100))
					     (and
					      (>= (getx document :emp-no) (/ size 2))
					      (<= (getx document :emp-no) (+ (/ size 2) 100)))
					     (and
					      (>= (getx document :emp-no) (- size 50))
					      (<= (getx document :emp-no) size))))))))
	   results))

    (print "Fetching an index set.")
    (time
     (push (list
	    :how-many-davises?
	    (length (query-data employee-collection
				:index-values (list (list :surname "Davis")))))
	   results))

    (print "Doing a query against an index set.")
    (time
     (push (list
	    :how-many-davises-in-chile?
	    (length (query-data employee-collection
				:query (lambda (emp)
					 (string-equal (getx emp :country) "Chile"))
				:index-values (list (list :surname "Davis")))))
	   results)))

  (print results))

