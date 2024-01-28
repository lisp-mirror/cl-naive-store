(in-package :cl-naive-store.tests)

(naive-impl:initialize)

(defun get-temp ()
  (handler-case
      (cl-fad::get-default-temporary-directory)
    (error (c)
      (declare (ignore c))
      (make-pathname :directory '(:absolute "tmp")))))

(defun test-location ()
  (cl-fad:merge-pathnames-as-directory
   (get-temp) ;;(user-homedir-pathname)
   (make-pathname :directory (list :relative "test-multiverse"))))

(defparameter *universe* nil)

(defparameter *multiverse* nil)

(defvar *keep-test-multiverse* nil
  "Set to true to keep (renaming them away) the multiverse data directory in tear-down-suite.
If set to a pathname designator, moves them under the directory of this pathname.
")

(defmethod cl-naive-tests:tear-down-suite :around (test-name)
  "An :around method deletes the whole multiverse and sets multiverse and
universe to nil."
  (let ((multiverse-directory (if *multiverse*
                                  (location *multiverse*)
                                  (test-location))))
    (if (or (null test-name) (not *keep-test-multiverse*))
        (cl-fad:delete-directory-and-files multiverse-directory :if-does-not-exist :ignore)
        (let* ((destination (if (or (stringp *keep-test-multiverse*)
                                    (pathnamep *keep-test-multiverse*))
                                (pathname *keep-test-multiverse*)
                                nil))
               (new-directory
                 (make-pathname
                  :directory (append
                              (if destination
                                  (pathname-directory destination)
                                  (butlast (pathname-directory multiverse-directory)))
                              (list (concatenate
                                     'string
                                     (first (last (pathname-directory multiverse-directory)))
                                     "-preserved-for-"
                                     (string test-name))))
                  :defaults multiverse-directory)))
          ;; We cannot just preserve the data directory, since following tests expect to work on a clean slate.
          ;; therefore we will rename it.
          (handler-case (if destination
                            (progn
                              (cl-fad:delete-directory-and-files new-directory :if-does-not-exist :ignore)
                              (ql-bundle::copy-directory-tree multiverse-directory new-directory)
                              (cl-fad:delete-directory-and-files multiverse-directory :if-does-not-exist :ignore))
                            (ql-impl-util:rename-directory  multiverse-directory new-directory))
            (:no-error (&rest ignored)
              (declare (ignore ignored))
              (format *terminal-io* "~&Preserved test multiverse: ~S as ~S~%" multiverse-directory new-directory)
              (finish-output *terminal-io*))
            (error () nil)))))
  (setf *universe* nil)
  (setf *multiverse* nil)
  (call-next-method))

(defgeneric get-universe-class (test-name))

(defmethod get-universe-class (test-name)
  'universe)

(defgeneric get-store-class (test-name))

(defmethod get-store-class (test-name)
  'store)

(defmethod cl-naive-tests:setup-suite :around (test-name)
  "An :around method tears down any residual infrastructure and creates multiverse and
universe."

  ;;Do default tear down of multiverse just incase
  (tear-down-suite nil)

  (setf *multiverse* (make-instance
                      'multiverse
                      :name "multiverse"
                      :location (test-location) ;Setting the location on disk.
                      :universe-class (get-universe-class test-name)))

  (setf *universe* (make-instance 'universe
                                  :name "universe"
                                  :store-class (get-store-class test-name)))

  (add-multiverse-element *multiverse* *universe*)

  (call-next-method))

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
