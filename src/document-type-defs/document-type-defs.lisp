(in-package :cl-naive-document-type-defs)

#|
cl-naive-document-type-defs is a "declaritive" implementation (I am using the word loosly) of cl-naive-document-type (abstract data type???). 

What use are type-defs? Well they are there to descibe the document type. IE more information about elements of a document type and the document type it self. Such information could cover any thing from the basics to the esoteric, and that is one of the reasons why this is a "declaritive" implmentation. The information covered is not always relevant to the database but could be extra information needed by a GUI to display an update a document and its elments correctly.

To understand the "loose" protocol/interface of cl-naive-type-defs we will look at the parts of the type def.

:document-type

(:document-type
  (:name "for use by programs"
   :label "name for use by humans"
   :elements "a list of elements that make up the document type"
   :attributes "anything else you want to share about the data type"
   :documentation ""))

Looks familiar? Well it should look and smell like a naivie-document-type

:elements

((:name "for use by programs, by convention KEYWORD to encourage data portability"
   :label "name for use by humans"
   :key-p "is this a key element"
   :concrete-type "how is the data represented in a program, ie string, integer, document etc"
   :attributes "anything else you want to share about the element"
   :documentation "")		    

  ...)

:attributes

(:property-name-1 'value :property-name-2 'value)

:concrete-type

These can be simple or complex.

Simple concrete type would be :string or :number

Complex concrete type examples:

Type of document

(:type (:document
	  (:type "Employee"
	   :accessor (:emp-no))))

List of strings.

(:type (:list :string))

List of documents

(:type (:list
	 (:document
	  (:type "Child"
	   :accessor (:name)))))

A reference doc. (Document residing in a different place (ie collection))

(:type (:document
	 (:type "Laptop"
	  :collection "laptop-collection"
	  :accessor (:id))))

Then convention is to stop at 2 layers of element type, any deeper becomes unwieldy. That does not mean that you can only stop at two levels of document hierarchy you can have doc types within doc types and so on, but the complect document type only needs to know about it self and one layer down.

You can have self referencing types, see the first-born element in the full example.

To be able to fully define reference types we need to also give defs of collections.

(:collection
     (:name "laptops"
	    :label "Company Laptops"
	    :data-type "laptop"))

cl-naive-type defs offer us the opportunity to use an element-def as the accessor to the value of the element in the document. getx is used to achieve this, the specialization makes full use of the the type defs to implement much more complex/intelligent getting and setting of the values.  It does that by calling getxe that has specializations that understand complex types and what to do.

Simple validation at setf time is also offered by supplying a function to setf-validate in attributes of an element. Validation functions must return 2 values the first indicates pass or fail the second is a message which is usually the reason for failure.

(:name :age
  :label "Age"                      
  :concrete-type :number
  :attributes (:display t :editable t
	       ;;setf-validate-is called for (setf getxe)
	       :setf-validate
	       (lambda (age)
		 (if (<= age 21)
		     (values t nil)
		     (values nil "Child is to old"))))
  :documentation "How old the child is")

Because tyge-defs uses cl lists you are well on your way to a domain specific language, your imagination is your only limitation.

You can use type-defs to bootstrap an application, creating stores and collections on the fly. They also come in very handy when you implement a GUI because they are so rich in information.

cl-naive-store does not use type-defs. All it does is give you cl-document-types which gives you a place to load your defs and access them at run time. It will also persist the type defs for you.

cl-document-types requirements are minimal and its up to you to load your type-defs into the store so that means you can do with or structure your type defs any way you want. But if you want to share typedefs portably then you might have to consider sticking to what cl-document-type-defs offer, at least as a minimal baseline.

|#

'

(defvar *example-type-defs*
  '((:document-type
     (:name "laptop"
      :label "Laptop"
      :elements ((:name :id
			:label "Serial No"
			:key-p t
			:concrete-type :string
			:attributes (:display t :editable t)
			:documentation "Unique no that identifies the laptop.")
		 (:name :make
			:label "Manufaturer"                      
			:concrete-type :string
			:attributes (:display t :editable t)
			:documentation "Then manufaturer of the laptop.")
		 (:name :model
			:label "Model"                      
			:concrete-type :string
			:attributes (:display t :editable t)
			:documentation "Model of the laptop."))
      :attributes ()
      :documentation "List of laptops the company owns."))

    (:collection
     (:name "laptops"
      :label "Company Laptops"
      :data-type "laptop"))

    (:document-type
     (:name "child"
      :label "Child"
      :elements ((:name :name
			:label "Name"
			:key-p t
			:concrete-type :string
			:attributes (:display t :editable t)
			:documentation "Name of child")
		 (:name :sex
			:label "Gender"                      
			:concrete-type :key-word
			:value-list (:male :female)
			:attributes (:display t :editable t)
			:documentation "Gender of the child, can only be male or female.")
		 (:name :age
			:label "Age"                      
			:concrete-type :number
			:attributes (:display t :editable t
					      ;;setf-validate-is called for (setf getxe)
					      :setf-validate
					      (lambda (age)
						(if (<= age 21)
						    (values t nil)
						    (values nil "Child is to old"))))
			:documentation "How old the child is"
			))
      :attributes ()
      :documentation "List of laptops the company owns."))

    (:document-type
     (:name "employee"
      :label "Employee"
      :elements ((:name :emp-
			:label "Employee Number"
			:key-p t
			:concrete-type :number
			:attributes (:display t :editable t)
			:documentation "Unique identifier of employee.")
		 (:name :name
			:label "Name"
			:concrete-type :string
			:attributes (:display t :editable t)
			:documentation "Name of employee")
		 (:name :sex
			:label "Gender"                      
			:concrete-type :key-word
			:value-list (:male :female)
			:attributes (:display t :editable t)
			:documentation "Gender of the child, can only be male or female.")
		 (:name :dependents
			:label "Children"                      
			:concrete-type (:type (:list
					       (:document
						(:type "Child"
						       :accessor (:name)))))
			:attributes (:display t :editable t)
			:documentation "List of the employees children")
		 (:name :laptop
			:label "Laptop"                      
			:concrete-type (:type (:document
					       (:type "laptop"
						      :collection "laptop-collection"
						      :accessor (:id))))
			:attributes (:display t :editable t)
			:documentation "Laptop allocated to employee")
		 (:name :first-born
			:label "First Born Child"                      
			:concrete-type (:type (:document
					       (:type "child"
						      :collection "employees"
						      :accessor (:emp-no :dependents :name))))
			:attributes (:display t :editable t)
			:documentation "List of the employees children"))
      :attributes ()
      :documentation "List of laptops the company owns."))

    (:collection
     (:name "employees"
      :label "Company Employees"
      :data-type "employee"))))



(defun db-type-get-set (element)
  (cond ((listp (getx element :type-def))
	 (cond ((equalp (getx (getx element :type-def) :complex-type)
			:value-list)
		(getx (getx element :type-def) :type))
	       ((equalp (getx (getx element :type-def) :complex-type)
			:key-value-list)
		(getx (getx element :type-def) :type))
	       (t
		(getx (getx element :type-def) :complex-type))))
	(t
	 (getx element :type-def))))


(defmethod getx (document (element cl-naive-document-types:element) &key &allow-other-keys)
  (let ((db-type (db-type-get-set element)))
    (getxe document element db-type)))

(defmethod (setf getx) (value document (element cl-naive-document-types:element)
			 &key &allow-other-keys)
   (let ((db-type (db-type-get-set element)))
    (setf (getxe document element db-type) value)))

(defun getxe* (document element)
  (let* ((name (getx element :name)))
    (getx document name)))

(defgeneric getxe (document element type &key &allow-other-keys))

(defmethod getxe (document element type &key &allow-other-keys)
  (getxe* document element))

(defun element-type-val (element key)
  (Let ((type (getx element :type-def)))
    (when (listp type)
	(getx type key))))

(defun set-getxe* (value document element )
  (let* ((name (getx element :name)))
    (setf (getx document name) value)))

(defun setxe-read* (value document element type-test read-error)
  (let* ((name (getx element :name))
	 (*read-eval* nil)
	 (final-val))
    
    (if value
	(if (stringp value)
	    (if (not (empty-p value))
		(setf final-val (read-from-string value)))
	    (setf final-val value))
	(setf final-val value))
 
    (if final-val
	(if  (apply type-test (list final-val))
	     (setf (getx document name) final-val)
	     (error (frmt read-error final-val)))
	(setf (getx document name) final-val))))

(defgeneric validate-xe (document element type value &key &allow-other-keys))

(defmethod validate-xe (document element type value &key &allow-other-keys)  
  (if (digx element :attributes :setf-validate)
      (funcall (digx element :attributes :setf-validate) value)
      (values t nil)))

;;TODO: how to do collection checking, additional parameters/keys
;;when and how to pass
(defmethod (setf getxe) :around (value document element type     
				 &key &allow-other-keys)
  ;;TODO: Figure out what to do in validate-xe
  ;;it is no longer clear if it is needed or what it
  ;;it is trying to do
  (call-next-method)
  ;;  (when (validate-xe document element type value))
  )

(defmethod (setf getxe) (value document element type 
			  &key &allow-other-keys)
  (setf (getx document (getx element :name)) (frmt "~A" value)))


(defmethod (setf getxe) (value document element (type (eql :symbol))     
			  &key &allow-other-keys)
  (setxe-read* value document element #'symbolp  "~S is not a symbol!"))

(defmethod (setf getxe) (value document element (type (eql :keyword)) &key &allow-other-keys)
  (setxe-read* value document element #'keywordp  "~S is not a keyword!"))

(defmethod (setf getxe) (value document element (type (eql :number))
			 &key &allow-other-keys)
  (setxe-read* value document element #'numberp "~R is not a number!"))

(defmethod (setf getxe) (value document element (type (eql :integer))
			 &key &allow-other-keys)
  (setxe-read* value document element #'numberp "~R is not an integer!"))

(defmethod (setf getxe) (value document element (type (eql :date-time))
			  &key &allow-other-keys)
  (let* ((name (getx element :name)))
    (if (stringp value)
	(if (> (length value) 18)
	    (setf (getx document name) (local-time:parse-timestring value))
	    (if (= (length value) 16)
		  (setf (getx document name)
			  (local-time:parse-timestring
			   (format nil "~A~A" value
				   (subseq (format nil "~A" (local-time:now) ) 16))))
		  (setf (getx document name) value)))
	(setf (getx document name) value))))

(defmethod (setf getxe) (value document element (type (eql :date))
			 &key &allow-other-keys)
  (set-getxe* value document element))

(defmethod (setf getxe) (value document element (type (eql :time))
			 &key &allow-other-keys)
  (set-getxe* value document element))

(defmethod (setf getxe) (value document element (type (eql :boolean))   
			  &key &allow-other-keys)

  (let* ((split (split-sequence:split-sequence #\, value))
	(val (if (equalp (car split) "true")
		 t)))    
    (set-getxe* element document val)))


(defmethod (setf getxe) (value document element (type (eql :lambda)) &key &allow-other-keys)
  (setxe-read* value document element #'consp "~S is not a cons!"))


(defmethod (setf getxe) (value document element (type (eql :collection))
			  &key &allow-other-keys)

  (let ((name (getx element :name))
	(final-val))
    (if (not (empty-p value))
	(setf final-val value))
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value document element (type (eql :document))
			  &key &allow-other-keys)

  (let ((name (getx element :name))
	(final-val))
    (if (not (empty-p value))
	(setf final-val value))
    (setf (getx document name) final-val)))


(defmethod validate-xe (document element (type (eql :collection-contained-document)) value
			 &key &allow-other-keys)
    (let* ((valid (find value document)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (digx element :type-def :collection))))))


(defmethod validate-xe (document element (type (eql :collection)) value
			&key &allow-other-keys)

  
  (let* ((valid (find value document)))
    
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (digx element :type-def :collection))))))

(defmethod validate-xe (document element (type (eql :document)) value
			&key &allow-other-keys)
  
    (let* ((valid (find value document)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (digx element :type-def :collection))))))

(defmethod (setf getxe) (value document element (type (eql :value-string-list))   
			 &key &allow-other-keys)
  (let* ((name (getx element :name))
	 (delimiter (if (stringp (digx element :type-def :delimiter))
			(coerce (digx element :type-def :delimiter)
				'character)
			(coerce (eval (digx element :type-def :delimiter))
				'character)
			))
	 (type (digx element :type-def :type))
	 (split (split-sequence:split-sequence delimiter value))
	 (list))
    (dolist (x split)
   
      (unless (empty-p x)
	(if (equalp type :keyword)
	    (setf list (append list 
			       (list (intern (string-upcase
					      (remove #\: (naive-impl:trim-whitespace x))) 
					     :KEYWORD))))
	    (setf list (append list (list (naive-impl:trim-whitespace x)))))))   
    (setf (getx document name) list)))

(defmethod validate-xe (value document element (type (eql :value-list))
			&key &allow-other-keys)
  
  (let* ((list (or (and (digx element :type-def :values-script)
			(eval (digx element :type-def :values-script)))
		   (digx element :type-def :values)))
	 (*read-eval* nil)
	 (valid ))

    (if (functionp list)
	(setf list (funcall (eval (digx element :type-def :values-script)) document)))

    (setf valid (find (if (not (or (equalp (digx element :type-def :type) :string)
				   (equalp (digx element :type-def :type) :link)
				   (equalp (digx element :type-def :type) :text)))
			  (if (and value (not (empty-p value)))
			      (read-from-string value))
			  value)
		      list :test #'equalp))
 
    (values valid (if (not valid)
		      (frmt "Value not one of ~S" list)))))


(defmethod (setf getxe) (value document element (type (eql :value-list)) 
			 &key &allow-other-keys)
  (let* ((name (getx element :name))
	 (list (or (and (digx element :type-def :values-script)
			(eval (digx element :type-def :values-script)))
		   (digx element :type-def :values)))
	 (*read-eval* nil)
	 (val ))

    (if (functionp list)
	(setf list (funcall (eval (digx element :type-def :values-script)) document)))
   
    (setf val (find (if (not (or (equalp (digx element :type-def :type) :string)
				 (equalp (digx element :type-def :type) :link)
				 (equalp (digx element :type-def :type) :text)))
			(if (and value (not (empty-p value)))
			    (read-from-string value))
			value)
		      list :test #'equalp))
 
    (setf (getx document name) val)))

(defmethod (setf getxe) (value document element (type (eql :key-value-list)) 
			 &key &allow-other-keys)
  (setf (getxe (digx element :type-def :type) element document) value))

;;TODO: Check for dulplicates?
(defmethod (setf getxe) (value document element (type (eql :collection-documents))   
			  &key &allow-other-keys)
  (set-getxe* value document element))


(defmethod (setf getxe) (value document element (type (eql :contained-document))   
			 &key &allow-other-keys)
  (let ((name (getx element :name))
	(final-val))
    
    (setf final-val value)
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value document element (type (eql :collection-contained-document))
			  &key &allow-other-keys)
  (let ((name (getx element :name))
	(final-val))
    
    (setf final-val value)
    (setf (getx document name) final-val)))


