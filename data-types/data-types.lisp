(in-package :cl-naive-data-types)

(defvar *example-type-defs*
  '(:example-type-defs
    (:type :object
     :complex-type :object
     :data-type "data-type"
     :accessor (:some-field-that-contains-an-object 
		:the-contained-object-field-name 
		:etc :etc ))
    
    (:type :object
     :complex-type :collection
     :data-type "data-type"
     :collection "some-collection"  
     :accessor (:some-field-that-contains-an-object 
		:the-contained-object-field-name 
		:etc :etc ))
			     
    (:type :object
     :complex-type :hierarchical
     :data-type "data-type"
     :collection "some-collection"
     :accessor (:some-field-name :etc)
     :child-accessor  (:some-childholder-field-name :etc))
			     
    (:type :string 
     :complex-type :key-value
     :values ((:key "1" :val "Ones") (:key "2" :val "Twos"))
     :documentation ":type is any basic type")
			     
    (:type :string
     :complex-type :value-string-list
     :values ("rst" "arst" "Ast")
     :delimiter " "
     :documentation ":type is any basic type, delimiter is used to return a string delimeted with this and when set used to split string and coerce basic type"
     )
    
    (:type :string
     :complex-type :value-list
     :values ("rst" "arst" "Ast")
     :documentation ":type is any basic type"
     )
			     
    (:type :list 
     :complex-type :key-value-list ;;((:key "1" :val "Ones") (:key "2" :val "Twos"))
     :key-list (:arst :arst :ARst)
     :value-type :string	   ;;or any other basic type
     )
    
    (:type :list 
     :complex-type :p-list ;;((:key "1" :val "Ones" :key "2" :val (:can :go :down (:a :bit)))
   
     )
			     
    (:type :list
     :complex-type :collection-objects 
     :data-type "arst"
     :collection "arst"
     :accessor (:context-spec :etc)
     :documentation "Selected objects from a collection"
     )
			     
    (:type :list
     :complex-type :list-objects ;;object not in a collection
     :data-type "user-permission"
     :accessor (:context-spec :etc))
			     
    (:type :list
     :complex-type :contained-object ;;object contianed in itself
     :data-type "user-permission"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name)
     )
			     
    (:type :list
     ;;a loose object contianed in another object in a collection
     :complex-type :colletion-contained-object 
     :data-type "user-permission"
     :collection "the-other-collection-from-which-you-want-a-contained-object"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name))))


(defun db-type-get-set (field)
  (cond ((listp (getf field :db-type))
	 (cond ((equalp (getf (getf field :db-type) :complex-type)
			:value-list)
		(getf (getf field :db-type) :type))
	       ((equalp (getf (getf field :db-type) :complex-type)
			:key-value-list)
		(getf (getf field :db-type) :type))
	       (t
		(getf (getf field :db-type) :complex-type))))
	(t
	 (getf field :db-type))))

(defgeneric getfx (object field &key &allow-other-keys)
  (:documentation "Returns the value of an object, but takes the field definition and not just the name."))

(defmethod getfx (object field &key  &allow-other-keys)
  (let ((db-type (db-type-get-set field)))
    (getsfx db-type field object)))

(defmethod (setf getfx) (value object field
			 &key &allow-other-keys)
   (let ((db-type (db-type-get-set field)))
    (setf (getsfx db-type field object) value)))

(defun getsfx* (field object)
  (let* ((name (getf field :name)))
    (getx object name)))

(defgeneric getsfx (type field object &key &allow-other-keys))

(defmethod getsfx ((type (eql :symbol)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :keyword)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :string)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :link)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :text)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :image)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :file)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :number)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :integer)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :date)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :time)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :email)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :lambda)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :lisp-code)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :java-script)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :css)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :text-blob)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :boolean)) field object &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :key-value)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :value-string-list)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :value-list)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :key-value-list)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :collection)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :collection-objects)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :list-objects)) field object &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :hierarchical)) field object &key &allow-other-keys)
   (getsfx* field object))


(defmethod getsfx ((type (eql :object)) field object
		   &key &allow-other-keys)
  (getsfx* field object))

(defmethod getsfx ((type (eql :contained-object)) field object
		   &key &allow-other-keys)
   (getsfx* field object))

(defmethod getsfx ((type (eql :collection-contained-object)) field object &key &allow-other-keys)
   (getsfx* field object))


(defun field-type-val (field key)
  (Let ((type (getf field :db-type)))
    (when (listp type)
	(getf type key))))

(defun set-getsfx* (field object value)
  (let* ((name (getf field :name)))
    (setf (getx object name) value)))

(defun setsfx-read* (field object value type-test read-error)
  (let* ((name (getf field :name))
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
	     (setf (getx object name) final-val)
	     (error (frmt read-error final-val)))
	(setf (getx object name) final-val))))


(defgeneric validate-sfx (type field object value &key &allow-other-keys))

(defmethod validate-sfx (type field object value &key &allow-other-keys)
  (values t nil))

;;TODO: how to do collection checking, additional parameters/keys
;;when and how to pass
(defmethod (setf getsfx) :around (value type field object   
				  &key &allow-other-keys)
;;  (when (validate-sfx type field object value)    )
  (call-next-method))

(defmethod (setf getsfx) (value (type (eql :symbol)) field object   
			  &key &allow-other-keys)
  (setsfx-read* field object value #'symbolp  "~S is not a symbol!"))

(defmethod (setf getsfx) (value (type (eql :keyword)) field object
			  &key &allow-other-keys)
  (setsfx-read* field object value #'keywordp  "~S is not a keyword!"))

(defmethod (setf getsfx) (value (type (eql :string)) field object
			  &key &allow-other-keys)
  (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :link)) field object
			  &key &allow-other-keys)
  (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :text)) field object
			  &key &allow-other-keys)
  (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :image)) field object
			  &key &allow-other-keys)
  (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :file)) field object
			  &key &allow-other-keys)
  (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :email)) field object
			  &key &allow-other-keys)
   (setf (getx object (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :number)) field object
			 &key &allow-other-keys)
  (setsfx-read* field object value #'numberp "~R is not a number!"))

(defmethod (setf getsfx) (value (type (eql :integer)) field object
			 &key &allow-other-keys)
  (setsfx-read* field object value #'numberp "~R is not an integer!"))

(defmethod (setf getsfx) (value (type (eql :date)) field object
			 &key &allow-other-keys)
  (set-getsfx* field object value))

(defmethod (setf getsfx) (value (type (eql :time)) field object
			 &key &allow-other-keys)
  (set-getsfx* field object value))

(defmethod (setf getsfx) (value (type (eql :boolean)) field object   
			  &key &allow-other-keys)

  (let* ((split (split-sequence:split-sequence #\, value))
	(val (if (equalp (car split) "true")
		 t)))    
    (set-getsfx* field object val)))


(defmethod (setf getsfx) (value (type (eql :lambda)) field object   &key &allow-other-keys)
  (setsfx-read* field object value #'consp "~S is not a cons!"))

(defmethod (setf getsfx) (value (type (eql :lisp-code)) field object   &key &allow-other-keys)
  (let ((blob (getx object (getf field :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(setf blob (make-blob  :file-type :text
			       :file-ext "lisp"
			       :location ""
			       :raw value)))
    (set-getsfx* field object blob)))


(defmethod (setf getsfx) (value (type (eql :css)) field object   &key &allow-other-keys)
  (let ((blob (getx object (getf field :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(setf blob (make-blob  :file-type :text
			       :file-ext "css"
			       :location ""
			       :raw value)))
    (set-getsfx* field object blob)))

(defmethod (setf getsfx) (value (type (eql :java-script)) field object   &key &allow-other-keys)
  (let ((blob (getx object (getf field :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(setf blob (make-blob  :file-type :text
			       :file-ext "js"
			       :location ""
			       :raw value)))
    (set-getsfx* field object blob)))

(defmethod (setf getsfx) (value (type (eql :text-blob)) field object   &key &allow-other-keys)
  (let ((blob (getx object (getf field :name))))
    (if (blob-p blob)
	(setf (blob-raw blob) value)
	(setf blob (make-blob  :file-type :text
			       :file-ext "txt"
			       :location ""
			       :raw value)))
    (set-getsfx* field object blob)))

(defmethod (setf getsfx) (value (type (eql :collection)) field object
			  &key &allow-other-keys)

  (let ((name (getf field :name))
	(final-val))
    (if (not (empty-p value))
	(setf final-val value))
    (setf (getx object name) final-val)))

(defmethod (setf getsfx) (value (type (eql :object)) field object
			  &key &allow-other-keys)

  (let ((name (getf field :name))
	(final-val))
    (if (not (empty-p value))
	(setf final-val value))
    (setf (getx object name) final-val)))


(defmethod validate-sfx ((type (eql :collection-contained-object)) field object value
			 &key objects &allow-other-keys)
    (let* ((valid (find value objects)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))


(defmethod validate-sfx ((type (eql :collection)) field object value
			 &key objects &allow-other-keys)
    (let* ((valid (find value objects)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))

(defmethod validate-sfx ((type (eql :object)) field object value
			 &key objects &allow-other-keys)
    (let* ((valid (find value objects)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))


(defmethod (setf getsfx) (value (type (eql :hierarchical)) field object
			 &key &allow-other-keys) 
    (set-getsfx* field object value))

(defmethod (setf getsfx) (value (type (eql :key-value)) field object
			 &key &allow-other-keys) 
    (set-getsfx* field object value))

(defmethod (setf getsfx) (value (type (eql :value-string-list)) field object   
			 &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (delimiter (if (stringp (dig field :db-type :delimiter))
			(coerce (dig field :db-type :delimiter)
				'character)
			(coerce (eval (dig field :db-type :delimiter))
				'character)
			))
	 (type (dig field :db-type :type))
	 (split (split-sequence:split-sequence delimiter value))
	 (list))
    (dolist (x split)
   
      (unless (empty-p x)
	(if (equalp type :keyword)
	    (setf list (append list 
			       (list (intern (string-upcase
					      (remove #\: (trim-whitespace x))) 
					     :KEYWORD))))
	    (setf list (append list (list (trim-whitespace x)))))))   
    (setf (getx object name) list)))

(defmethod validate-sfx (value (type (eql :value-list)) field object
			 &key &allow-other-keys)
  (let* ((list (or (and (dig field :db-type :values-script)
			(eval (dig field :db-type :values-script)))
		   (dig field :db-type :values)))
	 (*read-eval* nil)
	 (valid ))

    (if (functionp list)
	(setf list (funcall (eval (dig field :db-type :values-script)) object)))

    (setf valid (find (if (not (or (equalp (dig field :db-type :type) :string)
				   (equalp (dig field :db-type :type) :link)
				   (equalp (dig field :db-type :type) :text)))
			  (if (and value (not (empty-p value)))
			      (read-from-string value))
			  value)
		      list :test #'equalp))
 
    (values valid (if (not valid)
		      (frmt "Value not one of ~S" list)))))


(defmethod (setf getsfx) (value (type (eql :value-list)) field object 
			 &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (or (and (dig field :db-type :values-script)
			(eval (dig field :db-type :values-script)))
		   (dig field :db-type :values)))
	 (*read-eval* nil)
	 (val ))

    (if (functionp list)
	(setf list (funcall (eval (dig field :db-type :values-script)) object)))

   ;; (break "~s~%~%~s~%~%~s" (read-from-string value) value list)
    (setf val (find (if (not (or (equalp (dig field :db-type :type) :string)
				 (equalp (dig field :db-type :type) :link)
				 (equalp (dig field :db-type :type) :text)))
			(if (and value (not (empty-p value)))
			    (read-from-string value))
			value)
		      list :test #'equalp))
 
    (setf (getx object name) val)))

(defmethod (setf getsfx) (value (type (eql :key-value-list)) field object 
			 &key &allow-other-keys)
  (setf (getsfx (dig field :db-type :type) field object) value))

;;TODO: Check for dulplicates?
(defmethod (setf getsfx) (value (type (eql :collection-objects)) field object   
			  &key &allow-other-keys)
  (set-getsfx* field object value))


(defmethod (setf getsfx) (value (type (eql :contained-object)) field object   
			 &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (setf final-val value)
    (setf (getx object name) final-val)))

(defmethod (setf getsfx) (value (type (eql :collection-contained-object))
			  field object   &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (setf final-val value)
    (setf (getx object name) final-val)))

;;TODO: Check for dulplicates?
(defmethod (setf getsfx) (value (type (eql :list-objects)) field object   
			  &key &allow-other-keys)
  (set-getsfx* field object value))
