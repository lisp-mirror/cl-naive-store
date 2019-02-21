(in-package :cl-naive-store)

(defvar *example-type-defs*
  '(:example-type-defs
    (:type :item
     :complex-type :item
     :data-type "data-type"
     :accessor (:some-field-that-contains-an-item 
		:the-contained-item-field-name 
		:etc :etc ))
    
    (:type :item
     :complex-type :collection
     :data-type "data-type"
     :collection "some-collection"  
     :accessor (:some-field-that-contains-an-item 
		:the-contained-item-field-name 
		:etc :etc ))
			     
    (:type :item
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
     :complex-type :collection-items 
     :data-type "arst"
     :collection "arst"
     :accessor (:context-spec :etc)
     :documentation "Selected items from a collection"
     )
			     
    (:type :list
     :complex-type :list-items ;;item not in a collection
     :data-type "user-permission"
     :accessor (:context-spec :etc))
			     
    (:type :list
     :complex-type :contained-item ;;item contianed in itself
     :data-type "user-permission"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name)
     )
			     
    (:type :list
     ;;a loose item contianed in another item in a collection
     :complex-type :colletion-contained-item 
     :data-type "user-permission"
     :collection "the-other-collection-from-which-you-want-a-contained-item"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name))))

(declaim (inline frmt))
(defun frmt (control-string &rest args)
  (apply #'format nil control-string args))

;;STRING MANIPULATION
(defun trim-whitespace (string)
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))


(defun empty-p (value)
  "Checks if value is null or an empty string."
  (if (equalp (type-of value) 'item)
      nil
      (or (null value)
	  (equal value "")
	  (equal (trim-whitespace (princ-to-string value)) ""))))


(defgeneric getfx (item field &key &allow-other-keys))

(defmethod getfx ((item item) field &key  &allow-other-keys)
  (let ((db-type (if (listp (getf field :db-type))
		     (or (getf (getf field :db-type) :complex-type)
			 (getf (getf field :db-type) :type))
		     (getf field :db-type))))
    (getsfx db-type field item)))

(defmethod (setf getfx) (value (item item) field
			 &key &allow-other-keys)
  (let ((db-type (if (listp (getf field :db-type))
		     (or (getf (getf field :db-type) :complex-type)
			 (getf (getf field :db-type) :type))
		     (getf field :db-type))))
    (setf (getsfx db-type field item) value)))

(defun getsfx* (field item)
  (let* ((name (getf field :name)))
    (getx item name)))

(defgeneric getsfx (type field item &key &allow-other-keys))

(defmethod getsfx ((type (eql :symbol)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :keyword)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :string)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :link)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :text)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :image)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :file)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :number)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :integer)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :date)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :time)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :email)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :script)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :boolean)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :key-value)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :value-string-list)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :value-list)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :key-value-list)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :collection)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :collection-items)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :list-items)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :hierarchical)) field item &key &allow-other-keys)
   (getsfx* field item))


(defmethod getsfx ((type (eql :item)) field item
		   &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :contained-item)) field item
		   &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :collection-contained-item)) field item &key &allow-other-keys)
   (getsfx* field item))


(defun field-type-val (field key)
  (Let ((type (getf field :db-type)))
    (when (listp type)
	(getf type key))))

(defun set-getsfx* (field item value)
  (let* ((name (getf field :name)))
    (setf (getx item name) value)))

(defun setsfx-read* (field item value type-test read-error)
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
	     (setf (getx item name) final-val)
	     (error (frmt read-error final-val)))
	(setf (getx item name) final-val))))


(defgeneric validate-sfx (type field item value &key &allow-other-keys))

(defmethod validate-sfx (type field item value &key &allow-other-keys)
  (values t nil))

;;TODO: how to do collection checking, additional parameters/keys
;;when and how to pass
(defmethod (setf getsfx) :around (value type field item   
				  &key &allow-other-keys)
;;  (when (validate-sfx type field item value)    )
  (call-next-method))

(defmethod (setf getsfx) (value (type (eql :symbol)) field item   
			  &key &allow-other-keys)
  (setsfx-read* field item value #'symbolp  "~S is not a symbol!"))

(defmethod (setf getsfx) (value (type (eql :keyword)) field item
			  &key &allow-other-keys)
  (setsfx-read* field item value #'keywordp  "~S is not a keyword!"))

(defmethod (setf getsfx) (value (type (eql :string)) field item
			  &key &allow-other-keys)
  (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :link)) field item
			  &key &allow-other-keys)
  (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :text)) field item
			  &key &allow-other-keys)
  (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :image)) field item
			  &key &allow-other-keys)
  (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :file)) field item
			  &key &allow-other-keys)
  (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :email)) field item
			  &key &allow-other-keys)
   (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :number)) field item
			 &key &allow-other-keys)
  (setsfx-read* field item value #'numberp "~R is not a number!"))

(defmethod (setf getsfx) (value (type (eql :integer)) field item
			 &key &allow-other-keys)
  (setsfx-read* field item value #'numberp "~R is not an integer!"))

(defmethod (setf getsfx) (value (type (eql :date)) field item
			 &key &allow-other-keys)
  (set-getsfx* field item value))

(defmethod (setf getsfx) (value (type (eql :time)) field item
			 &key &allow-other-keys)
  (set-getsfx* field item value))

(defmethod (setf getsfx) (value (type (eql :boolean)) field item   
			  &key &allow-other-keys)

  (let* ((split (split-sequence:split-sequence #\, value))
	(val (if (equalp (car split) "true")
		 t)))    
    (set-getsfx* field item val)))


(defmethod (setf getsfx) (value (type (eql :script)) field item   &key &allow-other-keys)
  (setsfx-read* field item value #'consp "~S is not a cons!"))

(defmethod (setf getsfx) (value (type (eql :collection)) field item
			  &key &allow-other-keys)

  (let ((name (getf field :name))
	(final-val))
    (if (not (empty-p value))
	(if (equalp (type-of value) 'item)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec)))))
    (setf (getx item name) final-val)))

(defmethod (setf getsfx) (value (type (eql :item)) field item
			  &key &allow-other-keys)

  (let ((name (getf field :name))
	(final-val))
    (if (not (empty-p value))
	(if (equalp (type-of value) 'item)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec)))))
    (setf (getx item name) final-val)))




(defmethod validate-sfx ((type (eql :collection-contained-item)) field item value
			 &key items &allow-other-keys)
    (let* ((valid (find value items)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))


(defmethod validate-sfx ((type (eql :collection)) field item value
			 &key items &allow-other-keys)
    (let* ((valid (find value items)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))

(defmethod validate-sfx ((type (eql :item)) field item value
			 &key items &allow-other-keys)
    (let* ((valid (find value items)))
     (values valid (if (not valid)
		      (frmt "Value ~A not found in ~A" value
			    (dig field :db-type :collection))))))


(defmethod (setf getsfx) (value (type (eql :hierarchical)) field item
			 &key &allow-other-keys) 
 
  ;;TODO: will id not be full ref hash now?
  #|
  (if (not (empty-p value))
      
    (let* ((id (parse-integer value))
	   (list (getx source
		       (dig field :db-type :child-accessor)))
	   (object)) 
      
      (dolist (contact list)
	(when (equalp id (item-hash contact))
	  (setf object contact)))
      (if (type-of-p field object)
	  (set-getsfx* field item object)
	  (error (frmt "~S is not of type ~A!" object (dig field :db-type :data-type)))))
  |#
    (set-getsfx* field item nil))

(defmethod (setf getsfx) (value (type (eql :key-value)) field item
			 &key &allow-other-keys) 
    (set-getsfx* field item nil))

(defmethod (setf getsfx) (value (type (eql :value-string-list)) field item   
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
    (setf (getx item name) list)))

(defmethod validate-sfx (value (type (eql :value-list)) field item
			 &key &allow-other-keys)
  (let* ((list (or (and (dig field :db-type :values-script)
			(eval (dig field :db-type :values-script)))
		   (dig field :db-type :values)))
	 (*read-eval* nil)
	 (valid ))

    (if (functionp list)
	(setf list (funcall (eval (dig field :db-type :values-script)) item)))

    (setf valid (find (if (not (or (equalp (dig field :db-type :type) :string)
				   (equalp (dig field :db-type :type) :link)
				   (equalp (dig field :db-type :type) :text)))
			  (if (and value (not (empty-p value)))
			      (read-from-string value))
			  value)
		      list :test #'equalp))
 
    (values valid (if (not valid)
		      (frmt "Value not one of ~S" list)))))


(defmethod (setf getsfx) (value (type (eql :value-list)) field item 
			 &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (or (and (dig field :db-type :values-script)
			(eval (dig field :db-type :values-script)))
		   (dig field :db-type :values)))
	 (*read-eval* nil)
	 (val ))

    (if (functionp list)
	(setf list (funcall (eval (dig field :db-type :values-script)) item)))

   ;; (break "~s~%~%~s~%~%~s" (read-from-string value) value list)
    (setf val (find (if (not (or (equalp (dig field :db-type :type) :string)
				 (equalp (dig field :db-type :type) :link)
				 (equalp (dig field :db-type :type) :text)))
			(if (and value (not (empty-p value)))
			    (read-from-string value))
			value)
		      list :test #'equalp))
 
    (setf (getx item name) val)))



(defmethod (setf getsfx) (value (type (eql :key-value-list)) field item 
			 &key &allow-other-keys)
  (setf (getsfx (dig field :db-type :type) field item) value))



(defmethod (setf getsfx) (value (type (eql :collection-items)) field item   
			 &key &allow-other-keys)
  (setsfx-read* field item value #'listp "~R is not a list!"))




(defmethod (setf getsfx) (value (type (eql :contained-item)) field item   
			 &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (if (equalp (type-of value) 'item)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec))))
    (setf (getx item name) final-val)))

(defmethod (setf getsfx) (value (type (eql :collection-contained-item))
			  field item   &key &allow-other-keys)
  (let ((name (getf field :name))
	(final-val))
    
    (if (equalp (type-of value) 'item)
		(setf final-val value)
		(error (frmt "~S is not of type ~A!" value
			   (dig field :db-type :data-spec))))
    (setf (getx item name) final-val)))
