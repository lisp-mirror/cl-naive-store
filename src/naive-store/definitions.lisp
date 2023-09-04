(in-package :cl-naive-store.naive-core)

;;; There are two main sources of definitions, hand crafted or persited definitions.

;;; Persisted definitions are found for individual elements along the
;;; directory structure of the peristed data and is relative to the
;;; multivers/universe/store/collection structure. These definitions
;;; can be considered as partial definitions because they do not
;;; contain any information about the children. This is because they
;;; are created during add-multiverse-element and at that stage the
;;; element might not have any children yet. It also means that there
;;; is not one central complete definition that can show what the full
;;; database structure looks like.

;;; A complete definition of all the multiverse elments is typically
;;; created by hand as a plist tree. You could of course create such a
;;; complete definition by walking the directory tree. A complete
;;; definition could be used for bootstrapping the database if
;;; needed. Bootstrapping the whole database is of course counter to
;;; the lazy loading principal that is supported by naive-store.

;;;The funcitons and methods in this file relates to definition
;;;manipulation and the creation of multiverse elements from
;;;definitions.

;;; The definition manipulation functions are simple in the sense that
;;; they will not ensure the integrity of the final definition. For
;;; example if you remove a document-type that document-type might
;;; still be used by a collection which will cause errors when loading
;;; from such a broken definition.

;;TODO: Do some work on ensuring entegrity when dealing with document-type also split out document-type stuff to document-types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code that does definition manipulation.

(defun query-chain (definition criteria)
  (let ((results))
    (labels ((query* (definition criteria)
               (let ((result (cl-naive-ptrees:query definition (list (car criteria)))))
                 (when result

                   (if (cdr criteria)
                       (dolist (huh result)
                         (query* huh (cdr criteria)))
                       (push result results))))))

      (query* definition criteria)

      (nreverse (car results)))))

(defgeneric query-definition (definition &key fn element)
  (:documentation "Queries the definition passed for an element or elements.

If an element is supplied limits calling the function to those elements or if no function is supplied just fetches elements of keyword element.

If you need more control use cl-naive-ptrees directly."))

(defmethod query-definition (definition &key fn element)
  (if fn
      (if element
          (cl-naive-ptrees::query definition
                                  `(((lambda (node)
                                       (when (listp node)
                                         (funcall fn node)))
                                     ,element)))
          (cl-naive-ptrees::map-plists definition
                                       fn))
      (if element
          (cl-naive-ptrees::query definition
                                  `((,element)))
          definition)))

(defgeneric get-definition-element (element-type parent name)
  (:documentation "Fetches a definition of the type element-type by name from the parent definition."))

;;TODO: depricated
(defgeneric find-named-elements (element name definition &key fn)
  (:documentation
   "Returns all the specific named definition-elements found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-elements (element name definition &key fn)
  (cl-naive-ptrees::query definition
                          (if fn
                              `(((lambda (node)
                                   (when (listp node)
                                     (and
                                      (equalp (getf node :name) ,name)
                                      (funcall fn node))))
                                 ,element))
                              `(((lambda (node)
                                   (when (listp node)
                                     (equalp (getf node :name) ,name)))
                                 ,element)))))

(defgeneric find-named-element (element name definition)
  (:documentation
   "Returns the first specific named definition-element found.

What the definition needs to be can vary for elements but worst case
implementations should at least deal with a multiverse definition
and parent definition."))

(defmethod find-named-element (element name definition)
  (car (find-named-elements element name definition)))

(defgeneric add-definition-element (element-type definition element &key name-path)
  (:documentation "Adds a definition element to the parent definition."))

(defun build-name-path-chain (name-path)
  `,(mapcar (lambda (path-element)
              `((lambda (node)
                  (when (listp node)
                    (when  (equalp (getf node :name) ,(second path-element))
                      node)))
                ,(first path-element)))
            name-path))

(defmacro add-defintion-element* (element-type list-key definition element name-path
                                  replace-p)
  (let ((element-type% (gensym))
        (list-key% (gensym))
        (definition% (gensym))
        (element% (gensym))
        (name-path% (gensym))
        (replace-p% (gensym)))

    `(let ((,element-type% ,element-type)
           (,list-key% ,list-key)
           (,definition% ,definition)
           (,element% ,element)
           (,name-path% ,name-path)
           (,replace-p% ,replace-p))

       (let* ((elements (or (and ,name-path%
                                 (getx
                                  (second
                                   (car
                                    (query-chain
                                     ,definition%
                                     (cl-naive-store.naive-core::build-name-path-chain
                                      ,name-path%))))
                                  ,list-key%))
                            (getx ,definition% ,list-key%)))
              (existing-element (find-named-element ,element-type%
                                                    (digx ,element% ,element-type% :name)
                                                    elements)))

         (unless elements
           (error (format nil "~A not found in definition."
                          (string-capitalize ,list-key%))))

         (if existing-element
             (if (not ,replace-p%)
                 (error "Collection definition already exsists: ~A"
                        (digx ,element% :collection :name))
                 (setf elements (nsubstitute ,element% existing-element elements)))
             (nconc elements (list ,element)))
         ,definition%))))

(defmethod add-definition-element ((element-type (eql :collection))
                                   definition
                                   collection
                                   &key name-path replace-p)
  "Adds a collection to :collections."

  (add-defintion-element* :collection
                          :collections
                          definition
                          collection
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :document-type))
                                   definition
                                   document-type
                                   &key name-path replace-p)
  "Adds a document-type to :document-types."

  (add-defintion-element* :document-type
                          :document-types
                          definition
                          document-type
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :store))
                                   definition
                                   store
                                   &key name-path replace-p)
  "Adds a store to :stores."

  (add-defintion-element* :store
                          :stores
                          definition
                          store
                          name-path
                          replace-p))

(defmethod add-definition-element ((element-type (eql :universe))
                                   definition
                                   universe
                                   &key name-path replace-p)
  "Adds a universe to :universes."

  (add-defintion-element* :universe
                          :universes
                          definition
                          universe
                          name-path
                          replace-p))

(defgeneric remove-definition-element (element-type  definition element-name
                                       &key name-path)
  (:documentation "Removes a definition element from the definition."))

(defmacro remove-defintion-element* (element-type list-key definition name name-path)
  (let ((definition% (gensym))
        (name% (gensym))
        (name-path% (gensym))
        (element-type% (gensym))
        (list-key% (gensym)))

    `(let* ((,definition% ,definition)
            (,name% ,name)
            (,name-path% ,name-path)
            (,element-type% ,element-type)
            (,list-key% ,list-key))

       (let* ((path-element (second (car
                                     (query-chain
                                      ,definition%
                                      (build-name-path-chain ,name-path%)))))
              (elements (or (and ,name-path%
                                 path-element
                                 (getx
                                  path-element
                                  ,list-key%))
                            (getx path-element ,list-key%)))
              (existing-element (find-named-element ,element-type%
                                                    ,name%
                                                    elements)))

         (unless elements
           (error (format nil "~A not found in definition."
                          (string-capitalize ,list-key%))))

         (when existing-element
           (setf (getx (or path-element ,definition%) ,list-key%)
                 (remove existing-element elements)))

         ,definition%))))

(defmethod remove-definition-element ((element-type (eql :universe))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :universe
   :universes
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :store))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :store
   :stores
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :collection))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :collection
   :collections
   definition
   name
   name-path))

(defmethod remove-definition-element ((element-type (eql :document-type))
                                      definition
                                      name
                                      &key name-path)
  "Removes a universe from :universes."
  (declare (ignore element-type))

  (remove-defintion-element*
   :document-type
   :document-types
   definition
   name
   name-path))

(defun get-definitions (location definition-type)
  "Returns persisted definitions for the type (multiverse, universe, store, collection) using the passed object that can be a multiverse, universe, store, collection.
If no definition-type is not supplied the definition of the object is returned."
  (mapcar (lambda (filename)
            (naive-impl:sexp-from-file filename))
          (directory (format nil "~A/**/*.~A"
                             location definition-type))))

(defun get-definition (location definition-type name &key (error-p t))
  "Returns a persisted definition for the type (multiverse, universe, store, collection) using the passed object that can be a multiverse, universe, store, collection."
  (let* ((filename (format nil "~A/**/~A.~A"
                           location name
                           (if (keywordp definition-type)
                               (cond ((equal definition-type :collection)
                                      "col")
                                     ((equal definition-type :document-type)
                                      "type")
                                     (t
                                      (format nil "~(~a~)" definition-type)))
                               definition-type)))
         (file (car (directory filename))))
    (when error-p
      (unless file
        (error "Defintion file not found for ~A." filename)))
    (naive-impl:sexp-from-file filename)))

;;TODO: Should we split this out into its own file?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code that deals with the creation of instances of the multiverse from definitions.

(defgeneric find-definition-files ())

(defgeneric instance-from-definition (definition-type definition)
  (:documentation "Instanciates an element from the definition for the likes of multiverse, universe, store, collection or document-type.

This method has no knowledge of or ignores the existance of parents and children elements."))

;;TODO: Should we check if the defintion is already loaded before we try to load it???
(defgeneric load-from-definition (parent definition-type definition
                                  &key class persist-p with-children-p
                                  with-data-p)
  (:documentation "Instanciates an element from the definition for the likes of multiverse, universe, store, collection or document-type and loads it into the multiverse using the parent.

If persist-p the definition is persisted along with any child definitions  that where instanciated.

Multiverse elements usually have a reference to the parent that needs to be set. For instance a collection will have a reference to its store.

Multiverse elements usually also have child elements that could be instanciated for the relevent element. The choice is left up to the user. If the user does want children to also be instanciated they can supply a complete definition or rely on naive-store persisted definition files to be found and used. Set with-children-p for the required behaviour.

Whether documents (the actual data) is loaded after instanciation is a choice of the user. Use with-data-p to affect the behaviour. Just note that if you load data this way you are forgoing lazy loading."))

#|
TODO: Try again with mop later when we have time

(defun instance-from-definition* (definition class)
(let ((definition-body (if (and
(member (first definition)
'(:multiverse :universe :store :collection
:document-type :element :attribute))
(listp (second definition)))
(second definition)
definition)))

`(make-instance
,(or
(getx definition-body :class)
class)
,@(nreverse (loop :for (key value) :on definition-body :by (function cddr)
:when (not (member key '(:universes :stores :collections
:document-types
:elements :attributes)))
:collect value
:collect key)))))
|#

(defun definition-body (definition)
  (definition-body (if (and
                        (member (first definition)
                                '(:multiverse :universe :store :collection
                                  :document-type :element :attribute))
                        (listp (second definition)))
                       (second definition)
                       definition)))

(defmethod instance-from-definition ((class (eql 'multiverse)) definition)

  (let ((definition-body (definition-body definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :location (getx definition-body :location)
                   :universe-class (getx definition-body :universe-class))))

(defmethod load-from-definition (parent (definition-type (eql :multiverse))
                                 definition &key class persist-p
                                 with-children-p
                                 with-data-p)
  ;;Parent is irrelevant for multiverse it cannot have a parent.
  (declare (ignore parent))
  (let* ((definition-body (definition-body (definition-body definition)))
         (instance (instance-from-definition (or
                                              (getx definition-body :class)
                                              class
                                              'multiverse)
                                             definition)))
    (when persist-p
      (unless (location instance)
        (error "Cannot persist the multiverse, there is no location."))
      (persist instance))

    (when (or with-data-p with-children-p)

      (dolist (child-definition (or (getx definition-body :universes)
                                    (get-definitions (location instance)
                                                     :universe)))
        (load-from-definition
         instance
         :universe child-definition
         :class (getx definition-body :universe-class)
         :persist-p persist-p
         :with-children-p with-children-p
         :with-data-p with-data-p)))

    instance))

(defmethod instance-from-definition ((class (eql 'universe)) definition)
  (let ((definition-body (definition-body definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :location (getx definition-body :location)
                   :store-class (getx definition-body :store-class))))

(defmethod load-from-definition ((multiverse multiverse)
                                 (definition-type (eql :universe))
                                 definition &key class persist-p
                                 with-children-p
                                 with-data-p)

  (let* ((definition-body (definition-body (definition-body definition)))
         (instance (instance-from-definition (or
                                              (getx definition-body :class)
                                              class
                                              (getx multiverse :universe-class)
                                              'universe)
                                             definition)))
    (when persist-p
      (unless (location instance)
        (error "Cannot persist the universe, there is no location."))
      (persist instance))

    (when (or with-data-p with-children-p)
      (dolist (child-definition (or (getx definition-body :stores)
                                    (get-definitions (location instance)
                                                     :store)))
        (load-from-definition
         instance
         :store
         child-definition
         :class (getx definition-body :store-class)
         :persist-p persist-p
         :with-children-p with-children-p
         :with-data-p with-data-p)))

    (when instance
      (setf (multiverse instance) multiverse)
      (add-multiverse-element multiverse instance :persist-p persist-p))

    instance))

(defmethod instance-from-definition ((class (eql 'store)) definition)
  (let ((definition-body (definition-body definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :location (getx definition-body :location)
                   :collection-class (getx definition-body :collecition-class))))

(defmethod load-from-definition ((universe universe) (definition-type (eql :store))
                                 definition &key class persist-p
                                 with-children-p
                                 with-data-p)

  (let* ((definition-body (definition-body definition))
         (instance (instance-from-definition (or
                                              (getx definition-body :class)
                                              class
                                              (getx universe :store-class)
                                              'store)
                                             definition)))
    (when persist-p
      (unless (location instance)
        (error "Cannot persist the universe, there is no location."))
      (persist instance))

    (when (or with-data-p with-children-p)
      (dolist (child-definition (or (getx definition-body :collections)
                                    (get-definitions (location instance)
                                                     :collection)))
        (load-from-definition
         instance
         :store
         child-definition
         :class (getx definition-body :collection-class)
         :persist-p persist-p
         :with-children-p with-children-p
         :with-data-p with-data-p)))

    (when instance
      (setf (universe instance) universe)
      (add-multiverse-element universe instance :persist-p persist-p))

    instance))

(defmethod instance-from-definition ((class (eql 'collection)) definition)
  (let ((definition-body (definition-body definition)))
    (make-instance class
                   :name (getx definition-body :name)
                   :location (getx definition-body :location))))

(defmethod load-from-definition ((store store) (definition-type (eql :collection))
                                 definition &key class persist-p
                                 with-children-p
                                 with-data-p)

  (declare (ignore with-children-p))

  (let* ((definition-body (definition-body definition))
         (instance (instance-from-definition (or
                                              (getx definition-body :class)
                                              class
                                              (getx store :collection-class)
                                              'collection)
                                             definition)))
    (when persist-p
      (unless (location instance)
        (error "Cannot persist the universe, there is no location."))
      (persist instance))

    (when with-data-p
      (load-data instance))

    (when instance
      (setf (store instance) store))

    instance))

;;There is no persist-p option for this function because their is no
;;point in saving definitions that where just read.
(defun instances-from-definitions (location definition-type)
  (mapcar (lambda (definition)
            (instance-from-definition (getx (definition-body definition) :class)
                                      definition))
          (get-definitions location
                           definition-type)))

(defun load-from-definitions (parent definition-type &key class
                              with-children-p
                              with-data-p)
  (mapcar (lambda (definition)
            (load-from-definition parent definition-type definition
                                  :class class
                                  :with-children-p with-children-p
                                  :with-data-p with-data-p))
          (get-definitions (location parent)
                           definition-type)))

(defun instance-from-definition-file (location definition-type name &key class)
  (let ((definition (get-definition
                     location
                     definition-type
                     name)))
    (when definition
      (instance-from-definition (or (getx (definition-body definition) :class)
                                    class)
                                definition))))

(defgeneric load-from-definition-file (parent definition-type name
                                       &key class
                                       with-children-p
                                       with-data-p))

(defmethod load-from-definition-file (parent
                                      definition-type
                                      name &key class
                                      with-children-p
                                      with-data-p)
  (unless (location parent)
    (error "No location to get definition-file from."))

  (let ((definition (get-definition
                     (location parent)
                     definition-type
                     name)))
    (when definition
      (load-from-definition parent definition-type definition
                            :class class
                            :persist-p nil
                            :with-children-p with-children-p
                            :with-data-p with-data-p))))

