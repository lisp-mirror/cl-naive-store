(in-package :cl-naive-store.naive-core)

(defclass shard ()
  ((mac :initarg :mac
        :accessor mac
        :initform nil
        :documentation "Mac to identify shard.")
   (location :initarg :location
             :accessor location
             :initform nil
             :documentation "The file path to this shard is stored.")
   (documents :initarg :documents
              :accessor documents
              :initform (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)
              :documentation "Documents belonging to shard.")
   (status :initarg :status ;; accessor is defined below.
           :initform nil
           :documentation "Used internally during the loading of the documents in a shard to help with locking.")
   (lock :initarg :lock
         :accessor lock
         :initform (list :docs (bt:make-lock)
                         :hash-index (bt:make-lock)
                         :values-index (bt:make-lock))
         :documentation "Used internally to do shard specific locking."))

  (:documentation "Sharding is when you break the physical file that backs the collection into smaller files based on data elements of a document. An instance of a shard class is used to load the documents belonging to the shard into memory."))

(defclass collection ()
  ((store :initarg :store
          :accessor store
          :initform nil
          :documentation "The store that this collection belongs to.")
   (name :initarg :name
         :accessor name
         :documentation "The collection name.")
   (location :initarg :location
             :accessor location
             :initform nil
             :documentation "The directory path to where files for this collection are stored.")
   (shards :initarg :shards
           :accessor shards
           :initform (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)
           :type cl:vector
           :documentation "A vector of shards.

NOTES:

 Originally naive-store used lists but with the re-introduction of sharding, we chose to also introduce the use of lparrallel to speed up many functions and lparrallel has a preference for arrays.")

   (keys :initarg :keys
         :accessor keys
         :initform (list :key)
         :documentation "Keys need to be set to handle duplicates, the default is :key if :key is not found in the document then duplicates will occur.

NOTES:

For collections that use cl-naive-document-type there is a fallback the document-type is checked for keys as well and the collection's keys will be set to the keys set in the document-type elements.")
   (shard-elements :initarg :shard-elements
                   :accessor shard-elements
                   :initform nil
                   :documentation "shard-elements is a list of document element keywords to use for sharding."))

  (:documentation "A collection of documents of a specific document-type."))

(defclass store ()
  ((universe :initarg :universe
             :accessor universe
             :initform nil
             :documentation "The universe this store belongs to.")
   (name :initarg :name
         :accessor name
         :documentation "Store name.")
   (collection-class :initarg :collection-class
                     :accessor collection-class
                     :initform 'collection
                     :allocation :class
                     :documentation "The class that should be used to make collections.")
   (collections :initarg :collections
                :accessor collections
                :initform nil
                :documentation "List of collections represented by this store.")
   (location :initarg :location
             :accessor location
             :initform nil
             :documentation "The directory path to the document-type files and collection files for this store."))
  (:documentation "Document types and their associated collections are organized into groups called stores.

NOTES:

collection-class and document-type-class is delcaritively specied here because they are dynamicly created when definition files are loaded. The alternative would be defmethod hell where the customizer of naive-store would have to implement a whole lot of methods that do exactly what the provided methods do just to be able to be type specific in other methods where it is actually needed. Alternatively meta classes could be used for element-class but that opens another can of worms."))

(defclass universe ()
  ((multiverse :initarg :multiverse
               :accessor multiverse
               :initform nil
               :documentation "The multiverse this universe belongs to.")
   (name :initarg :name
         :accessor name
         ;;Initializing name to help with backwards
         ;;compatibility. Name was intruduced with multiverse and is
         ;;needed for references etc.
         :initform "universe"
         :documentation "Universe name.")
   (stores :initarg :stores
           :accessor stores
           :initform nil
           :documentation "List of stores contained by this universe.")
   (store-class :initarg :store-class
                :accessor store-class
                :initform 'store
                :allocation :class
                :documentation "The class that should be used to make stores.

NOTES:

store-class is delcaritively specied here because stores are dynamicly
created when definition files are loaded. (see store notes for more
about this.).")
   (location :initarg :location
             :accessor location
             :initform nil
             :documentation "Directory path to stores.")
   (shards-cache% :initarg :shards-cache%
                  :accessor shards-cache%
                  :initform
                  #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized nil)
                  #+(not (or sbcl ecl)) (make-hash-table :test 'equalp)
                  :documentation "This was introduced to speedup finding a shard. It is only for internal use!")
   (shards-macs-cache% :initarg :shards-macs-cache%
                       :accessor shards-macs-cache%
                       :initform
                       #+(or sbcl ecl) (make-hash-table :test 'equalp :synchronized nil)
                       #+(not (or sbcl ecl)) (make-hash-table :test 'equalp)
                       :documentation "This was introduced to speedup finding a shard. Calulating macs is expensive. It is only for internal use!"))
  (:documentation "Stores are held by a universe to make up a database."))

(defclass multiverse ()
  ((name :initarg :name
         :accessor name
         :documentation "Universe name.")
   (universes :initarg :universes
              :accessor universes
              :initform nil
              :documentation "List of universes contained by this multiverse.")
   (universe-class :initarg :universe-class
                   :accessor universe-class
                   :initform 'universe
                   :allocation :class
                   :documentation "The class that should be used to make universes.

NOTES:

universe-class is delcaritively specied here because stores are dynamicly created when definition
files are loaded. (see store notes for more about this.).")
   (location :initarg :location
             :accessor location
             :initform (cl-fad:merge-pathnames-as-directory
                        (user-homedir-pathname)
                        (make-pathname :directory (list :relative "multiverse")))
             :documentation "Directory path to stores."))
  (:documentation "Universes are held by a multiverse to make up a group of databases."))

(defmethod cl:print-object ((shard shard) stream)
  (if *print-readably*
      (format stream "(~S ~S)" (class-name (class-of shard))
              (list :status (slot-value shard 'status)
                    :number-of-documents (length (documents shard))
                    ;; there's no bt:lock-name function, so:
                    :locks (mapcar (function princ-to-string) (lock shard))
                    :mac (mac shard)
                    :location (location shard)))
      (print-unreadable-object (shard stream :type t :identity t)
        (format stream "~S" (list :status (slot-value shard 'status)
                                  :number-of-documents (length (documents shard))
                                  :locks (lock shard)
                                  :mac (mac shard)
                                  :location (location shard)))))
  shard)

(defmethod print-object ((collection collection) stream)
  (if *print-readably*
      (format stream "(~S ~S)" (class-name (class-of collection))
              (list :name (name collection)
                    :store (store collection)
                    :shards (shards collection)))
      (print-unreadable-object (collection stream :type t :identity t)
        (format stream "~S" (list :name (name collection)
                                  :store (name (store collection))
                                  :shards (map 'list (function short-mac)
                                               (shards collection))))))
  collection)

(defmethod print-object ((store store) stream)
  (if *print-readably*
      (format stream "(~S ~S)" (class-name (class-of store))
              (list :name (name store)
                    :collections (collections store)))
      (print-unreadable-object (store stream :type t :identity t)
        (format stream "~S" (list :name (name store)
                                  :colletions (map 'list (function name) (collections store))))))
  store)

(defmethod getx ((multiverse multiverse) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :name)
         (name multiverse))
        ((equalp accessor :universes)
         (universes multiverse))
        ((equalp accessor :universe-class)
         (universe-class multiverse))
        ((equalp accessor :location)
         (location multiverse))))

(defmethod (setf getx) (value (multiverse multiverse) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :name)
         (setf (name multiverse) value))
        ((equalp accessor :universes)
         (setf (universes multiverse) value))
        ((equalp accessor :universe-class)
         (setf (universe-class multiverse) value))
        ((equalp accessor :location)
         (setf (location multiverse) value))))

(defmethod getx ((universe universe) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :multiverse)
         (multiverse universe))
        ((equalp accessor :name)
         (name universe))
        ((equalp accessor :stores)
         (stores universe))
        ((equalp accessor :store-class)
         (store-class universe))
        ((equalp accessor :location)
         (location universe))))

(defmethod (setf getx) (value (universe universe) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :multiverse)
         (setf (multiverse universe) value))
        ((equalp accessor :name)
         (setf (name universe) value))
        ((equalp accessor :stores)
         (setf (stores universe) value))
        ((equalp accessor :store-class)
         (setf (store-class universe) value))
        ((equalp accessor :location)
         (setf (location universe) value))))

(defmethod getx ((store store) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :universe)
         (universe store))
        ((equalp accessor :name)
         (name store))
        ((equalp accessor :collections)
         (collections store))
        ((equalp accessor :collection-class)
         (collection-class store))
        ((equalp accessor :location)
         (location store))))

(defmethod (setf getx) (value (store store) accessor &key &allow-other-keys)
  ""
  (cond ((equalp accessor :universe)
         (setf (universe store) value))
        ((equalp accessor :name)
         (setf (name store) value))
        ((equalp accessor :collections)
         (setf (collections store) value))
        ((equalp accessor :collection-class)
         (setf (collection-class store) value))
        ((equalp accessor :location)
         (setf (location store) value))))

(defgeneric short-mac (shard)
  (:documentation "Return a short string containing a prefix of the MAC"))

(defmethod short-mac ((shard shard))
  (let ((mac (cl-naive-store.naive-core::mac shard)))
    (subseq mac 0 (min 8 (length mac)))))

(defgeneric (setf status) (new-status shard))
(defgeneric status (shard))

(defmethod (setf status) (new-status (shard shard))
  (setf (slot-value shard 'status) new-status))

(defmethod status ((shard shard))
  (let ((status (slot-value shard 'status)))
    (etypecase status
      (symbol status)
      (cons   (car status)))))

;;TODO: add parameter to select vector or list
(defmethod documents ((collection collection))
  (let ((documents))
    (do-sequence (shard (shards collection))
      (setf documents (concatenate 'list documents (documents shard))))
    documents))

(defun match-shard (filename shards)
  "Check filename against a list of shards to find the matching shard."
  (dolist (mac shards)
    (when (search (typecase mac
                    (shard
                     (mac mac))
                    (t mac)) (format nil "~A" filename) :test 'equal)
      (return-from match-shard (values mac filename)))))

(defgeneric get-shard (collection shard-mac &key &allow-other-keys)
  (:documentation "Get the shard object by its mac. Shard lookups are done so much that there is no choice but to cache them in a hashtable, but that hashtable needs to be thread safe so using safe functions to get and set."))

(defvar *shards-cache-lock* (bt:make-lock))

(defun get-shard-cache-safe% (collection shard-mac)
  (gethash-safe (frmt "~A-~A-~A"
                      (name (store collection))
                      (name collection)
                      (or shard-mac (name collection)))
                (shards-cache% (universe (store collection)))
                :lock *shards-cache-lock*))

(defun set-shard-cache-safe% (collection shard-mac shard)
  (setf (gethash-safe (frmt "~A-~A-~A"
                            (name (store collection))
                            (name collection)
                            (or shard-mac (name collection)))
                      (shards-cache% (universe (store collection)))
                      :lock *shards-cache-lock*)
        shard))

(defmethod get-shard :around (collection shard-mac &key &allow-other-keys)
  (let ((shard (get-shard-cache-safe% collection shard-mac)))
    (if (not shard)
        (call-next-method)
        shard)))

(defgeneric make-shard (collection shard-mac))

(defmethod make-shard (collection shard-mac)
  "Creates an instance of a shard using the supplied mac."
  (make-instance 'shard
                 :mac shard-mac
                 :location
                 (cl-fad:merge-pathnames-as-file
                  (pathname (ensure-location collection))
                  (make-pathname
                   ;;:directory (list :relative (name collection))
                   :name shard-mac
                   :type "log"))
                 :status :new))

(defmethod get-shard (collection shard-mac &key &allow-other-keys)
  (cl-naive-store.naive-core::get-shard-cache-safe% collection shard-mac)
  #|
  (find (or shard-mac (name collection))
  (shards collection)
  :test 'equal :key 'mac)
  |#)

(defvar *shards-macs-cache-lock* (bt:make-lock))

(defun get-shard-mac-cache-safe% (collection value)
  (gethash-safe value (shards-macs-cache% (universe (store collection)))
                :lock *shards-macs-cache-lock*))

(defun set-shard-mac-cache-safe% (collection value mac)
  (setf (gethash-safe value (shards-macs-cache% (universe (store collection)))
                      :lock *shards-macs-cache-lock*)
        mac))

(defun document-shard-mac (collection document)
  "Calculating a mac is expensive so caching shard value macs in a hashtable but that hashtable needs to be thread safe so using safe functions to get and set."
  (let ((value))
    ;;cant use lparallel the order of values are important.
    (dolist (element (shard-elements collection))
      (push (list element (getx document element)) value))

    (if value
        (let ((mac (get-shard-mac-cache-safe% collection value)))
          (unless mac
            (setf mac (naive-impl:make-mac (reverse value)))
            (set-shard-mac-cache-safe% collection value mac))
          mac)
        (name collection))))

(defgeneric query-multiverse (element fn)
  (:documentation "Queries the multiverse element passed for an element or elements."))

(defmethod query-multiverse ((collection collection) fn)
  (let ((result))
    (let ((fn-result (funcall fn collection)))
      (when fn-result
        (push fn-result result)))))

(defmethod query-multiverse ((store store) fn)
  (let ((result (mapcar (lambda (collection)
                          (query-multiverse collection fn))
                        (collections store))))
    (let ((fn-result (funcall fn store)))
      (when fn-result
        (push fn-result result)))))

(defmethod query-multiverse ((universe universe) fn)
  (let ((result (mapcar (lambda (store)
                          (query-multiverse store fn))
                        (stores universe))))
    (let ((fn-result (funcall fn universe)))
      (when fn-result
        (push fn-result result)))))

(defmethod query-multiverse ((multiverse multiverse) fn)
  (let ((result (mapcar (lambda (universe)
                          (query-multiverse universe fn))
                        (universes multiverse))))
    (let ((fn-result (funcall fn multiverse)))
      (when fn-result
        (push fn-result result)))))

(defmacro get-multiverse-element* (parent child-list-name name)
  (let ((parent% (gensym))
        (name% (gensym)))
    `(let ((,parent% ,parent)
           (,name% ,name))
       (dolist (element (,child-list-name ,parent%))
         (when (string-equal ,name% (name element))
           (return-from get-multiverse-element
             element))))))

(defgeneric get-multiverse-element (element-type parent name)
  (:documentation "Fetches an element of the type "))

(defmethod get-multiverse-element ((element-type (eql :universe))
                                   (multiverse multiverse) name)
  (get-multiverse-element* multiverse universes name))

(defmethod get-multiverse-element ((element-type (eql :store))
                                   (universe universe) name)
  (get-multiverse-element* universe stores name))

(defmethod get-multiverse-element ((element-type (eql :collection))
                                   (store store) name)
  (get-multiverse-element* store collections name))

;;TODO:Deprecated remove some time
(defgeneric get-store (universe store-name)
  (:documentation "Returns a store if found in the universe."))

;;TODO:Deprecated remove some time
(defmethod get-store ((universe universe) store-name)
  (get-multiverse-element :store universe store-name))

;;TODO:Deprecated remove some time
(defgeneric get-collection (store collection-name)
  (:documentation "Returns a collection document if found in the store."))

;;TODO:Deprecated remove some time
(defmethod get-collection ((store store) collection-name)
  (get-multiverse-element :collection store collection-name))

(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Writes various store structural objects to "))

(defmethod persist ((store store) &key &allow-other-keys)
  "Persists a store definition and not what it contains! Path to file is of this general format
/universe/store-name/store-name.store."
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location store))
    (make-pathname :name (name store)
                   :type "store"))
   (list :name (name store)
         :class (type-of store)
         :location (location store))

   :if-exists :supersede))

(defmethod persist ((universe universe) &key &allow-other-keys)
  "Persists a universe definition and not what it contains! Path to file is of this general format
/multiverse/universe-name/universe-name.universe."
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location universe))
    (make-pathname :name (name universe)
                   :type "universe"))
   (list :name (name universe)
         :class (type-of universe)
         :location (location universe))

   :if-exists :supersede))

(defmethod persist ((multiverse multiverse) &key &allow-other-keys)
  "Persists a universe definition and not what it contains! Path to file is of this general format
/multiverse/universe-name/universe-name.universe."
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location multiverse))
    (make-pathname :name (name multiverse)
                   :type "multiverse"))
   (list :name (name multiverse)
         :class (type-of multiverse)
         :location (location multiverse))

   :if-exists :supersede))

;;TODO: Need to sort out the inconsistencies between persist and
;;persist-collection-* funcitons some time. It is just plain confusing
;;even if it might have been convenient at one time.
(defgeneric persist-collection-def (collection)
  (:documentation "Persists a collection definition. Path to file is of this general format /multiverse/universe/store-name/collection-name.col."))

(defmethod persist-collection-def ((collection collection))
  (naive-impl:write-to-file
   (cl-fad:merge-pathnames-as-file
    (pathname (location (store collection)))
    (make-pathname :name (name collection)
                   :type "col"))
   (list
    :name (name collection)
    :class (type-of collection)
    :location (location collection))
   :if-exists :supersede))

;;TODO: handle hashtable with specialization
(defun persist-collection (collection)
  "Persists the documents in a collection in the order that they where added."

  (let ((channel (lparallel:make-channel)))
    (do-sequence (shard (shards collection) :parallel-p t)
      (lparallel:submit-task
       channel
       (lambda ()
         (naive-impl::with-open-file-lock
             (stream (location shard))

           (if (hash-table-p (documents shard))

               (maphash (lambda (key doc)
                          (declare (ignore key))
                          (persist-document collection doc :shard shard :file-stream stream))
                        (documents shard))
               (do-sequence (doc (documents shard))
                 (persist-document collection doc :shard shard :file-stream stream)))))))
    (dotimes (i (length (shards collection)))
      i
      (lparallel:receive-result channel))))

(defmethod persist ((collection collection) &key &allow-other-keys)
  "Persists a collection definition and the documents in a collection.
Path to file for data is this general format /multiverse/universe/store-name/collection-name/collection-name.log."
  (persist-collection-def collection)
  (persist-collection collection))

(defgeneric add-multiverse-element (parent element &key persist-p)
  (:documentation "Adds an instance of a multiverse element to the parent instance"))

(defun set-and-ensure-locations (parent child)
  "Used internally to create child location if it does not exist and to ensure the location exists."
  (if (location child)
      (ensure-directories-exist (pathname (location child)))
      (let ((location
              (cl-fad:merge-pathnames-as-directory
               (pathname (location parent))
               (make-pathname :directory (list :relative (name child))))))

        (ensure-directories-exist location)
        (setf (location child) (pathname location)))))

(defmacro add-multiverse-element* (parent child persist-p
                                   parent-type child-type child-list-name)
  (let ((parent% (gensym))
        (child% (gensym))
        (persist-p% (gensym)))
    `(let ((,parent% ,parent)
           (,child% ,child)
           (,persist-p% ,persist-p))
       (if (not (,parent-type ,child-type))
           (setf (,parent-type ,child-type) ,parent-type)
           (unless (eql (,parent-type ,child-type) ,parent-type)
             (error
              ,(format nil
                       "~A already references a different ~A instance!"
                       (string-capitalize child-type)
                       parent-type))))
       (unless (get-multiverse-element
                ,(intern (format nil "~:@(~A~)" child-type)
                         :keyword)
                ,parent% (name ,child%))

         (set-and-ensure-locations ,parent-type ,child-type)

         (setf (,parent-type ,child%) ,parent%)
         (if ,persist-p%
             (persist ,child%))
         (pushnew ,child% (,child-list-name ,parent%)))
       ,child%)))

(defmethod add-multiverse-element ((multiverse multiverse) (universe universe)
                                   &key persist-p)
  (add-multiverse-element* multiverse universe
                           persist-p
                           multiverse universe
                           universes))

(defmethod add-multiverse-element ((universe universe) (store store)
                                   &key persist-p)
  (add-multiverse-element* universe store
                           persist-p
                           universe store
                           stores))

;;Cant use add-multiverse-element* because the path merge is different

(defmethod add-multiverse-element ((store store) (collection collection)
                                   &key persist-p)
  (if (not (store collection))
      (setf (store collection) store)
      (unless (eql (store collection) store)
        (error "Collection already references a different store instance!")))

  (unless (get-multiverse-element :collection store (name collection))
    (let ((location (location collection)))
      (when location
        (ensure-directories-exist (pathname location)))
      (unless location
        (setf location
              (cl-fad:merge-pathnames-as-file
               (pathname (location store))
               (make-pathname
                :defaults (pathname (location store))
                :directory (list :relative (name collection))
                :name (name collection)
                :type "log"
                :version nil)))

        (ensure-directories-exist location))

      (setf (location collection) (pathname location))
      (pushnew collection (collections store))
      (setf (store collection) store)
      (when persist-p
        (persist-collection-def collection))))
  collection)

(defgeneric clear-collection (collection)
  (:documentation "Clears documents indexes etc from collection."))

(defmethod clear-collection (collection)
  (do-sequence (shard (shards collection))
    (remhash (frmt "~A-~A-~A"
                   (name (store collection))
                   (name collection)
                   (or (mac shard) (name collection)))
             (shards-cache% (universe (store collection))))
    (setf shard nil))
  (setf (shards collection) (make-array 1 :fill-pointer 0 :adjustable t :initial-element nil)))

(defgeneric remove-multiverse-element (parent element &key)
  (:documentation "Removes an instance of a multiverse element from the parent instance"))

(defmethod remove-multiverse-element ((store store) (collection collection) &key)
  (clear-collection collection)
  (setf (collections store) (remove collection (collections store))))

(defmethod remove-multiverse-element ((multiverse multiverse) (universe universe) &key)
  ;;Should we be bothering with this?
  (dolist (store (stores universe))
    (dolist (collection (collections store))
      (clear-collection collection)))
  (setf (universes multiverse) (remove universe (universes multiverse))))

(defmethod remove-multiverse-element ((universe universe) (store store) &key)
  ;;Should we be bothering with this?
  (dolist (collection (collections store))
    (clear-collection collection))
  (setf (stores universe) (remove universe (stores universe))))

;;TODO: Deprecated remove sometime
(defgeneric remove-collection (store collection)
  (:documentation "Removes a collection to a store."))

;;TODO: Deprecated remove sometime
(defmethod remove-collection ((store store) (collection collection))
  (clear-collection collection)
  (setf (collections store) (remove collection (collections store))))

(defgeneric load-data (collection &key shard-macs parallel-p &allow-other-keys)
  (:documentation "Loads the data documents of a collection from file or files if sharding is used. If the data is already loaded it wont reload it, if you want the data to be reloaded use force-reload-p.

shard-macs is a list of shard macs to indicate which shards should be used. If no shards are specified all shards will be loaded."))

(defgeneric ensure-location (object)
  (:documentation "Tries to find or build path to cl-naive-store files."))

(defmethod ensure-location ((object multiverse))
  (if (not (empty-p (location object)))
      (location object)
      (error "Unverse location is not set.")))

(defmethod ensure-location ((object universe))
  (if (not (empty-p (location object)))
      (location object)
      (if (not (multiverse object))
          (error "Universe multiverse not set, cant ensure location.")
          (if (not (empty-p (ensure-location (multiverse object))))
              (setf (location object)
                    (cl-fad:merge-pathnames-as-file
                     (pathname (location (multiverse object)))
                     (make-pathname :directory (list :relative (name object))
                                    :name (name object)
                                    :type "universe")))
              (error "Multiverse location is empty can't manufacture universe location from it.")))))

(defmethod ensure-location ((object store))
  (if (not (empty-p (location object)))
      (location object)
      (if (not (universe object))
          (error "Store universe not set, cant ensure location.")
          (if (not (empty-p (ensure-location (universe object))))
              (if (not (name object))
                  (error "Store has no name cannot manufacture location.")
                  (setf (location object)
                        (cl-fad:merge-pathnames-as-file
                         (pathname (location (universe object)))
                         (make-pathname :directory (list :relative (name object))
                                        :name (name object)
                                        :type "store"))))
              (error "Universe location is empty can't manufacture store location from it.")))))

(defmethod ensure-location ((object collection))
  (if (and (not (empty-p (location object)))
           (equalp (pathname-type (pathname (location object)))
                   "log"))
      (location object)
      (if (not (store object))
          (error "Collection store not set, cant ensure location.")
          (if (not (empty-p (ensure-location (store object))))
              (setf (location object)
                    (cl-fad:merge-pathnames-as-file
                     (pathname (location (store object)))
                     (make-pathname :directory
                                    (list :relative (name object))
                                    :name (name object)
                                    :type "log")))
              (error "Store location is empty can't manufacture collection location from it.")))))

(defgeneric data-loaded-p (container &key *allow-other-keys)
  (:documentation "Checks if the data is loaded for the container, be it universe , store or collection.

NOTES:

This physically checks each collection's underlying concrete data structure for data. This is done because a collection can be empty and still loaded, thus setting a status when loaded became confusing and could be missed by an over loading method.

If you change the underlying container for (shards collection) or the container for (docutments shard) you have to implement data-loaded-p. Your implementation is expected to physically check for document count > 0 and not some status set. Be smart about it you are not expected to return a count so dont waist time counting just check if there is at least one document in the container."))

(defmethod data-loaded-p ((collection collection) &key shard-macs &allow-other-keys)

  (let ((all-shards-p nil))
    (if (not shard-macs)
        (if (and collection
                 (shards collection)
                 (> (fill-pointer (shards collection)) 0))
            (do-sequence (shard-found (shards collection))
              (if (or
                   (equalp (status shard-found) :loaded)
                   (> (length (documents shard-found)) 0))
                  (push shard-found all-shards-p)
                  (push nil all-shards-p))))

        (do-sequence (mac shard-macs)
          (let ((shard-found (cl-naive-store.naive-core::get-shard-cache-safe% collection mac)))
            (if shard-found
                (push shard-found all-shards-p)
                (push nil all-shards-p)))))

    (if all-shards-p
        (every (lambda (x) x) all-shards-p))))

(defmethod data-loaded-p ((store store) &key &allow-other-keys)
  (let ((loaded-p t))
    (dolist (collection (collections store))
      (unless (data-loaded-p collection)
        (return-from data-loaded-p nil)))
    loaded-p))

(defmethod data-loaded-p ((universe universe) &key &allow-other-keys)
  (let ((loaded-p t))
    (dolist (store (stores universe))
      (unless (data-loaded-p store)
        (return-from data-loaded-p nil)))
    loaded-p))
