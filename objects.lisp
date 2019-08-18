(in-package :cl-naive-store)

(defgeneric persist-object (collection object &key &allow-other-keys)
  (:documentation "The default behavior is two just write what ever is given to file.
Collection is needed to write to the right file and directory.

However this is where tasks checking for duplicates should be done. This is also where 
reference objects should be converted to a reference% marker instead of writing out the actual object. 
Use naive-items if the later behaviour is desired."))
 
(defmethod persist-object ((collection collection) object &key (handle-duplicates-p nil) delete-p &allow-other-keys)
  "Writes an data object to file and adds it to the collection."
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location collection))
	       (make-pathname :name (name collection)
			      :type "log"))
   (if (not delete-p)
       (add-data-object collection object :handle-dupliates-p handle-duplicates-p)
       object)))

(defgeneric deleted-p (object)
  (:documentation "Indicates if a data object has been marked as deleted. 

naive-store writes data to file sequentially and when deleting data objects it does not 
remove a data object from the underlying file it just marks it as deleted."))

(defmethod deleted-p (object)
  (getx object :deleted-p))

(defgeneric (setf deleted-p) (value object &key &allow-other-keys))

(defmethod (setf deleted-p) (value object &key &allow-other-keys)
  (setf (getx object :deleted-p) value)
  object)

(defgeneric remove-data-object (collection object &key &allow-other-keys)
  (:documentation "Removes an object from the collection and its indexes. See add-data-object."))

(defmethod remove-data-object ((collection collection) object &key &allow-other-keys)
  (setf (data-objects collection)
	(remove object (data-objects collection)
		:test #'equalp)))

(defgeneric delete-data-object (collection object &key &allow-other-keys))

(defmethod delete-data-object ((collection collection) object &key &allow-other-keys)
    (remove-data-object collection object)
    (setf (deleted-p object) t)
    (persist-object collection object :delete-p t))

(defgeneric add-data-object (collection object &key &allow-other-keys)
  (:documentation "Adds data object to a collection. This method in combination with remove-data-object,
 and data-objects slot of the collection can be used to customize the container (list,array,hash etc) 
used for data objects. "))

(defmethod add-data-object ((collection collection) object &key (handle-duplicates-p nil) &allow-other-keys)
  "Handling duplicates makes adding objects exponentially slower!! If there are a lot of objects in your
collections and you need duplicate handling use naive-store-indexed."
  (if handle-duplicates-p
      (pushnew object
	       (data-objects collection))
      (push object
	    (data-objects collection)))
  object)
