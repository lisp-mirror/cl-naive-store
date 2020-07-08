(in-package :cl-naive-store)

(defgeneric deleted-p (object)
  (:documentation "Indicates if a data object has been marked as deleted. 

naive-store writes data to file sequentially and when deleting data objects it does not remove a data object from the underlying file it just marks it as deleted."))

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
	(remove (if (keys collection)
		    (key-values collection object)
		    object)
		(data-objects collection)
		:test #'equalp :key (lambda (objectx)
				      (if (keys collection)
					  (key-values collection objectx)
					  objectx)))))

(defgeneric delete-data-object (collection object &key &allow-other-keys))

(defmethod delete-data-object ((collection collection) object &key &allow-other-keys)
    (remove-data-object collection object)
    (setf (deleted-p object) t)
    (persist-object collection object :delete-p t))

(defgeneric add-data-object (collection object &key &allow-other-keys)
  (:documentation "Adds data object to a collection. 

Notes:

This method in combination with remove-data-object and data-objects slot of the collection can be used to customize the container (list,array,hash etc) used for data objects."))

(defgeneric object-values (object))

(defmethod object-values (object)
  object)

(defgeneric key-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of key values from the values of a data object.
Looks for :key or uses all values"))

(defmethod key-values ((collection collection) values &key &allow-other-keys)  
  (loop for (a b) on values by #'cddr
     when (member a (keys collection))
     do (return (list (list a b)))
     unless (or (equalp a :hash)
		(equalp a :deleted-p))
     :collect (list a b)))


(defmethod add-data-object ((collection collection) object
			    &key (handle-duplicates-p t) &allow-other-keys)
  "To get a better performance for first time you add any data you can set handle-duplicates-p to nil to speed up things."
  (if (not handle-duplicates-p)
      (push object
	    (data-objects collection))
      
      (if (keys collection)
	  (let ((position
		 (position object (data-objects collection)
			   :test (lambda (x y)
				   (equalp (key-values collection x) (key-values collection y))))))
	    (if position
		(setf (nth position (data-objects collection)) object)
		(push object (data-objects collection))))
	  (push object
		  (data-objects collection))))
  object)

(defgeneric persist-object (collection object &key &allow-other-keys)
  (:documentation "The default behavior is two just write what ever is given to file.
collection is needed to write to the right file and directory.

Notes:

This is where tasks checking for duplicates should be done. This is also where reference objects should be converted to a reference% marker instead of writing out the actual object. 
Use naive-items if the later behaviour is desired."))
 
(defmethod persist-object ((collection collection) object &key (handle-duplicates-p t) delete-p &allow-other-keys)
  "Writes an data object to file and adds it to the collection. 

Notes:

Duplicate checking is very rudementry in cl-naive-store core, it is assumed that if the object you are persisting has the same key that it should replace what is in the db thus the object is always written to the file, but it wont cause a duplicate in the database in memory.

For more advanced duplicate checking and not writing out objects when they are equal value wise look to cl-naive-items."

  
  (write-to-file
   (cl-fad:merge-pathnames-as-file
	       (pathname (location collection))
	       (make-pathname :name (name collection)
			      :type "log"))
   
   (if (not delete-p)
       (add-data-object collection object :handle-duplicates-p handle-duplicates-p)
       object)))


