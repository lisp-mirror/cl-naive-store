(in-package :cl-naive-store)

(defgeneric persist (object &key &allow-other-keys)
  (:documentation "Persist is used to write \"stuff\" to files."))
  
(defmethod persist ((list list) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes a list of data documents to a file as individual documents."
  
  (naive-impl:with-open-file-lock (stream file :if-exists if-exists)
    (write list :stream stream)))


(defmethod persist ((hash hash-table) &key file (if-exists :append)
				  &allow-other-keys)
  "Writes the values of a hash table to a file as individual documents."
  
  (naive-impl:with-open-file-lock (stream file :if-exists if-exists)
    (write hash :stream stream)))



