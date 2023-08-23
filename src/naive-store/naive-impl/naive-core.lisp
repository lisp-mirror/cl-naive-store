(in-package :naive-impl)

(defun get-store* (universe name)
  "Used internally to find or return a store created from its store definition.."
  (let ((store (get-store universe name)))
    (unless store
      (setf store (cl-naive-store.naive-core::instance-from-definition-file
                   (location universe) "store" name (store-class universe)))
      (add-store universe store))
    store))

(defun get-collection* (store name)
  "Used internally to find or create a new collection."
  (let ((collection (get-collection store name)))
    (unless collection
      (setf collection (get-collection-from-def store name))

      (when collection
        (add-collection store collection))

      (unless collection
        (error "Could not create collection ~A in store ~A" name (and store (name store)))))
    collection))
