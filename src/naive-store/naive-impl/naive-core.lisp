(in-package :naive-impl)

;;TODO:Deprecated remove some time
(defun get-store* (universe name)
  "Used internally to find or return a store created from its store definition.."
  (let ((store (get-multiverse-element :store universe name)))
    (unless store
      (setf store (cl-naive-store.naive-core::load-from-definition-file
                   universe
                   :store
                   name
                   :class (store-class universe)))
      (add-multiverse-element universe store))
    store))

;;TODO:Deprecated remove some time
(defun get-collection* (store name)
  "Used internally to find or create a new collection."
  (let ((collection (get-multiverse-element :collection store name)))
    (unless collection
      (setf collection (load-from-definition-file store :collection name))
      (unless collection
        (error "Could not create collection ~A in store ~A" name (and store (name store)))))
    collection))
