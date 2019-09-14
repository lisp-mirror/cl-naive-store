(in-package :cl-naive-items)

(defstruct item
  "Data is loaded into these structures from files. Changes slot is used to store setf values
when using getx the preffered accessor for values. This helps with comparing of values when persisting."
  store
  collection
  data-type
  hash
  values
  changes
  versions
  deleted-p
  persisted-p)

(defmethod object-values ((object item))
  (item-values object))

