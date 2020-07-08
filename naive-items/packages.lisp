(in-package :common-lisp-user)

(defpackage :cl-naive-items
  (:use :cl :cl-naive-store :cl-naive-indexed :cl-naive-data-types :cl-naive-data-type-defs)
  (:export

   ;;Items
   :item-collection
   :item-store
   
   :item
   :make-item
   :item-hash
   :item-bucket-key
   :item-values
   :item-changes
   :item-versions
   :item-collection
   :item-data-type
   :item-store
   :item-persisted-p
   :item-deleted-p
   :item-p

   :item-of-type-p

   :item-list-p
   :item-of-type-p
   :getxo
   :getxn

   :find-equalp-item

   ;;####Export
   :items-to-plist
   :item-list-to-csv
   :item-list-to-json))
