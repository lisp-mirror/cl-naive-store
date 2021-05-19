(defpackage :naive-impl
  (:use :cl :cl-getx :cl-naive-store)
  (:export

   ;;COMMON

   :frmt
   :map-append
   :maphash-collect
   :frmt
   :empty-p
   :trim-whitespace
   :plist-to-values
   :plist-to-pairs
   :make-mac
   :%loading-shard%
   :*lock*
   :gethash-safe
   :remhash-safe
   :*disable-parallel-p*
   :do-sequence

   ;;LOGGING

   :*break-on-error-log*
   :write-log
   :*debug-log-p*
   :debug-log

   ;;FILES
   :file-to-string
   :with-file-lock
   :with-open-file-lock
   :write-to-file
   :write-list-to-file
   :sexp-from-file

   ;;PARSING
   :load-document-reference-collection
   :find-document-by-hash
   :type-of-sexp
   :compose-special
   :compose-parse
   :compose-document

   ;;PERSISTING
   :type-of-doc-element
   :persist-form
   :persist-parse
   :persist-document
   :persist-delete-document))
