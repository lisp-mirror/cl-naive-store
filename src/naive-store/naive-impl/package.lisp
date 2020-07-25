(defpackage :naive-impl
  (:use :cl :cl-getx :cl-naive-store)
  (:export

   :frmt
   :map-append
   :maphash-collect
   :frmt
   :empty-p
   :trim-whitespace
   :plist-to-values
   :plist-to-pairs
   :*break-on-error-log*
   :write-log
   :file-to-string
   
   :with-file-lock
   :with-open-file-lock
   :wrap-in-list
   :wrap-in-loader

   :write-to-file
   :write-list-to-file

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
   :persist-delete-document
   
   
))
