(in-package :cl-naive-indexed)

(defmethod naive-impl:find-document-by-hash (collection hash)
 (index-lookup-hash
  collection
  hash))

(defmethod naive-impl:compose-special (collection sexp (type (eql :document)))
  (if (getx sexp :deleted-p)
	(remove-document collection sexp)
	;;TODO: Where to get handle-duplicates-p ???
	(add-document collection sexp)))


