(in-package :cl-naive-indexed)

(defmethod naive-impl:find-document-by-hash ((collection indexed-collection-mixin) hash
					     &key shards &allow-other-keys)
  (naive-impl::debug-log
   (format nil "indexed:find-document-by-hash :document ~A~%" (name collection)))
  (let ((doc 
	  (index-lookup-hash
	   collection
	   hash :shards shards)))

    (naive-impl::debug-log (format nil "END find-document-by-hash :document ~A~%" (name collection)))

    doc))

(defmethod naive-impl:compose-special ((collection indexed-collection-mixin)
				       shard
				       sexp (type (eql :document)))
  (naive-impl::debug-log (format nil "Indexed:Compose-special :document ~A~%" (name collection)))
  (if (getx sexp :deleted-p)
      (progn
	;;(break "poesisie")
	(remove-document collection sexp :shard shard)
	;;(break "hoer ~A~%~A" shard sexp)
	)
      
	;;TODO: Where to get handle-duplicates-p ???
	(add-document collection sexp :shard shard)))


