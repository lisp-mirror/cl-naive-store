(in-package :cl-naive-indexed)

(defmethod naive-impl:find-document-by-hash ((collection indexed-collection-mixin) hash
					     &key shards &allow-other-keys)
 (index-lookup-hash
  collection
  hash :shards shards))

(defmethod naive-impl:compose-special ((collection indexed-collection-mixin)
				       shard
				       sexp (type (eql :document)))
  (if (getx sexp :deleted-p)
	(remove-document collection sexp :shard shard)
	;;TODO: Where to get handle-duplicates-p ???
	(add-document collection sexp :shard shard)))


