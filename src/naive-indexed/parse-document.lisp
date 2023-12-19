(in-package :cl-naive-store.naive-indexed)

(defmethod naive-impl:find-document-by-hash ((collection indexed-collection-mixin) hash
                                             &key shards &allow-other-keys)
  (naive-impl::debug-log "indexed:find-document-by-hash :document ~A" (name collection))

  (let ((doc
          (index-lookup-hash
           collection
           hash :shards shards)))

    (naive-impl::debug-log "END find-document-by-hash :document ~A" (name collection))

    doc))

(defmethod naive-impl:compose-special ((collection indexed-collection-mixin)
                                       shard
                                       sexp (type (eql :document))
                                       &key (handle-duplicates-p t) &allow-other-keys)
  (naive-impl::debug-log "Indexed:Compose-special :document ~A sexp ~S shard ~S"
                         (name collection)
                         sexp
                         (subseq (cl-naive-store.naive-core::mac shard) 0 8))
  (if (getx sexp :deleted-p)
      (remove-document collection sexp :shard shard)
      ;;TODO: Where to get handle-duplicates-p ???
      (add-document collection sexp :shard shard
                                    :handle-duplicates-p handle-duplicates-p)))

