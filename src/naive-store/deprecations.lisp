(in-package :cl-naive-store.naive-core)

;;TODO: get-collection-from-def should join this list see todo

(cl-naive-deprecation:declare-deprecation

 (function get-store* deprecated)
 (function get-collection* deprecated)
 (function get-collection-from-def deprecated)
 (function find-store-definitions deprecated)
 (function find-collection-definitions deprecated)
 (function load-store-collections deprecated)

 (generic-function get-store (universe store-name) replaced-by
                   get-multiverse-element as
                   (let ((universe% (gensym))
                         (store-name% (gensym)))
                     `(let ((,universe% ,universe)
                            (,store-name% ,store-name))
                        (get-multiverse-element :store ,universe%
                                                ,store-name%))))

 (generic-function add-store (universe store) replaced-by
                   add-multiverse-element as
                   (let ((universe% (gensym))
                         (store% (gensym)))
                     `(let ((,universe% ,universe)
                            (,store% ,store))
                        (add-multiverse-element ,universe%
                                                ,store%
                                                :persist-p t))))

 (generic-function get-collection (store collection-name) replaced-by
                   get-multiverse-element as
                   (let ((store% (gensym))
                         (collection-name% (gensym)))
                     `(let ((,store% ,store)
                            (,collection-name% ,collection-name))
                        (get-multiverse-element :collection ,store%
                                                ,collection-name%))))

 (generic-function remove-collection (store collection) replaced-by
                   remove-multiverse-element as
                   (let ((store% (gensym))
                         (collection% (gensym)))
                     `(let ((,store% ,store)
                            (,collection% ,collection))
                        (remove-multiverse-element ,store% ,collection%))))

 (generic-function add-collection (store collection) replaced-by
                   add-multiverse-element as
                   (let ((store% (gensym))
                         (collection% (gensym)))
                     `(let ((,store% ,store)
                            (,collection% ,collection))
                        (add-multiverse-element ,store% ,collection%
                                                :persist-p t))))

 (function get-store-from-def (universe store-name) replaced-by
           load-from-definition-file as
           (let ((universe% (gensym))
                 (store-name% (gensym)))
             `(let ((,universe% ,universe)
                    (,store-name% ,store-name))
                (instance-from-definition-file
                 ,universe%
                 :store
                 ,store-name%))))

 (function load-store (store &key with-data-p) replaced-by
           load-from-definitions as
           (let ((store% (gensym))
                 (with-data-p% (gensym)))
             `(let ((,store% ,store)
                    (,with-data-p% ,with-data-p))
                (load-from-definitions
                 ,store%
                 :collection
                 :class nil
                 :with-children-p t
                 :with-data-p ,with-data-p%))))

 (function load-stores (universe &key with-collections-p with-data-p) replaced-by
           load-from-definitions as
           (let ((universe% (gensym))
                 (with-children-p% (gensym))
                 (with-data-p% (gensym)))
             `(let ((,universe% ,universe)
                    (,with-children-p% ,with-collections-p)
                    (,with-data-p% ,with-data-p))
                (load-from-definitions
                 ,universe%
                 :store
                 :class nil
                 :with-children-p ,with-children-p%
                 :with-data-p ,with-data-p%))))

 (function load-collections (store &key with-data-p) replaced-by
           load-from-definitions as
           (let ((store% (gensym))
                 (with-data-p% (gensym)))
             `(let ((,store% ,store)
                    (,with-data-p% ,with-data-p))
                (load-from-definitions
                 ,store%
                 :collection
                 :class nil
                 :with-children-p t
                 :with-data-p ,with-data-p%)))))
