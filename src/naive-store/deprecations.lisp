(in-package :cl-naive-store.naive-core)

;;TODO: get-collection-from-def should join this list see todo

(cl-naive-deprecation:declare-deprecation

 (function get-store* deprecated)
 (function get-collection* deprecated)

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
                                                ,store%))))

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
                        (add-multiverse-element ,store% ,collection%))))

 (function get-store-from-def (universe store-name) replaced-by
           instance-from-definition-file as
           (let ((universe% (gensym))
                 (store-name% (gensym)))
             `(let ((,universe% ,universe)
                    (,store-name% ,store-name))
                (instance-from-definition-file (location ,universe%)
                                               ,universe%
                                               :store
                                               ,store-name%)))))
