(in-package :cl-naive-store.document-types)

(cl-naive-deprecation:declare-deprecation

 (function load-store-document-types deprecated)

 (generic-function get-document-type-from-def (store document-type-name) replaced-by
                   instance-from-definition-file as
                   (let ((store% (gensym))
                         (document-type-name% (gensym)))
                     `(let ((,store% ,store)
                            (,document-type-name% ,document-type-name))
                        (instance-from-definition-file (location ,store%)
                                                       ,store%
                                                       :document-type
                                                       ,document-type-name%))))

 (generic-function add-document-type (store document-type &key persist-p) replaced-by
                   add-multiverse-element as
                   (let ((store% (gensym))
                         (document-type% (gensym))
                         (persist-p% (gensym)))
                     `(let ((,store% ,store)
                            (,document-type% ,document-type)
                            (,persist-p% ,persist-p))
                        (add-multiverse-element ,store%
                                                ,document-type%
                                                :perist-p persist-p%)))))
