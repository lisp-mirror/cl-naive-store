(in-package :cl-naive-store.document-types)

(cl-naive-deprecation:declare-deprecation

 (generic-function get-document-type-from-def (store document-type-name) replaced-by
                   instance-from-definition-file as
                   (let ((store% (gensym))
                         (document-type-name% (gensym)))
                     `(let ((,store% ,store)
                            (,document-type-name% ,document-type-name))
                        (instance-from-definition-file (location ,store%)
                                                       ,store%
                                                       :document-type
                                                       ,document-type-name%)))))
