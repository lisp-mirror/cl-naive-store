;;;; This is an implementation of merkle trees for naive-documents to be used in the naive-api etc.
;;;; It differs from normal merkle-trees in that it does not use "chunking" to break up a file
;;;; but instead uses the structure of the document and any sub documents to create the merkle tree.
;;;; Because it mirrors the document structure we did not use binary trees, a "full" tree is used.
;;;; It also uses two hashes per entry in the tree, the document hash and the computed hash for the
;;;; document, this is to help with partial lookups from different "sources" and lookups within
;;;; the merkle tree. The additional hash also helps with the incremental creation of a tree as
;;;; the document changes over time. The idea is to maintain the merkle tree hashes with the
;;;; document so the merkle tree is not completely computed each time the document is passed
;;;; around.
;;;;
;;;; Nodes are represented as clos objects internally because it just simplifies the implementation
;;;; considerably. I could have used naive documents but that would have limited the use
;;;; to naive-documents. Naive documents would also be complete over kill. Also storing the parent
;;;; becomes an issue because a document could have many parents accross the store.
;;;;
;;;; Notes for further development:
;;;; If I store the merkle hashes in the documents I could remove the requirement to use clos
;;;; objects but then I would have to remove parent and the simplifications it brings.

(in-package :cl-naive-merkle)

(defclass merkle-node ()
  ((parent :initarg :parent
	   :initform nil
	   :accessor parent)
   (merkle-hash :initarg :merkle-hash
		:initform nil
		:accessor merkle-hash
		:documentation "Then original hash of the document.")
   (merkle-sum :initarg :merkle-sum
		:initform nil
		:accessor merkle-sum
		:documentation "Then computed hash of the document")
   (children :initarg :children
	     :initform nil
	     :accessor children)))


(defun node-p (node)
  (if (listp node)
      (getf node :children)
      (children node)))

(defun leaf-p (node)
  (not (node-p node)))

(defun recalc (node)  
  (labels ((parent% (node)
	     (let ((sum 0))
	       (dolist (nd (children node))
		 (setf sum (+ sum (or (merkle-sum nd)
				      (merkle-hash nd)))))

	       (setf (merkle-sum node) sum))
	       (when (parent node)                 
		 (parent% (parent node)))))      
    (parent% node)
    node))

(defun make-node (hash &key parent children hash-sum)
  "Returns a new instance of node object representing the hash node."
  (let ((node (if children
		  (make-instance 'merkle-node
				 :parent parent
				 :merkle-hash hash
				 :merkle-sum (or hash-sum hash)
				 :children (if (listp children)
					       children
					       (list children)))
		  (make-instance 'merkle-node
				 :parent parent
				 :merkle-hash hash
				 :merkle-sum (or hash-sum hash)))))
    (when parent
	(if (leaf-p parent)	    
	      (setf (children parent) (list node))
	      (setf (children parent) (append (children parent) (list node))))
	(recalc parent))
    node))

(defun calc (node)
  "Sums hashes for node branch including root."
  (let ((sum 0))
    (labels ((parent% (node)
	       (dolist (nd (children node))
		 (setf sum (+ sum (or (merkle-sum nd)
				      (merkle-hash nd)))))

	       (if (parent node)
		   (setf sum (- sum (or (merkle-sum node)
					(merkle-hash node)))))
	       
	       (when (parent node)
		 (parent% (parent node)))))
      
      (parent% (parent node))
      sum)))

(defun calc-branch (node)
  "Sums hashes excluding root."
  (let ((sum 0))
    (labels ((parent% (node)
	       (dolist (nd (children node))
		 (setf sum (+ sum (or (merkle-sum nd)
				      (merkle-hash nd)))))

	       (when (and (parent node) (parent (parent node)))
		 (if (parent node)
		   (setf sum (- sum (or (merkle-sum node)
					(merkle-hash node)))))
		 (parent% (parent node)))))
      
      (parent% (parent node))
      sum)))

(defun map-tree (tree function)
  (cond ((null tree))
	((consp tree)
	 (map-tree (car tree) function)
	 (map-tree (cdr tree) function))
	((leaf-p tree)
	 (funcall function tree))
	((node-p tree)
	 (funcall function tree)
	 (map-tree (children tree) function))	
	(t
	 (error "Malformed tree:~%~% ~S" tree))))


(defun branch (node)
  (let ((nodes))
    (labels ((parent% (node)
	       (when (parent node)
		 (pushnew (parent node) nodes)
		 (parent% (parent node)))))
      (push node nodes)
      (parent% node)
      nodes)))

(defun tree-to-reference-tree (tree)
  (let ((ref-tree (list :hash (merkle-hash tree)
			:hash-sum (merkle-sum tree)
			:children nil)))
    (labels ((traverse-tree (node parent)
	       (cond ((null node))
		     ((consp node)
		      (traverse-tree (car node) parent)
		      (traverse-tree (cdr node) parent))
		     ((leaf-p node)
		      (let ((ref-node (list :hash (merkle-hash node)
						  :hash-sum (merkle-sum node)
						  :children nil)))
			(setf (getf parent :children)
			      (append (getf parent :children)
				      (list ref-node)))))
		     ((node-p node)
		      (let ((ref-node (list :hash (merkle-hash node)
					    :hash-sum (merkle-sum node)
					    :children nil)))
			(setf (getf parent :children)
			      (append (getf parent :children)
				      (list ref-node)))
			(traverse-tree (children node) ref-node)))	
		     (t
		      (error "Malformed tree:~%~% ~S" node)))))
      (traverse-tree (children tree) ref-tree))
    ref-tree))

(defun tree-from-reference-tree (ref-tree)
  (let ((tree (make-node (getf ref-tree :hash))))
    (labels ((traverse-tree (tree parent)
	       (cond ((null tree))
		     ((consp (car tree))                     
		      (traverse-tree (car tree) parent)
		      (traverse-tree (cdr tree) parent))
		     ((leaf-p tree)
		      (make-node (getf tree :hash)
				 :parent parent
				 :hash-sum (getf tree :hash-sum))) 
		     ((node-p tree)		      
		      (traverse-tree (getf tree :children)
				     (make-node (getf tree :hash)
						:parent parent
						:hash-sum (getf tree :hash-sum)
						:children nil)))
		     (t
		      (error "Malformed tree:~%~% ~S" tree)))))
      
      (traverse-tree (getf ref-tree :children) tree))
    tree))


(defun make-hash (value)
  (ironclad:octets-to-integer
   (ironclad:digest-sequence
    :tiger
    (babel:string-to-octets
     (cl-naive-store:frmt
      "~A" value) ))))

(defun make-document-hash (document)
  (make-hash
   (cl-naive-store:key-values
    (cl-naive-documents:document-collection document)
    document)))

(defun calc-document (document)
  (let ((merkle-tree))
    (labels ((traverse-doc (doc parent)
	       (when (cl-naive-documents:document-p doc)
		 (let* ((node (make-node
			       (make-document-hash
				doc)
			       :parent parent)))
		   
		   (loop for (key value) on (cl-naive-documents:document-elements doc) by #'cddr
		      when (cl-naive-documents:document-p value)
			do
			(traverse-doc value node))))))
      (setf merkle-tree (make-node (make-document-hash document) ))
      (traverse-doc document merkle-tree))
    merkle-tree))


#|


(let ((buffer )
      
      (crypt))

  (time
   (dotimes (x 1000000)
     (progn
       (setf buffer (babel:string-to-octets "fuck"))
       
       (setf crypt (ironclad:digest-sequence :tiger buffer))
       
       ;;(print crypt)
       (ironclad::octets-to-integer crypt)
       ;;(print (ironclad:integer-to-octets (bit-smasher:octets->int crypt)))

       )))

  

   
  
  )

(let* ((tree)
       (two)
       (seven)
       (nine)
       (eleven)
       (thirteen))


  (setf tree (make-node 1))
  (setf two (make-node 2 :parent tree))
  (make-node 3 :parent two)
  
  (make-node 4 :parent two)

  (setf seven (make-node 7 :parent tree))
  (make-node 8 :parent seven)
  
  (setf nine (make-node 9 :parent tree))
  (make-node 10 :parent nine)
  
  (setf eleven (make-node 11 :parent nine))
  (make-node 12 :parent eleven)
  
  (setf thirteen (make-node 13 :parent eleven)) 

  (format t "~A~%"  (calc thirteen))
  (format t "~A~%"  (calc-branch thirteen))
  (format t "~A" (merkle-hash tree))

(print (tree-to-reference-tree tree))

'(:HASH 1 :HASH-SUM 972 :CHILDREN
 ((:HASH 2 :HASH-SUM 4 :CHILDREN ((:HASH 3 :HASH-SUM 3) (:HASH 4 :HASH-SUM 4)))
  (:HASH 7 :HASH-SUM 7 :CHILDREN ((:HASH 8 :HASH-SUM 8)))
  (:HASH 9 :HASH-SUM 72 :CHILDREN
   ((:HASH 10 :HASH-SUM 10)
    (:HASH 11 :HASH-SUM 22 :CHILDREN
     ((:HASH 12 :HASH-SUM 12) (:HASH 13 :HASH-SUM 13)))))))



  )




|#
