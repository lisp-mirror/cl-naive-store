(in-package :cl-naive-indexed)

(defclass avl-tree ()
  ((key
    :initarg :node-key
    :reader node-key)
   (value
    :initarg :node-value
    :accessor node-value
    ;;    :reader node-value
    )
   (left
    :initarg :left-child
    :reader left-child)
   (right
    :initarg :right-child
    :reader right-child))
  (:documentation
   "A node in an AVL tree."))

(defun make-node (key value left right)
  "Binary tree node with association and branches."
  (make-instance
   'avl-tree :node-key key :node-value value
   :left-child left :right-child right))

(defmethod tree-height ((tree null))
  "Get the height of an empty TREE."
  0)

(defmethod tree-height ((tree avl-tree))
  "Get height of TREE."
  (1+ (max (tree-height (left-child tree))
           (tree-height (right-child tree)))))

(defun balance-factor (node)
  "Get balance factor of subtree rooted at NODE."
  (ecase (- (tree-height (right-child node))
            (tree-height (left-child node)))
    (-2 :imbalanced-left)
    (-1 :left-heavy)
    ( 0 :balanced)
    (+1 :right-heavy)
    (+2 :imbalanced-right)))

(defmethod rotate-left ((node avl-tree))
  "Return TREE rotated left."
  (with-slots (key value height left right) node
    (avl-node
     (node-key right)
     (node-value right)
     (avl-node key value
               left (left-child right))
     (right-child right))))

(defmethod rotate-right ((node avl-tree))
  "Return TREE rotated right."
  (with-slots (key value height left right) node
    (avl-node
     (node-key left)
     (node-value left)
     (left-child left)
     (avl-node key value
               (right-child left) right))))

(defun avl-node (key value &optional left right)
  "Balanced AVL tree node."
  (let ((node (make-node key value left right)))
    (ecase (balance-factor node)
      ((:left-heavy :balanced :right-heavy)
       node)

      (:imbalanced-left
       (ecase (balance-factor left)
         (:left-heavy
          (rotate-right node))
         (:right-heavy
          (avl-node key value
                    (rotate-left left) right))))

      (:imbalanced-right
       (ecase (balance-factor right)
         (:left-heavy
          (avl-node key value
                    left (rotate-right right)))
         (:right-heavy
          (rotate-left node)))))))

(defmethod lessp ((a number) &rest rest)
  (apply #'< a rest))

(defmethod insert (key value (tree null))
  "Insert pair of KEY and VALUE in an empty TREE."
  (avl-node key value nil nil))

(defmethod insert (key value (tree avl-tree))
  "Add an association from KEY to VALUE in TREE."
  (avl-node
   (node-key tree)
   (node-value tree)
   (if (lessp key (node-key tree))
       (insert key value
               (left-child tree))
       (left-child tree))
   (if (lessp key (node-key tree))
       (right-child tree)
       (insert key value
               (right-child tree)))))


(defmethod lookup (key (tree null))
  "Lookup KEY in the empty TREE."
  nil)

(defmethod lookup (key (tree avl-tree))
  "Return all values associated with KEY in TREE."
  (with-slots ((node-key key) value left right)
      tree
    (cond
      ((lessp key node-key) (lookup key left))
      ((lessp node-key key) (lookup key right))
      (t (cons value
               (append (lookup key left)
                       (lookup key right)))))))
