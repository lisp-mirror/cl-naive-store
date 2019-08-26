(in-package :cl-naive-items)

(defun item-values% (values)
  (if (item-p values)
      (item-values values)
      values))

