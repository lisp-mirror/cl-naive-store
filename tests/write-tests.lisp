(defun test-list()
  (let ((stream (make-string-output-stream))
	(vals (list 1 2 3)))
    (cl-naive-store:write-document vals stream)
    (let ((string (get-output-stream-string stream)))
      (values
       (equalp string "(1 2 3)")
       string))))

(defun test-hash ()
  (let ((stream (make-string-output-stream))
	(vals (make-hash-table)))

    (dolist (val (list 1 2 3))
      (setf (gethash val vals) val))
      
    (cl-naive-store:write-document vals stream)
    (let ((string (get-output-stream-string stream)))
      (values
       (equalp string "(:|#HASH-TABLE| (:KEY 1 :OBJECT 1) (:KEY 2 :OBJECT 2) (:KEY 3 :OBJECT 3) )")
       string))))
