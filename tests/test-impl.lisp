(ignore-errors (delete-package :test-impl))

;; TODO: There are a lot of outstanding tests!

(defpackage :test-impl
  (:use :cl :cl-getx :cl-naive-store.tests :cl-naive-store.naive-core))

(in-package :test-impl)

(cl-naive-tests:define-suite (:test-impl)
  (cl-naive-tests:testcase
   :plist-to-values
   :expected '(1 2)
   :actual (naive-impl:plist-to-values (list :test 1 :more 2)))
  (cl-naive-tests:testcase
   :plist-to-pairs
   :expected '((:test 1) (:more 2))
   :actual (naive-impl:plist-to-pairs (list :test 1 :more 2)))
  (cl-naive-tests:testcase
   :file-to-string
   :expected "Testing 1 2 3 ...
"
   :actual (let ((path
                   (cl-fad:merge-pathnames-as-file
                    (get-temp)
                    (make-pathname :directory '(:relative "naive-impl")
                                   :name "file"
                                   :type "string")))
                 (content "Testing 1 2 3 ..."))
             (ensure-directories-exist path)
             (with-open-file (stream path :direction :output :if-exists :supersede)
               (write-string content stream))

             (naive-impl:file-to-string path)))
  ;;TODO: Expand test to actually check if lock was enforced.
  (cl-naive-tests:testcase
   :with-file-lock
   :expected "lock-me.log"
   :actual (let* ((folder (cl-fad:merge-pathnames-as-file
                           (get-temp)
                           (make-pathname :directory '(:relative "naive-impl"))))
                  (path
                    (cl-fad:merge-pathnames-as-file
                     folder
                     (make-pathname :name "lock-me"
                                    :type "log")))
                  (lock (cl-fad:merge-pathnames-as-file
                         folder
                         (make-pathname :name "lock-me.log"
                                        :type "lock")))
                  (lock-found-p nil))

             (ensure-directories-exist path)

             (naive-impl:with-file-lock
                 (path)
               (setf lock-found-p
                     (probe-file lock)))

             (pathname-name lock-found-p)))
  ;;TODO: Expand test to check lock and write to file.
  (cl-naive-tests:testcase
   :with-open-file-lock
   :expected '("lock-me" "lock-me.log")
   :actual (let* ((folder (cl-fad:merge-pathnames-as-file
                           (get-temp)
                           (make-pathname :directory '(:relative "naive-impl"))))
                  (path
                    (cl-fad:merge-pathnames-as-file
                     folder
                     (make-pathname :name "lock-me"
                                    :type "log")))
                  (lock (cl-fad:merge-pathnames-as-file
                         folder
                         (make-pathname :name "lock-me.log"
                                        :type "lock")))
                  (lock-found-p nil))

             ;; (ensure-directories-exist path)
             (naive-impl:with-open-file-lock
                 (stream path :if-exists :supersede)
               (write "Testing 1 2 3 ..." :stream stream)
               (setf lock-found-p
                     (probe-file lock)))

             (list
              (pathname-name (probe-file path))
              (pathname-name lock))))
  (cl-naive-tests:testcase
   :compose-parse
   :expected '(:NAME "Piet" :SURNAME "Gieter" :ID 123 :WIFE
               (:NAME "Sannie" :SURNAME "Gieter" :ID 321) :PHOTO
               ("pic" "pic" "~/picture.pic"))
   :actual (let ((sexp
                   '(:NAME "Piet" :SURNAME "Gieter" :ID 123
                     :WIFE  (:NAME "Sannie" :SURNAME "Gieter" :ID 321)
                     :PHOTO (:blob% :location "~/picture.pic")))

                 (result))

             (setf result (naive-impl::compose-parse nil nil sexp nil))

             (list :name (getf result :name)
                   :surname (getf result :surname)
                   :id (getf result :id)
                   :wife  (getf result :wife)
                   :photo (list (blob-file-type (getf result :photo))
                                (blob-file-ext (getf result :photo))
                                (blob-location (getf result :photo)))))))

;;(cl-naive-tests:run :suites :test-impl)
