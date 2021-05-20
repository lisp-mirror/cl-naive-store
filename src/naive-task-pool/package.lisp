(defpackage :cl-naive-task-pool
  (:use :cl)
  (:export
   :task
   :thunk
   :status
   :result
   :task-type
   :task-pool
   :thread-pool
   :thread-pool-size
   :tasks
   :status

   :start-task-pool
   :stop-task-pool

   :submit-task
   :task-result
   ))
