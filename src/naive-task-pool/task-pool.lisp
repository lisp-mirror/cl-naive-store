(in-package :cl-naive-task-pool)

(defclass task ()
  ((name :initarg :name
	 :accessor name
	 :initform nil
	 :documentation "Tasks are named so that they can be looked up by name.")
   (thunk :initarg :thunk
	  :accessor thunk
	  :initform nil
	  :documentation "The actual work that needs doing it should be a lambda with no parameters.")
   (status :initarg :status
	   :accessor status
	   :initform nil
	   :documentation "When a task is submitted the status is nil and when the scheduler gets hold of it the status changes to :started. Once the task is complete the status is changed to :completed.")   
   (result :initarg :result
	   :accessor result
	   :initform nil
	   :documentation "Set to :result if the task returns a result that should be retrievable.")   
   (task-type :initarg :task-type
	      :accessor task-type
	      :initform nil))
  (:documentation "Tasks are submitted to a pool and the pool schedules the task as threads become available. There is two types of tasks, tasks that return a value and tasks that dont. Tasks that dont return a value are removed from the pool when complete where as tasks that return a value are kept in the pool until the value is retrieved with task-result."))

(defclass task-pool ()
  ((thread-pool :initarg :thread-pool
		:accessor thread-pool
		:initform nil
		:documentation "Tasks are added to the thread-pool as there is space available and removed as they complete.")
   (thread-pool-size :initarg :thread-pool-size
		     :accessor thread-pool-size
		     :initform 1
		     :documentation "Resources are limited so adjust to cpu count or less.")   
   (tasks :initarg :tasks
	  :accessor tasks
	  :initform nil
	  :documentation "A list of the tasks submitted to the pool. Only tasks that are running or tasks that have a result and are completed will be found in this list.")
   (status :initarg :status
	   :accessor status
	   :initform :initialized
	   :documentation "When a task pool is started the status is set to :started. When a pool is stopped the status is set to :stopped. The status is used to startup and shut down pool management tasks."))
  (:documentation "A simple task pool that accepts tasks and schedules tasks in a thread pool. The size of the thread pool should be supplied according to the cpu count or less."))

(defparameter *tasks-lock* (bt:make-recursive-lock))
(defparameter *task-pool-lock* (bt:make-lock))

(defun schedule-tasks (task-pool)
  "Continually checks availability in thread pool and schedules tasks when there is space in the pool."
  (loop
    (cond ((equalp (status task-pool) :stopped)
	   (return-from schedule-tasks :stopped))
	  (t
	   (sleep 0.001)
	   (when
	       (< (length (thread-pool task-pool))
		  (thread-pool-size task-pool))

	     
	     (dolist (task (tasks task-pool))
	       
	       (when  (not (status task))

		 

		 (when (< (length (thread-pool task-pool))
			  (thread-pool-size task-pool))
		   (bt:with-lock-held (*task-pool-lock*)
		     (push task (thread-pool task-pool)))

		   (bt:make-thread
		    (lambda ()
		      (bt:with-recursive-lock-held (*tasks-lock*)
			(setf (status task) :started))
		      
		      ;;(format nil "~A Task ~A started~%" (bt:current-thread) task)
		      (let ((result (list (name task) (funcall (thunk task)))))
			(bt:with-recursive-lock-held
			    (*tasks-lock*)

			  ;;(break "huh")
			  
			  (setf (status task) :completed)
			  ;;(format nil "~A Task ~A completed~%" (bt:current-thread) task)
			  (if (equalp (task-type task) :result)
			      (progn
				(setf (result task) result))	      
			      (setf (tasks task-pool)  (remove task (tasks task-pool)))))))
		    :name (format nil "~A" (name task)))))
	       return))))))


(defun manage-task-pool (task-pool)
  "Continually check thread pool and removes completed tasks from the pool."
  (loop
    (cond ((equalp (status task-pool) :stopped)
	   (return-from manage-task-pool :stopped))
	  (t	   
	   (sleep 0.001)
	   (dolist (task (thread-pool task-pool))
	     (when (equalp (status task) :completed)
	       	;;(break "fuck")
	       (bt:with-lock-held (*task-pool-lock*)
		 (setf (thread-pool task-pool)
		       (remove task (thread-pool task-pool))))))))))

(defun start-task-pool (task-pool)
  "Starts to different task pool managers. The one manages the scheduling of tasks to the thread pool and the other clears the thread pool of completed tasks. To make sure that these task managers are stopped correctly use stop-task-pool."
  (bt:with-lock-held (*task-pool-lock*)
    (setf (status task-pool) :started))  
  (bt:make-thread
   (lambda ()
     (manage-task-pool task-pool)))
  (bt:make-thread
   (lambda ()
     (schedule-tasks task-pool))))

(defun stop-task-pool (task-pool)
  "Stop task managers but tasks that have already been started will continue running until complete."
  (bt:with-lock-held (*task-pool-lock*)
    (setf (status task-pool) :stopped)))

(defun submit-task (task-pool thunk &key name result-p)
  "Sumbit a task to the pool. This does not immediately start the task in its own thread, the task will be added to the thread pool and started when space in the pool becomes available."
  (let* ((task-name (or name (random 100000)))
	 (task (make-instance 'task
			      :name task-name
			      :thunk thunk
			      :task-type (if result-p :result)
			      :status nil)))

    (bt:with-lock-held (*tasks-lock*)
      (when (or (not name)
		(not (find name (tasks task-pool) :key 'name :test 'equalp )))
	(push task (tasks task-pool)))
      (when result-p
	    task))
    
    ))

(defun get-task-result (task-pool task tries wait)
  (loop
    (dotimes (i tries)
      (when (equalp (status task) :completed)
        (bt:with-recursive-lock-held (*tasks-lock*)
	    (setf (tasks task-pool) (remove task (tasks task-pool))))
	(return-from get-task-result (list (result task) :task-completed))))
     (if wait (sleep wait))))

(defun task-result (task-pool task &optional (tries 100) (wait 0.001))
  "This function checks for a task result if the task is not completed it will block execution unitl such time as the task is complete or tries have been exhausted."
  (when task
    (if (stringp task)
	(progn	
	  (dolist (task-x (tasks task-pool))
	    (when (equal task
			 task-x)            
	      (return-from task-result (get-task-result task-pool task-x tries wait))))
	  (error "Task not found!"))
	(get-task-result task-pool task tries wait))))
