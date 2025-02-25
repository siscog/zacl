;;;; portable-process.lisp

(in-package #:zacl)

(defvar *current-process* nil)
(defvar *all-processes* nil)
(defvar *process-list-lock* (make-lock "process list lock"))

(defgeneric name (process))
(defgeneric (setf name) (new-value process))

(defgeneric add-run-reason (process object))
(defgeneric revoke-run-reason (process object))
(defgeneric reasons-condition-variable (process))
(defgeneric reasons-lock (process))

(defgeneric killedp (process))
(defgeneric wait-until-runnable (process))

(defgeneric enable (process))
(defgeneric kill (process))
(defgeneric preset (process fun &rest args))
(defgeneric yield (process))

(defgeneric state (process))
(defgeneric (setf state) (new-value process))

(defgeneric initial-function (process))
(defgeneric (setf initial-function) (new-value process))
(defgeneric initial-arguments (process))
(defgeneric (setf initial-arguments) (new-value process))

(defun add-to-all-processes (process)
  (with-lock-held (*process-list-lock*)
    (push process *all-processes*)))

(defun remove-from-all-processes (process)
  (with-lock-held (*process-list-lock*)
    (setf *all-processes* (delete process *all-processes*))))

(defun make-process-function-wrapper (process)
  (lambda ()
    (unwind-protect
         (let ((*current-process* process))
           (loop
             (catch 'killed
               (when (killedp process)
                 (return :killed))
               (setf (state process) :reset)
               (wait-until-runnable process)
               (setf (state process) :running)
               (apply (initial-function process) (initial-arguments process))
               (unless (reset-action process)
                 (return (setf (state process) :finished))))))
      (remove-from-all-processes process))))

(defclass process ()
  ((initial-function
    :initarg :initial-function
    :initform nil
    :accessor initial-function
    :reader initial-function)
   (reset-action
    :initform t
    :initarg :reset-action
    :accessor reset-action)
   (initial-arguments
    :initarg :initial-arguments
    :initform nil
    :accessor initial-arguments)
   (initial-bindings
    :initarg :initial-bindings
    :initform nil
    :accessor initial-bindings)
   (state
    :initform :initial
    :accessor state)
   (run-reasons
    :accessor run-reasons
    :initform nil)
   (arrest-reasons
    :accessor arrest-reasons
    :initform nil)
   (thread
    :initform nil
    :initarg :thread
    :accessor thread)
   (name
    :initarg :name
    :reader name
    :reader process-name
    :initform "anonymous")
   (reasons-lock
    :initform (make-lock "*-reasons lock")
    :reader reasons-lock)
   (reasons-condition-variable
    :initform (make-condition-variable :name "*-reasons condition variable")
    :reader reasons-condition-variable)
   (lock
    :initform (make-lock "process lock")
    :reader lock)
   (property-list
    :initform nil
    :reader property-list
    :accessor mp:process-property-list)))

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t :identity t)
    (format stream "~S ~S"
            (name process)
            (state process))))

(defun start (process)
  (unless (initial-function process)
    (error "No function initialized for ~A" process))
  (setf (thread process)
        (make-thread (make-process-function-wrapper process)
                     :name (name process)
                     :initial-bindings (initial-bindings process))))

(defun maybe-start (process)
  (unless (thread process)
    (let ((fun (initial-function process)))
      (when fun
        (start process)))))

(defmethod enable ((process process))
  (push :enable (run-reasons process))
  (maybe-start process))

(defmethod killedp ((process process))
  (eql (state process) :killed))

(defmethod kill ((process process))
  (setf (state process) :killed)
  (when (thread process)
    (interrupt-thread (thread process)
                      (lambda () (throw 'killed nil))))
  (remove-from-all-processes process)
  process)

(defmethod preset ((process process) fun &rest args)
  (setf (initial-function process) fun)
  (setf (initial-arguments process) args)
  (maybe-start process)
  fun)

(defmethod initialize-instance :after ((process process) &key &allow-other-keys)
  (maybe-start process))

(defmethod wait-until-runnable ((process process))
  (loop
    (cond ((run-reasons process)
           (return))
          (t
           (let ((lock (reasons-lock process)))
             (acquire-lock lock)
             (condition-wait (reasons-condition-variable process) lock)
             (release-lock lock))
           (when (run-reasons process)
             (return))))))

(defmethod add-run-reason ((process process) object)
  (with-lock-held ((reasons-lock process))
    (push object (run-reasons process)))
  (condition-notify (reasons-condition-variable process)))

(defmethod revoke-run-reason ((process process) object)
  (with-lock-held ((reasons-lock process))
    (setf (run-reasons process)
          (delete object (run-reasons process)))))

(defun make-process (name &key (class 'process) initial-bindings)
  (let ((process (make-instance class :initial-bindings initial-bindings
                                :name name)))
    (add-to-all-processes process)
    process))

(defun %make-process (&key name (class 'process) initial-bindings)
  (make-process name :class class :initial-bindings initial-bindings))

(defmethod yield ((process null))
  ;; *current-process* might be nil if YIELD is not run from a
  ;; portable process. Don't error.
  nil)

(defmethod yield ((process process))
  (unless (eq process *current-process*)
    (error "Can't yield other processes"))
  (thread-yield))

(defun process-run-function (name-or-keywords function &rest arguments)
  (let* ((plist (if (consp name-or-keywords)
                    (copy-list name-or-keywords)
                    (list :name name-or-keywords)))
         (process (apply #'%make-process plist)))
    (push :enable (run-reasons process))
    (apply #'preset process function arguments)
    (setf (reset-action process) nil)
    process))

(defun current-process ()
  *current-process*)
