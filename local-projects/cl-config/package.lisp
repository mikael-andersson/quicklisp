
(defpackage #:cl-config
  (:use :cl)
  (:documentation
    "Simple configuration library")
  (:export #:get-value #:set-value))

(in-package :cl-config)

(defvar *config-values* nil)

(defclass config-callback-value ()
  ((function :initarg :function)))

(defun set-value (key value &key functionp)
  (setf (getf *config-values* key) 
        (if functionp 
          (progn 
            (assert (functionp value))
            (make-instance 'config-callback-value :function value))
          value)))

(defun get-value (key &rest context)
  (let ((default-value (and 
                         (evenp (length context))
                         (find :default context)
                         (getf context :default)))
        (value (getf *config-values* key)))
    (cond 
      ((functionp value)
       (apply value context))
      ((equal 'config-callback-value 
              (type-of value))
       (slot-value value 'function))
      (t (or value default-value)))))

(defun test-sets-value ()
  (declare (special *config-values*))
  (set-value :test1 "Test 1")
  (assert (string= (getf *config-values* :test1) "Test 1")))

(defun test-sets-function-value ()
  (declare (special *config-values*))
  (set-value :test2 #'test-sets-function-value :functionp t)
  (assert (equal #'test-sets-function-value (get-value :test2))))

(defun test-sets-context-value () 
  (set-value :test3 
             (lambda (number)
               (if (= number 1)
                 "1"
                 "2")))
  (assert (string= (get-value :test3 1) "1"))
  (assert (string= (get-value :test3 2) "2")))

(defun test-returns-default-value ()
  (assert (= 1 (get-value :test4 :default 1))))

(defun test-returns-overrided-value-if-default-is-set ()
  (set-value :test5 2)
  (assert (= 2  (get-value :test5 :default 1))))

(defun test-all ()
  (declare (special *config-values*))
  (test-sets-value)
  (test-sets-function-value)
  (test-sets-context-value)
  (test-returns-default-value)
  (test-returns-overrided-value-if-default-is-set)
  (setf *config-values* nil))

(test-all)
