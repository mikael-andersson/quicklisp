;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mp::process)

;;; Thread Creation

(defun start-multiprocessing ()
  (mp::startup-idle-and-top-level-loops))

(defun %make-thread (function name)
  (mp:make-process function :name name))

(defun current-thread ()
  mp:*current-process*)

(defmethod threadp (object)
  (mp:processp object))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mp:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (if wait-p
      (mp::lock-wait lock "Lock")
      (mp::lock-wait-with-timeout lock "Lock" 0)))

(defun release-lock (lock)
  (setf (mp::lock-process lock) nil))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock-held (,place) ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-lock-held (,place "Lock Wait" :timeout ,timeout) ,@body))

;;; Note that the locks _are_ recursive, but not "balanced", and only
;;; checked if they are being held by the same process by with-lock-held.
;;; The default with-lock-held in bordeaux-mp.lisp sort of works, in that
;;; it will wait for recursive locks by the same process as well.

;;; Resource contention: condition variables

;;; There's some stuff in x86-vm.lisp that might be worth investigating
;;; whether to build on. There's also process-wait and friends.

(defstruct condition-var
  "CMUCL doesn't have conditions, so we need to create our own type."
  name
  lock
  active)

(defun make-condition-variable (&key name)
  (make-condition-var :lock (make-lock)
                      :name (or name "Anonymous condition variable")))

(defun condition-wait (condition-variable lock)
  (check-type condition-variable condition-var)
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) nil))
  (release-lock lock)
  (mp:process-wait "Condition Wait"
                   #'(lambda () (condition-var-active condition-variable)))
  (acquire-lock lock)
  t)

(defun condition-notify (condition-variable)
  (check-type condition-variable condition-var)
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) t))
  (thread-yield))

(defun thread-yield ()
  (mp:process-yield))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  (once-only (timeout)
    `(mp:with-timeout (,timeout (error 'timeout :length ,timeout))
       ,@body)))

;;; Introspection/debugging

(defun all-threads ()
  (mp:all-processes))

(defun interrupt-thread (thread function &rest args)
  (flet ((apply-function ()
           (if args
               (lambda () (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (mp:process-interrupt thread (apply-function))))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:destroy-process thread))

(defun thread-alive-p (thread)
  (mp:process-active-p thread))

(defun join-thread (thread)
  (mp:process-wait (format nil "Waiting for thread ~A to complete" thread)
                   (lambda () (not (mp:process-alive-p thread)))))

(mark-supported)
