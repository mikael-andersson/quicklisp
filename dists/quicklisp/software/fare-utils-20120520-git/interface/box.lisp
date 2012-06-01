;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

;;;; Interface

;;; A class for box objects themselves
(defclass box () ())

(defgeneric box-ref (box)
  (:documentation "open a box and return its contents"))

;;; An interface for boxes

;;; A box: you can make it, or get something out of it
(define-interface <box> (<interface>) ())

(defgeneric make-box (<box> generator &key &allow-other-keys)
  (:documentation "Make a box from a generator for the value inside the box"))

(defgeneric unbox (<box> box)
  (:documentation "Return the value inside the box"))


;;; Classy box: same, based on a class
(define-interface <classy-box> (<box> <classy>) ())

(defmethod make-box ((i <classy-box>) generator &rest keys &key &allow-other-keys)
  (apply 'instantiate i :generator generator keys))

(defmethod unbox ((i <classy-box>) box)
  (declare (ignorable i))
  (box-ref box))


;;;; Boxes that hold a value

(defclass value-box (box)
  ((value :initarg :value :reader box-value)))

(defmethod box-ref ((box value-box))
  (if (slot-boundp box 'value)
    (box-value box)
    (call-next-method)))

(defclass simple-value-box (value-box)
  ((value :initarg :generator)))

(defmethod box-ref ((box simple-value-box))
  (box-value box))

(define-interface <value-box> (<classy-box>)
  ((class :initform 'simple-value-box)))

;;;; Boxes that hold a computation

(defclass thunk-box (box)
  ((thunk :initarg :thunk :reader box-thunk)))

(defclass simple-thunk-box (box)
  ((thunk :initarg :generator)))

(defmethod box-ref ((box simple-thunk-box))
  (funcall (box-thunk box)))

(define-interface <thunk-box> (<classy-box>)
  ((class :initform 'simple-thunk-box)))


;;;; Boxes that hold a promise

(defclass promise-box (value-box simple-thunk-box immutable-box) ())

(define-interface <promise-box> (<value-box> <simple-thunk-box>)
  ((class :initform 'promise-box)))

(defmacro delay (&body body)
  `(make-instance 'promise-box :thunk #'(lambda () ,@body)))

(defun force (promise)
  (box-ref promise))


;;;; Boxes that can only be used once
(defclass one-use-box (box)
  ((usedp :type boolean :initform nil :accessor box-usedp)))

(define-interface <one-use-box> (<classy-box>)
  ((class :initform 'one-use-box)))

(defmethod box-ref :before ((box one-use-box))
  (when (box-usedp box)
    (error "Tried to use ~A more than once" box)))

(defmethod box-ref :after ((box one-use-box))
  (setf (box-usedp box) t))

;;; Some concrete classes following that pattern.
(defclass one-use-value-box (one-use-box value-box) ())
(define-interface <one-use-value-box> (<one-use-box> <value-box>)
  ((class :initform 'one-use-value-box)))

(defclass one-use-thunk-box (one-use-box thunk-box) ())
(define-interface <one-use-thunk-box> (<one-use-box> <thunk-box>)
  ((class :initform 'one-use-thunk-box)))

(defun make-one-use-function (function &optional name)
  (let ((usedp t))
    (lambda (&rest args)
      (cond
        ((not usedp)
         (let ((fun function))
           (setf usedp t function nil)
           (apply fun args)))
        (t
         (error "Function ~@[~A ~]already called once" name))))))

(defmacro one-use-lambda (formals &body body)
  `(make-one-use-function #'(lambda ,formals ,@body)))


;;; Some boxes can be empty
(define-interface <emptyable-box> (<box>) ())

(defgeneric empty (<emptyable-box>)
  (:documentation "Return an empty box"))

(defgeneric empty-p (<emptyable-box> box)
  (:documentation "Return a boolean indicating whether the box was empty"))

;;; Some boxes can be refilled

(defclass mutable-box (box) ())
(defclass immutable-box (box) ())

(define-interface <mutable-box> (<box>) ())

(defgeneric box-set! (box value)
  (:documentation "set the contents of a box (if applicable)"))

(defmethod box-set! ((box immutable-box) value)
  (error "Trying to set an immutable box"))

(defgeneric set-box! (<box> box value))

(defmethod set-box! ((i <classy-box>) box value)
  (declare (ignorable i))
  (box-set! box value))

(defclass box! (mutable-box emptyable-box value-box) ())

(define-interface <box!> (<mutable-box> <classy-box> <emptyable-box>)
  ((class :initform 'box!)))

(defmethod box-set! ((box box!) value)
  (setf (slot-value box 'value) value))

(defmethod empty-p ((i <box!>) box)
  (declare (ignorable i))
  (slot-boundp box 'value))

(defmethod empty ((i <box!>))
  (declare (ignorable i))
  (make-instance 'box!))
