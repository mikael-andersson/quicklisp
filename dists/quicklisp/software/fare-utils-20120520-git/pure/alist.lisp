;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map implementation: alists.

#+xcvb
(module
 (:depends-on
  ("interface/interface"
   "interface/eq"
   "pure/package"
   "pure/map")))

(in-package :pure)

(defclass <alist>
    (<map>
     map-simple-empty map-simple-decons map-simple-update-key map-simple-divide/list
     map-simple-map/2 map-simple-join map-simple-join/list)
  ((eq-interface
    :initarg :eq
    :initform eq:<eq>
    :reader eq-interface)))

(defmethod check-invariant ((i <alist>) map &key)
  (loop :for ((key . nil) . rest) :on map :do
    (assert (not (member key rest
                         :key 'car
                         :test (eq:test-function (eq-interface i))))
            () "Key ~S is present twice in alist ~S" key map)))

(defun <alist> (&optional (eq eq:<eq>))
  (fmemo:memoized-funcall 'make-instance '<alist> :eq eq))

(defparameter <alist> (<alist>))

(defmethod lookup ((i <alist>) map key)
  (if (null map)
      (values nil nil)
      (let ((pair (assoc key map :test (eq:test-function (eq-interface i)))))
        (if pair
            (values (cdr pair) t)
            (values nil nil)))))

(defmethod insert ((i <alist>) map key value)
  (acons key value (drop i map key)))

(defmethod drop ((i <alist>) map key)
  (if (null map)
      (values nil nil nil)
      (multiple-value-bind (v f) (lookup i map key)
        (if f
            (values (remove key map :key 'car :test (eq:test-function (eq-interface i))) v t)
            (values map nil nil)))))

(defmethod first-key-value ((i <alist>) map)
  (values (caar map) (cdar map) (not (null map))))

(defmethod fold-left ((i <alist>) map f seed)
  (reduce #'(lambda (acc pair) (funcall f acc (car pair) (cdr pair)))
          map :initial-value seed))

(defmethod fold-right ((i <alist>) map f seed)
  (reduce #'(lambda (pair acc) (funcall f (car pair) (cdr pair) acc))
          map :initial-value seed :from-end t))

(defmethod for-each ((i <alist>) map f)
  (loop :for (key . val) :in map :do (funcall f key val))
  (values))

(defmethod divide ((i <alist>) map)
  (let* ((l (length map))
         (l1 (ceiling l 2)))
    (values (subseq map 0 l1) (nthcdr l1 map))))

(defmethod size ((i <alist>) map)
  (length map))
