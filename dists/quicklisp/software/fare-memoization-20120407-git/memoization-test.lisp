#+xcvb (module (:depends-on ("memoization" (:asdf "hu.dwim.stefil"))))

(defpackage :fare-memoization-test (:use :cl :fare-memoization :hu.dwim.stefil))

(in-package :fare-memoization-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-memoization
            :in root-suite
            :documentation "Testing memoization"))

(defparameter *counter* 0)
(declaim (notinline foo))
(defun foo (x) (list x (incf *counter*)))
(define-memo-function bar (x) (foo x))
(defparameter *baz* (memoizing #'foo))
(defun baz (x) (funcall *baz* x))
(defun quux (x) (memoized-funcall 'foo x))
(defun quuux (x) (memoized-apply 'foo (list x)))

(defun init-memo-test ()
  (setf *counter* 0)
  (clrhash *memoized*)
  (memoize 'bar)
  (setf *baz* (memoizing #'foo))
  (values))

(deftest memo-test ()
  (init-memo-test)
  (memoize 'foo)
  (is (equal (foo :a) '(:a 1)))
  (is (equal (foo :b) '(:b 2)))
  (is (equal (foo :a) '(:a 1)))
  (is (equal (foo :c) '(:c 3)))
  (is (equal (bar :a) '(:a 1)))
  (is (equal (baz :a) '(:a 4)))
  (is (equal (quux :a) '(:a 1)))
  (is (equal (quuux :a) '(:a 1)))
  (memoize 'foo)
  (is (equal (foo :c) '(:c 5)))
  (is (equal (foo :b) '(:b 6)))
  (is (equal (foo :a) '(:a 7)))
  (is (equal (foo :c) '(:c 5)))
  (is (equal (bar :a) '(:a 1)))
  (is (equal (baz :a) '(:a 4)))
  (is (equal (quux :a) '(:a 1)))
  (is (equal (quuux :a) '(:a 1)))
  (unmemoize 'foo)
  (is (equal (foo :a) '(:a 8)))
  (is (equal (foo :a) '(:a 9)))
  (is (equal (bar :a) '(:a 1)))
  (is (equal (bar :b) '(:b 10)))
  (is (equal (baz :a) '(:a 4)))
  (is (equal (baz :b) '(:b 11)))
  (is (equal (quux :a) '(:a 1)))
  (is (equal (quux :b) '(:b 12)))
  (is (equal (quuux :a) '(:a 1)))
  (is (equal (quuux :b) '(:b 12)))
  (is (eq (memoized-funcall 'cons 1 2) (memoized-funcall 'cons 1 2))))
