;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-config-asd
  (:use :cl :asdf))

(in-package :cl-config-asd)

(defsystem cl-config
   :name "Cl config"
   :version "0.1"
   :author "Olexiy Zamkoviy"
   :licence "Public Domain"
   :description "Simple configuration library"
   :depends-on ()
   :components ((:file "package")))

