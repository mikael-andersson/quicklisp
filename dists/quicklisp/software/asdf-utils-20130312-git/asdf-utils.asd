;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; ASDF-Utils, transitional package to asdf-driver.
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2010 - 2012, Francois-Rene Rideau
;;;

(defsystem :asdf-utils
  :licence "MIT"
  :description "Utilities from ASDF, repackaged"
  :long-description "A copy of some utilities initially developed as part of ASDF"
  :depends-on (#-asdf3 "asdf-driver"))
