;;; -*- Lisp -*-

(in-package :asdf)

(defsystem :fare-matcher
  :description "Lisp2-style Erlang/ML-like Extensible Pattern-Matcher for CL"
  :long-description "fare-matcher allows you to do pattern matching as in ML,
and to define your own extensions."
  :depends-on (:alexandria :fare-utils)
  :serial t
  :components
  ((:file "packages")
   (:file "matcher")
   ;;#-gcl ; 2.7.0-64.1 cannot defgeneric in a eval-now
   (:file "clos-match")
   (:file "mrd-extensions")))

(defmethod perform ((op test-op) (sys (eql (find-system :fare-matcher))))
  (format t "~&Testing fare-matcher")
  (load-system :fare-matcher-test)
  (funcall (find-symbol* :fare-matcher-test :fare-matcher-test)))
