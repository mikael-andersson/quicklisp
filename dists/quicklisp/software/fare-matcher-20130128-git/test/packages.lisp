#+xcvb (module ())

(in-package #:cl)

(defpackage #:fare-matcher-test
  (:use #:fare-matcher #:fare-quasiquote #:fare-utils #:common-lisp #:hu.dwim.stefil)
  (:shadowing-import-from #:fare-quasiquote
                          #:quote
                          #:list #:list* #:append #:cons #:nconc
                          #:unquote #:quasiquote
                          #:unquote-splicing #:unquote-nsplicing #:unquote-xsplicing-p)
  (:export))
