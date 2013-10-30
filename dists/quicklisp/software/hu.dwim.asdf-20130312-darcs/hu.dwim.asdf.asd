;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009-2011 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.asdf
  :description "Various ASDF extensions such as attached test and documentation system, explicit development support, etc."
  ;; this dependency on asdf may be a source of headaches if a newer asdf is part of the registry
  ;; and due to this dependency it gets updated in the middle of a half loaded image... nevertheless, we depend on asdf.
  :depends-on (:asdf)
  :components ((:module "source"
                :components ((:file "duplicates" :depends-on ("package"))
                             (:file "package")
                             (:file "production" :depends-on ("workspace" "duplicates"))
                             (:file "system" :depends-on ("production"))
                             (:file "workspace" :depends-on ("package"))))))
