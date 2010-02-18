;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.lazy-eval.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.lazy-eval
               :hu.dwim.stefil+hu.dwim.def)
  :components ((:module "test"
                :components ((:file "cons" :depends-on ("suite"))
                             (:file "fringe" :depends-on ("list"))
                             (:file "list" :depends-on ("cons"))
                             (:file "package")
                             (:file "prime" :depends-on ("list"))
                             (:file "suite" :depends-on ("package"))))))
