;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.lazy-eval.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.lazy-eval
               :hu.dwim.stefil+hu.dwim.def)
  :components ((:module "test"
                :components ((:file "combinator" :depends-on ("suite"))
                             (:file "cons" :depends-on ("suite"))
                             (:file "fringe" :depends-on ("list"))
                             (:file "list" :depends-on ("cons"))
                             (:file "package")
                             (:file "prime" :depends-on ("list"))
                             (:file "quicksort" :depends-on ("list"))
                             (:file "suite" :depends-on ("package"))))))
