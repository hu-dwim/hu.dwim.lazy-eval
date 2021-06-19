;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.lazy-eval
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Delimieted, compiled lazy evaluation."
  :depends-on (:hu.dwim.common
               :hu.dwim.def.namespace
               :hu.dwim.util
               :hu.dwim.defclass-star
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "lazy-eval" :depends-on ("package"))
                             (:file "package")))))

(defsystem :hu.dwim.lazy-eval/test
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

(defsystem :hu.dwim.lazy-eval/documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.lazy-eval/test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "lazy-eval" :depends-on ("package"))
                             (:file "package")))))
