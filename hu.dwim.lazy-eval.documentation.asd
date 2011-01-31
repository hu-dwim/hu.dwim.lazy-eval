;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.lazy-eval.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.lazy-eval.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "lazy-eval" :depends-on ("package"))
                             (:file "package")))))
