;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.lazy-eval.test
  :class hu.dwim.test-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.lazy-eval"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.lazy-eval)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "lazy-eval" :depends-on ("suite"))))))
