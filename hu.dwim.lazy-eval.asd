;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.lazy-eval
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Delimieted, compiled lazy evaluation."
  :depends-on (:hu.dwim.common
               :hu.dwim.def
               :hu.dwim.util
               :hu.dwim.defclass-star
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "lazy-eval" :depends-on ("package"))
                             (:file "package")))))
