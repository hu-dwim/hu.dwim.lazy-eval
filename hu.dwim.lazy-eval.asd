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
