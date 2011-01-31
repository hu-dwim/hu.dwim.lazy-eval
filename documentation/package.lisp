;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.lazy-eval.documentation
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.lazy-eval
        :hu.dwim.presentation
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:shadowing-import-from :hu.dwim.lazy-eval
                          #:delay
                          #:force
                          #:force-recursively))
