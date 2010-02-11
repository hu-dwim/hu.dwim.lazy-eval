;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.lazy-eval.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.lazy-eval
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar)
  (:shadow #:cons
           #:car
           #:cdr)
  (:readtable-setup (enable-standard-hu.dwim-syntaxes)))
