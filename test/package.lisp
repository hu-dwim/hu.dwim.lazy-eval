;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.lazy-eval/test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.lazy-eval
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar)
  (:shadow #:null
           #:cons
           #:consp
           #:car
           #:cdr
           #:equal
           #:append
           #:map
           #:flatten
           #:factorial)
  (:readtable-setup (hu.dwim.util:enable-standard-hu.dwim-syntaxes)))
