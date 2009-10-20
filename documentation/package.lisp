;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.lazy-eval.documentation
  (:use :hu.dwim.asdf
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.lazy-eval
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui)

  (:shadowing-import-from :hu.dwim.lazy-eval
                          #:delay
                          #:delay*
                          #:force
                          #:force*))
