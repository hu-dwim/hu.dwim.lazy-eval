;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-lazy-eval)

(defpackage :cl-lazy-eval-test
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :cl-def
        :cl-syntax-sugar
        :cl-lazy-eval))
