;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-lazy-eval
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :anaphora
        :iterate
        :defclass-star
        :closer-mop
        :cl-def
        :cl-syntax-sugar
        :cl-walker))
