;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; cons

(def lazy-function cons (car cdr)
  (common-lisp:cons (lazy car) (lazy cdr)))

(def lazy-function car (cell)
  (common-lisp:car cell))

(def lazy-function cdr (cell)
  (common-lisp:cdr cell))

(def test test/cons ()
  (is (= 1 (car (cons 1 2))))
  (is (= 2 (cdr (cons 1 2)))))
