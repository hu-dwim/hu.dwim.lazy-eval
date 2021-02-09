;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval/test)

;;;;;;
;;; cons

(def lazy-function null (candidate)
  (common-lisp:null candidate))

(def lazy-function cons (car cdr)
  (common-lisp:cons (lazily car) (lazily cdr)))

(def lazy-function consp (candidate)
  (common-lisp:consp candidate))

(def lazy-function car (cell)
  (common-lisp:car cell))

(def lazy-function cdr (cell)
  (common-lisp:cdr cell))

(def test test/cons ()
  (is (consp (cons 1 2)))
  (is (not (consp nil)))
  (is (null (car nil)))
  (is (= 1 (car (cons 1 2))))
  (is (= 1 (with-lazy-eval (car (cons 1 2)))))
  (is (null (cdr nil)))
  (is (= 2 (cdr (cons 1 2))))
  (is (= 2 (with-lazy-eval (cdr (cons 1 2))))))
