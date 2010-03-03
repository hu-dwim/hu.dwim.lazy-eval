;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; Y combinator

(def lazy-function y-combinator (f)
  (funcall f (lazy (y-combinator f))))

(def lazy-function factorial* (recurse)
  (lambda (n)
    (if (zerop n)
        1
        (* n (funcall recurse (- n 1))))))

(def lazy-function factorial (n)
  (funcall (y-combinator 'factorial*) n))
