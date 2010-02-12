;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; Primes

(def lazy-function integers-from (i)
  (cons i (integers-from (1+ i))))

(def lazy-function strike-off (i l)
  (if (zerop (mod (car l) i))
      (strike-off i (cdr l))
      (cons (car l) (strike-off i (cdr l)))))

(def lazy-function sieve (l)
  (cons (car l)
        (sieve (strike-off (car l) l))))

(def lazy-function take (l n)
  (if (zerop n)
      nil
      (cons (car l)
            (take (cdr l) (1- n)))))

(def lazy-function primes ()
  (sieve (integers-from 2)))

(def test test/primes ()
  (is (equal (with-lazy-eval (take (primes) 10)) '(2 3 5 7 11 13 17 19 23 29))))
