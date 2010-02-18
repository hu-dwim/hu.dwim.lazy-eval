;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; Primes

(def lazy-function integers-from (first)
  (cons first (integers-from (1+ first))))

(def lazy-function strike-off (list divisor)
  (if (zerop (mod (car list) divisor))
      (strike-off (cdr list) divisor)
      (cons (car list) (strike-off (cdr list) divisor))))

(def lazy-function sieve (list)
  (cons (car list)
        (sieve (strike-off list (car list)))))

(def lazy-function primes ()
  (sieve (integers-from 2)))

(def test test/primes ()
  (is (equal (with-lazy-eval (take (integers-from 0) 10))
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (with-lazy-eval (take (strike-off (integers-from 0) 2) 10))
             '(1 3 5 7 9 11 13 15 17 19)))
  (is (equal (with-lazy-eval (take (primes) 10))
             '(2 3 5 7 11 13 17 19 23 29))))
