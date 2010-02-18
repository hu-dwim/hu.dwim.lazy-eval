;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; fringe

(def lazy-function stream-equal (left right)
  (if (and (consp left)
           (consp right))
      (cons (eql (car left) (car right))
            (stream-equal (cdr left) (cdr right)))
      (eql left right)))

(def lazy-function same-fringe (left right)
  (always (stream-equal (flatten left) (flatten right))))

(def test test/fringe ()
  (is (same-fringe '(1 (2 3 (4 5) 6) 7 8) '((1 (2 3 4) 5) 6 (7 8))))
  (is (with-lazy-eval (always (take (stream-equal (repeat 1) (repeat 1)) 100))))
  (is (with-lazy-eval (always (take (stream-equal (repeat 1) (every-nth (repeat 1) 5)) 100))))
  (is (with-lazy-eval (always (same-fringe (random-tree) (random-tree))))))
