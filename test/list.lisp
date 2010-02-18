;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.test)

;;;;;;
;;; fringe

(def lazy-function repeat (element)
  (cons element (repeat element)))

(def lazy-function random-tree ()
  (if (zerop (random 10))
      (random 10)
      (cons (random-tree)
            (random-tree))))

(def lazy-function equal (left right)
  (or (eql left right)
      (and (consp left)
           (consp right)
           (eql (car left) (car right))
           (equal (cdr left) (cdr right)))))

(def lazy-function append (left right)
  (if (consp left)
      (cons (car left) (append (cdr left) right))
      right))

(def lazy-function take (list count)
  (if (or (null list)
          (zerop count))
      nil
      (cons (car list)
            (take (cdr list) (1- count)))))

(def lazy-function every-nth (list n)
  (labels ((recurse (tail count)
             (if (zerop count)
                 (cons (car tail)
                       (every-nth (cdr tail) n))
                 (recurse (cdr tail) (1- count)))))
    (recurse list (1- n))))

(def lazy-function flatten (list)
  (cond ((null list)
         nil)
        ((consp list)
         (append (flatten (car list)) (flatten (cdr list))))
        (t
         (list list))))

(def lazy-function always (list)
  (if (consp list)
      (and (car list)
           (always (cdr list)))
      t))

(def lazy-function map (function list)
  (if (consp list)
      (cons (funcall function (car list))
            (map function (cdr list)))
      nil))

(def test test/list ()
  (is (equal '(1 2) '(1 2)))
  (is (always '(#t #t)))
  (is (not (always '(#t #f))))
  (is (equal (with-lazy-eval
               (take (labels ((recurse (e)
                                (append e (delay (recurse e)))))
                       (recurse (list 1))) 10))
             '(1 1 1 1 1 1 1 1 1 1)))
  (is (equal (with-lazy-eval (take (map '1+ (repeat 0)) 10))
             '(1 1 1 1 1 1 1 1 1 1))))
