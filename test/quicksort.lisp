;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval/test)

;;;;;;
;;; quicksort

(def lazy-function partition (list pivot)
  (if (null list)
      nil
      (bind ((first (car list))
             (partitions (partition (cdr list) pivot)))
        (cond ((= first pivot)
               partitions)
              ((< first pivot)
               (cons (cons first (car partitions))
                     (cdr partitions)))
              ((> first pivot)
               (cons (car partitions)
                     (cons first (cdr partitions))))))))

(def lazy-function quicksort (list)
  (if (null list)
      nil
      (bind ((pivot (car list))
             (partitions (partition list pivot)))
        (append (quicksort (car partitions))
                (cons pivot (quicksort (cdr partitions)))))))

(def test test/quicksort ()
  (is (equal (quicksort '(162 11 6 9 1260 27 4 1 22 0 52 927 402 13 42 38 2 5 9238 752 98 7 54 23))
             '(0 1 2 4 5 6 7 9 11 13 22 23 27 38 42 52 54 98 162 402 752 927 1260 9238)))
  (is (equal (with-lazy-eval (take (quicksort '(162 11 6 9 1260 27 4 1 22 0 52 927 402 13 42 38 2 5 9238 752 98 7 54 23)) 5))
             '(0 1 2 4 5))))
