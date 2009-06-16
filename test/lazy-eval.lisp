;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-lazy-eval-test)

(def lazy-function cons (a b)
  (!cons a b))

(def lazy-function car (list)
  (!car list))

(def lazy-function cdr (list)
  (!cdr list))

(def lazy-function list (&rest args)
  (apply '!list args))

(def lazy-function mapcar (function list &rest lists)
  (if list
      (cons (apply function
                   (car list)
                   (!mapcar 'car lists))
            (apply 'mapcar function
                   (cdr list)
                   (!mapcar 'cdr lists)))))

(def lazy-function integer-list (i)
  (cons i (integer-list (+ i 1))))

(def lazy-function take (list n)
  ;; for simpliciy it returns the reverse
  (let ((result nil))
    (tagbody
     loop
       (if (= n 0)
           (go end)
           (setq n (- n 1)))
       (setq result (cons (car list) result))
       (setq list (cdr list))
       (go loop)
     end)
    result))

(def lazy-function + (&rest args)
  (apply '+ args))

(def lazy-function double (a)
  (* 2 a))

(def lazy-function double-list (list)
  (mapcar 'double list))

;;;;;
;;; Examples

(def function cons/lazy (a b)
  (delay (cons a b)))

(def function car/lazy (list)
  (delay (car (force list))))

(def function cdr/lazy (list)
  (delay (cdr (force list))))

(def test test-cons ()
  (is (equal (force* (cons/lazy 1 2))
             (cons 1 2)))
  (is (equal (force* (car/lazy (cons/lazy 1 2)))
             1))
  (is (equal (force* (cdr/lazy (cons/lazy 1 2)))
             2)))

(def function list/lazy (&rest args)
  (delay (apply #'list args)))

(def test test-list ()
  (is (equal (force* (list/lazy 1 2 3))
             (list 1 2 3))))

(def test test-list-and-cons ()
  (is (equal (force* (list/lazy (cons/lazy 'a 'b) 1 2 3))
             (list '(a . b) 1 2 3))))

(def function integer-list (i)
  (cons i (integer-list (+ i 1))))

(def function integer-list/lazy (i)
  (cons/lazy i (delay (force (integer-list/lazy (+ 1 (force i)))))))

(def function take (list n)
  ;; for simpliciy it returns the reverse
  (let ((result nil))
    (tagbody
     loop
       (if (= n 0)
           (go end)
           (setq n (- n 1)))
       (setq result (cons (car list) result))
       (setq list (cdr list))
       (go loop)
     end)
    result))

(def function take/lazy (list n)
  ;; for simpliciy it returns the reverse
  (let ((result nil))
    (tagbody
     loop
       (if (= (force n) 0)
           (go end)
           (setq n (- (force n) 1)))
       (setq result (cons/lazy (car/lazy list) result))
       (setq list (cdr/lazy list))
       (go loop)
     end)
    result))

;; optimized
(def function take/lazy (list n)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n)
           (type list list))
  ;; for simpliciy it returns the reverse
  (let ((result nil))
    (tagbody
     loop
       (if (= n 0)
           (go end)
           (setq n (- n 1)))
       (setq result (cons (car list) result))
       (setq list (force (cdr list)))
       (go loop)
     end)
    result))

(def test test-integer-list ()
  (is (equal (force* (take/lazy (integer-list/lazy 1) 3))
             (list 3 2 1))))

#+nil
(def function mapcar (function list &rest lists)
  (if list
      (cons (apply function
                   (car list)
                   (mapcar #'car lists))
            (apply #'mapcar function
                   (cdr list)
                   (mapcar #'cdr lists)))))

(def function mapcar/lazy (function list &rest lists)
  (if (force list)
      (cons/lazy (apply function
                        (force (car/lazy list))
                        (force (mapcar #'car/lazy lists)))
                 (delay (force (apply #'mapcar/lazy function
                                      (cdr/lazy list)
                                      (mapcar #'cdr/lazy lists)))))))

(def test test-mapcar ()
  (is (equal (force* (mapcar/lazy #'double (list/lazy 1 2 3)))
             '(2 4 6))))

(def function +/lazy (&rest args)
  (apply #'+ (mapcar #'force args)))

(def function double (a)
  (* 2 a))

(def function double/lazy (a)
  (* 2 (force a)))

(def function double-list (list)
  (mapcar #'double list))

(def function double-list/lazy (list)
  (mapcar/lazy #'double/lazy list))

(def test test-double-integer-list ()
  (is (equal (force* (take/lazy (double-list/lazy (integer-list/lazy 1)) 3))
             (list 6 4 2))))
