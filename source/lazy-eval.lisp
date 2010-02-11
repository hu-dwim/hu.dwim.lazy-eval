;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval)

;;;;;;
;;; API
;;;
;;; Values:
;;;  - a delayed computation is a closure that must be forced to get the actual strict value behind
;;;  - a delayed computation is only evaluated once, thus the strict value behind is remembered
;;;  - a strict value is any value that is not a delayed computation
;;;  - a lazy value is either a strict value or a delayed computation
;;;
;;; Special forms:
;;;  - delay takes a list of forms and creates a delayed computation that will evaluate the forms as an implicit progn
;;;  - force takes a form that evaluates to a lazy value and returns a strict value
;;;
;;; Types of function argument values and return values:
;;;  - a lazy function takes lazy argument values, and returns lazy return values, the function name ends with /lazy
;;;  - a strict function takes strict argument values, and returns strict return values
;;;
;;; Defining functions:
;;;  - a lazy function must be defined by the special definer (DEF LAZY-FUNCTION ...)
;;;  - a strict function can be defined by the standard function definition forms
;;;
;;; Calling a function:
;;;  - when a strict function calls a strict function (already provided by the VM)
;;;    it passes argument values untouched, and takes return values untouched
;;;  - when a strict function calls a lazy function (a separate function is created for this without suffix)
;;;    it passes argument values untouched, and forces return values
;;;  - when a lazy function calls a lazy function (lazy function names end with /lazy)
;;;    it passes argument values untouched, and takes return values untouched
;;;  - when a lazy function calls a strict function
;;;    it forces argument values unless the function cares about the argument's identity, and takes return values untouched
;;;  - when a lazy function calls itself recursively
;;;    it passes argument values untouched, and delays return values

;;;;;;
;;; Delay

(def (macro e) delay (&body forms)
  "strict -> lazy"
  (bind ((variable (gensym))
         (unbound-value (gensym)))
    `(bind ((,variable ',unbound-value))
       (lambda ()
         (if (eq ,variable ',unbound-value)
             (setf ,variable (progn ,@forms))
             ,variable)))))

;;;;;;
;;; Force

(def (function e) force (value)
  "lazy -> strict"
  (if (functionp value)
      (force (funcall value))
      value))

(def (generic e) force-recursively (value)
  (:documentation "lazy -> strict recursively")

  (:method (value)
    value)

  (:method ((value function))
    (force-recursively (funcall value)))

  (:method ((value cons))
    (cons (force-recursively (car value))
          (force-recursively (cdr value)))))

;;;;;;
;;; Definer

(def special-variable *lazy-function-name*)

(def (namespace e) lazy-function (args &body forms)
  (bind ((lazy-function-name (lazy-function-name -name-)))
    `(progn
       (def function ,-name- ,args
         (force (,lazy-function-name ,@args)))
       (def function ,lazy-function-name ,args
         ,@(bind ((*lazy-function-name* -name-))
             (with-active-layers (lazy-eval)
               (mapcar 'unwalk-form (body-of (walk-form `(lambda ,args ,@forms)))))))
       (fdefinition ',-name-))))

(def function lazy-function-name (name)
  (bind ((package (symbol-package name)))
    (format-symbol (if (eq package (find-package :common-lisp))
                       (find-package :hu.dwim.lazy-eval)
                       package)
                   "~A/LAZY" name)))

;;;;;;
;;; Walk/unwalk

(def layer lazy-eval ()
  ())

(def (function e) lazy (value)
  "Marker to pass value lazily instead of strict"
  value)

(def layered-method unwalk-form :in lazy-eval ((form application-form))
  (bind ((operator (operator-of form)))
    (cond ((or (find-lazy-function operator :otherwise nil)
               (eq operator *lazy-function-name*))
           (bind ((result `(,(lazy-function-name operator) ,@(mapcar 'unwalk-form (arguments-of form)))))
             (if (eq operator *lazy-function-name*)
                 `(delay ,result)
                 result)))
          (t
           `(,operator ,@(mapcar (lambda (argument)
                                   (if (and (typep argument 'free-application-form)
                                            (eq 'lazy (operator-of argument)))
                                       (unwalk-form (first (arguments-of argument)))
                                       `(force ,(unwalk-form argument))))
                                 (arguments-of form)))))))

(def layered-method unwalk-form :in lazy-eval ((form if-form))
     (call-next-method))
