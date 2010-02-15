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
;;;  - a strict value is any value that is not a delayed computation, and does not refer to a delayed computation (recursively)
;;;  - a lazy value is either a strict value or a delayed computation or some
;;;
;;; Types of function argument values and return values:
;;;  - a lazy function takes lazy argument values, and returns lazy return values, the function name ends with /lazy by convention
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
;;;    it passes argument values untouched, and forces return values recursively
;;;  - when a lazy function calls a lazy function (lazy function names end with /lazy)
;;;    it passes argument values untouched, and takes return values untouched
;;;  - when a lazy function calls a strict function
;;;    it forces argument values recursively unless the argument is marked lazy, and takes return values untouched

;;;;;;
;;; Delay and Force

(def (macro e) delay (&body forms)
  "DELAY takes a list of FORMS and creates a delayed computation that will evaluate FORMS as an implicit progn when invoked. The result of DELAY is a lambda or closure (capturing the necessary environment) that must be given to FORCE to get the actual value behind. A delayed computation is only evaluated once, thus the computation result is remembered and reused on subsequent invokations."
  (bind ((variable (gensym))
         (unbound-value (gensym)))
    `(bind ((,variable ',unbound-value))
       (lambda ()
         (if (eq ,variable ',unbound-value)
             (setf ,variable (progn ,@forms))
             ,variable)))))

(def (function e) force (value)
  "FORCE takes a VALUE that may or may not be a delayed computation and returns a value that is definitely not a delayed computation."
  (if (functionp value)
      (force (funcall value))
      value))

(def (generic e) force-recursively (value)
  (:documentation "FORCE-RECURSIVELY takes a VALUE and returns a value that does not refer to any delayed computation through references.")

  (:method (value)
    value)

  (:method ((value function))
    (force-recursively (funcall value)))

  (:method ((value cons))
    (setf (car value) (force-recursively (car value))
          (cdr value) (force-recursively (cdr value)))
    value)

  (:method ((instance standard-object))
    (bind ((class (class-of instance)))
      (dolist (slot (class-slots class))
        (when (slot-boundp-using-class class instance slot)
          (setf (slot-value-using-class class instance slot)
                (force-recursively (slot-value-using-class class instance slot)))))
      instance)))

;;;;;;
;;; Definer

(def (special-variable e) *lazy-function-call-level* 0)

(def (special-variable e) *immediate-lazy-function-call-level-limit* 1)

(def (function e) lazy-function-name (name)
  "Returns a name based on NAME suitable for lazy function definition."
  (format-symbol (symbol-package name) "~A/LAZY" name))

(def (namespace e) lazy-function)

(def (macro e) with-lazy-eval (&body forms)
  "WITH-LAZY-EVAL evaluates FORMS using lazy evaluation semantics and returns a strict value."
  `(force-recursively
    ,@(with-active-layers (lazy-eval)
        (mapcar 'unwalk-form (body-of (walk-form `(progn ,@forms)))))))

(def (definer e) lazy-function (name args &body forms)
  "Defines a function called NAME with arguments ARGS and the body FORMS. The function is defined in both LAZY and STRICT forms."
  (bind ((lazy-function-name (lazy-function-name name))
         (lazy-forms (with-active-layers (lazy-eval)
                       (setf (find-lazy-function name) (lambda ()))
                       (mapcar 'unwalk-form (body-of (walk-form `(lambda ,args ,@forms)))))))
    `(progn
       (eval-when (:compile-toplevel)
         (setf (find-lazy-function ',name) (lambda ())))
       (def function ,name ,args
         (force-recursively (,lazy-function-name ,@args)))
       (def function ,lazy-function-name ,args
         (if (> *lazy-function-call-level* *immediate-lazy-function-call-level-limit*)
             (delay ,@lazy-forms)
             (bind ((*lazy-function-call-level* (1+ *lazy-function-call-level*)))
               ,@lazy-forms)))
       (fdefinition ',name))))

;;;;;;
;;; Walk/unwalk

(def layer lazy-eval ()
  ())

(def (function e) lazy (value)
  "LAZY is marker to signify the intent that VALUE must be passed in lazily instead of strictly to a strict function. This can only be done if the strict function does not look at the actual value (e.g. CONS)."
  value)

(def layered-method hu.dwim.walker::function-name? :in lazy-eval (name)
  (or (call-next-method)
      (find-lazy-function name :otherwise nil)))

(def layered-method unwalk-form :in lazy-eval ((form application-form))
  (bind ((operator (operator-of form)))
    (cond ((find-lazy-function operator :otherwise nil)
           `(,(lazy-function-name operator) ,@(mapcar 'unwalk-form (arguments-of form))))
          (t
           `(,operator ,@(mapcar (lambda (argument)
                                   (if (and (typep argument 'free-application-form)
                                            (eq 'lazy (operator-of argument)))
                                       (unwalk-form (first (arguments-of argument)))
                                       `(force ,(unwalk-form argument))))
                                 (arguments-of form)))))))
