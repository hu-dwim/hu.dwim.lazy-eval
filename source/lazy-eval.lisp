;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval)

;;;;;;
;;; Delay

;; TODO: move these to a utility or what? (wui has the same)
(def (macro e) delay (&body forms)
  "strict -> lazy"
  `(lambda ()
     ,@forms))

(def (macro e) delay* (&body forms)
  "strict -> lazy"
  (bind ((value (gensym)))
    `(bind ((,value :unbound))
       (lambda ()
         (if (eq ,value :unbound)
             (setf ,value (progn ,@forms))
             ,value)))))

(def (type e) lazy ()
  "lazy values are represented as functions"
  'function)

(def (function e) lazy? (value)
  (typep value 'function))

(def function lazy (value)
  (if (lazy? value)
      value
      (delay value)))

;;;;;;
;;; Force

(def (function e) force (value)
  "lazy -> strict"
  (if (functionp value)
      ;;(funcall value)
      (force (funcall value))
      value))

(def (generic e) force* (value)
  (:documentation "lazy -> strict recursively")

  (:method (value)
    value)

  (:method ((value function))
    (force* (funcall value)))

  (:method ((value cons))
    (cons (force* (car value))
          (force* (cdr value)))))

;;;;;;
;;; Walker

(def special-variable *lazy-functions* (make-hash-table))

(def special-variable *lazy-function-name*)

(def function lazy-function-name? (name)
  (gethash name *lazy-functions*))

(def function lazy-name (name)
  (bind ((package (symbol-package name)))
    (format-symbol (if (eq package (find-package :common-lisp))
                       (find-package :hu.dwim.lazy-eval)
                       package)
                   "~A/lazy" name)))

(def function bang-function-name? (name)
  (eq #\! (elt (symbol-name name) 0)))

(def function remove-bang-from-function-name (name)
  (if (bang-function-name? name)
      (intern (subseq (symbol-name name) 1) (symbol-package name))
      name))

(def macro with-lazy-eval (&body forms)
  `(force ,(lazy-unwalk-form (hu.dwim.walker:walk-form `(progn ,@forms)))))

(def generic lazy-unwalk-form (form)
  (:method ((form constant-form))
    (bind ((value (value-of form)))
      (typecase value
        (symbol (list 'quote value))
        (t value))))

  (:method ((form progn-form))
    `(progn ,@(mapcar 'lazy-unwalk-form (body-of form))))

  (:method ((form if-form))
    `(if (force ,(lazy-unwalk-form (condition-of form)))
         (force ,(lazy-unwalk-form (then-of form)))
         (force ,(lazy-unwalk-form (else-of form)))))

  (:method ((form free-variable-reference-form))
    (name-of form))

  (:method ((form lexical-variable-binding-form))
    ;; TODO:
    )

  (:method ((form application-form))
    (bind ((operator (operator-of form)))
      (cond ((lazy-function-name? operator)
             (bind ((result `(,(lazy-name operator) ,@(mapcar 'lazy-unwalk-form (arguments-of form)))))
               (if (eq operator *lazy-function-name*)
                   `(delay (force ,result))
                   result)))
            ((eq operator 'apply)
             `(apply ',(remove-bang-from-function-name (value-of (first (arguments-of form))))
                     ,@(mapcar (lambda (form)
                                 `(force ,(lazy-unwalk-form form)))
                               (cdr (arguments-of form)))))
            (t
             `(,(remove-bang-from-function-name operator)
                ,@(mapcar (lambda (form)
                            `(force ,(lazy-unwalk-form form)))
                          (arguments-of form))))))))

(def (definer e :available-flags "eoi") lazy-function (name args &body forms)
  (bind ((lazy-name (lazy-name name)))
    `(progn
       (setf (gethash ',name *lazy-functions*) ',lazy-name)
       (defun ,lazy-name ,args
         (delay ,(bind ((*lazy-function-name* name))
                       (lazy-unwalk-form (hu.dwim.walker:walk-form `(progn ,@forms)))))))))
