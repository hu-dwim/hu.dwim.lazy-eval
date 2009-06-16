;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-lazy-eval)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun transform-function-definer-options (options)
  (if cl-lazy-eval-system:*load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '("CL-LAZY-EVAL" "CL-LAZY-EVAL-TEST") 'setup-readtable)
