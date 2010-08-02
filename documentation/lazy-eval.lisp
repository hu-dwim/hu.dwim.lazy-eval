;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.lazy-eval.documentation)

(def project :hu.dwim.lazy-eval)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))
