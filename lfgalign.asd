;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage #:lfgalign-asd
  (:use :cl :asdf))

(in-package :lfgalign-asd)

(defsystem lfgalign
  :name "lfgalign"
  :version "0.0.1"
  :maintainer "Kevin Brubeck Unhammer"
  :author "Kevin Brubeck Unhammer"
  :licence "GNU GPL 3.0"
  :description "lfgalign"
  :long-description "LFG alignment algorithm"
  :serial t ;; the dependencies are linear.
  :components ((:file "lfgalign")
               (:file "prolog-import")))
