;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:lfgalign
  (:use #:cl))

(in-package #:lfgalign)

(asdf:defsystem #:lfgalign
  :name "lfgalign"
  :version "0.0.1"
  :maintainer "Kevin Brubeck Unhammer"
  :author "Kevin Brubeck Unhammer"
  :licence "GNU GPL 3.0"
  :description "lfgalign"
  :long-description "LFG alignment algorithm"
  :depends-on ("lisp-unit")		; but only for testing...hmm
  :components ((:file "cl-extra")
	       (:file "prolog-import"
		      :depends-on ("cl-extra"))
	       (:file "lfgalign"
		      :depends-on ("cl-extra")
		      :depends-on ("prolog-import"))
	       (:file "eval"
		      :depends-on ("lfgalign")
		      :depends-on ("prolog-import"))))
