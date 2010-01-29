;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :lfgalign)

;;; Giza++ gives us the LPT-correspondence (criterion i), our Prolog
;;; files should have the information to use criterion ii.
