;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :lfgalign)

;;; Giza++ gives us the LPT-correspondence (criterion i), our Prolog
;;; files should have the information to use criterion ii.

(defparameter f-parse
  nil
  "The f-parse of the Prolog file.")
(defparameter c-parse
  nil
  "The c-parse of the Prolog file.")


(with-open-file
    (stream (merge-pathnames "dev/TEST_parse.pl"
			     (asdf:component-pathname (asdf:find-system :lfgalign))))
  (cdr (parse-pred stream)))