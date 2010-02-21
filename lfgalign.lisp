;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

;;; Giza++ gives us the LPT-correspondence (criterion i), our Prolog
;;; files should have the information to use criterion ii.

(defparameter f-parse
  nil
  "The f-parse of the Prolog file.")
(defparameter c-parse
  nil
  "The c-parse of the Prolog file.")

(progn
  (defun unravel (attval tab)
    (cons (car attval)
	  (if (listp (cdr attval))
	      (cdr attval)
	      (gethash (cdr attval) tab))))
  
  (defun get-pred (var tab)
    (let ((val (assoc '|'PRED'| (gethash var tab))))
      (format t "~A~%" val)
      (unravel val tab)
	;; TODO: what if we don't find a pred?
      ))
  (defun get-children (pred)
    (fourth pred))

  (defun f-align (var1 tab1 var2 tab2)
    (let* ((pred1 (get-pred var1 tab1))
	   (pred2 (get-pred var2 tab2)))
      (format t "Align ~A with ~A~%" pred1 pred2)
      (when (and pred1 pred2)
	(loop
	   for child1 in (get-children pred1)
	   for child2 in (get-children pred2)
	   collect (f-align child1 tab1 child2 tab2)))))

  (defun open-and-import (file)
    (with-open-file
	(stream (merge-pathnames file
				 (asdf:component-pathname (asdf:find-system :lfgalign))))
      (import-f-table stream)))  
  (defun test ()
      ;; assume outermost f-str has var(0) and contains a PRED
    (f-align '|0| (open-and-import "ka-24.pl")
	     '|0| (open-and-import "nb-24.pl"))
    (format t "---~%")
    (f-align '|0| (open-and-import "ka-1.pl")
	     '|0| (open-and-import "nb-1.pl"))
    )
  
  
      ;; => ((|'bjeffe'| |10| (|'NULL'| |5|) NIL))

  (test))


