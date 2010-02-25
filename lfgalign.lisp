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

(defun unravel (attval tab)
  (cons (car attval)
	(if (listp (cdr attval))
	    (cdr attval)
	    (gethash (cdr attval) tab))))

(defun get-pred (var tab)
  (if (equal "NULL" var)
      (format t "NULL-pred TODO~%")
      (let ((predval (assoc "PRED" (gethash var tab) :test #'equal)))
	(unless predval (error 'no-pred-error-todo var))
	(unravel predval tab))))

(defun get-children (pred)
    (fourth pred))

(defun references (parentv childv tab)
  "Give a list of attributes of var `parentv' in `tab' which refer to
var `childv'."
  (loop
     for attval in (gethash parentv tab)
     when (eq childv (cdr attval))
     collect attval))

(defun topnode (f-var tab)
  (let ((c-ids
	 (mapcar #'car
		 (remove-if (lambda (phi) (not (eq (second phi) f-var)))
			    (gethash '|phi| tab))))
	
	topmost)
    (dolist (id c-ids)
      ;; (if (member id ))
      ))
  )

(progn
  (defun f-align (var1 tab1 var2 tab2)
    "`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively."
    (let* ((pred1 (get-pred var1 tab1))
	   (pred2 (get-pred var2 tab2)))
      (format t "Align ~A_~A with ~A_~A~%" var1 pred1 var2 pred2)
      (when (and pred1 pred2)
	(loop
	   for child1 in (get-children pred1)
	   for child2 in (get-children pred2)
	   do (format t "...aligning ~A_~A and ~A_~A...~%"
		      var1 (references var1 child1 tab1)
		      var2 (references var2 child2 tab2))
	   collect (f-align child1 tab1 child2 tab2)))))

  (defun open-and-import (file)
    (with-open-file
	(stream (merge-pathnames file
				 (asdf:component-pathname (asdf:find-system :lfgalign))))
      (import-table stream)))  
  (defun test ()
    ;; assume outermost f-str has var(0) and contains a PRED
    (f-align '|0| (open-and-import "ka/23.pl")
	     '|0| (open-and-import "nb/24.pl"))
    (format t "---~%")
    (f-align '|0| (open-and-import "ka/1.pl")
	     '|0| (open-and-import "nb/1.pl"))
    (format t "---~% This one will be troublesome:~%~%")
    (f-align '|0| (open-and-import "ka/4.pl")
	     '|0| (open-and-import "nb/5.pl")))
;;   (test)
  )


;;;;;;;; TESTING:
(lisp-unit:define-test test-unravel
  (let ((tab (dup-alist-to-table
	      '((|20| ("PRED" . |4|))
		(|4| "qePa" |8| NIL NIL)
		(|3| ("CASE" . "erg"))
		(|3| ("PRED" "kata" |8| NIL NIL))))))

    (lisp-unit:assert-equal
     '("PRED" "qePa" |8| NIL NIL)
     (get-pred '|20| tab))
    (lisp-unit:assert-equal
     '("PRED" "qePa" |8| NIL NIL)
     (unravel (assoc "PRED" (gethash '|20| tab) :test #'equal) tab))
    (lisp-unit:assert-equal
     '("PRED" "kata" |8| NIL NIL)
     (unravel (assoc "PRED" (gethash '|3| tab) :test #'equal) tab))))
