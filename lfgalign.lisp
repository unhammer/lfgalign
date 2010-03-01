;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

(define-condition several-topnodes (unexpected-input) ()
  (:report (lambda (condition stream)
	     (format stream "Found superfluous topmost nodes: ~A" (text condition)))))

(defun maketree (tab)
  "Returns a binary tree created from the |subtree| and |terminal|
alists of the table `tab'. The second value returned contains the
back-references from each branch ID. Does not modify the input
table. Efficiency: 100 real-life trees takes about 0.5 seconds on a
laptop, should be OK."
  (let* ((subtree (copy-tree (gethash '|subtree| tab)))
	 (tree (append subtree
		       (copy-tree (gethash '|terminal| tab))))
	 ;; subtree elts are of the form (id name left-id right-id) 
	 (refs (mapcan-true (lambda (b)
			      (list (aif (third b) (list it (first b) 'left))
				    (aif (fourth b) (list it (first b) 'right))))
			    subtree)))
    (loop
       for branch in tree 
       for ref = (assoc (car branch) refs) 
       when ref do
	 (case (third ref)
	   (left  (setf (third  (assoc (second ref) tree)) branch))
	   (right (setf (fourth (assoc (second ref) tree)) branch)))
       else
         collect branch into newtree
       finally
	 (aif (cdr newtree)
	     (error 'several-topnodes :text it)
	     (return (values (car newtree) refs))))))

(defun treefind (c-ids tree)
  "Unfortunately, id's aren't sorted in any smart way :-/"
  (and (listp tree)
       (if (member (car tree) c-ids)
	   tree
	   (or (and (third tree) (treefind c-ids (third tree)))
	       (and (fourth tree) (treefind c-ids (fourth tree)))))))

(defun topnode (f-var tab tree)
  "`f-var' describes a functional domain, find the topmost of the
nodes in the c-structure which project this domain"
  (let ((c-ids
	 (mapcar #'car
		 (remove-if (lambda (phi) (not (eq (cdr phi) f-var)))
			    (gethash '|phi| tab)))))
    (treefind c-ids tree)))

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


(progn
  (defun f-align (var1 tab1 var2 tab2)
    "`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively."
    (let* ((pred1 (get-pred var1 tab1))
	   (pred2 (get-pred var2 tab2)))
      (format t "Align ~A_~A with ~A_~A~%" var1 pred1 var2 pred2)
      (format t "Align tree ~A~%" (topnode var1 tab1 (maketree tab1)))
      (format t " with tree ~A~%" (topnode var2 tab2 (maketree tab2)))
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
    (f-align '0 (open-and-import "ka/23.pl")
	     '0 (open-and-import "nb/24.pl"))
    (format t "---~%")
    (f-align '0 (open-and-import "ka/1.pl")
	     '0 (open-and-import "nb/1.pl"))
    (format t "---~% This one will be troublesome:~%~%")
    (f-align '0 (open-and-import "ka/4.pl")
	     '0 (open-and-import "nb/5.pl")))
;;   (test)
  )


;;;;;;;; TESTING:
(lisp-unit:define-test test-unravel
  (let ((tab (dup-alist-to-table
	      '((20 ("PRED" . 4))
		(4 "qePa" 8 NIL NIL)
		(3 ("CASE" . "erg"))
		(3 ("PRED" "kata" 8 NIL NIL))))))

    (lisp-unit:assert-equal
     '("PRED" "qePa" 8 NIL NIL)
     (get-pred 20 tab))
    (lisp-unit:assert-equal
     '("PRED" "qePa" 8 NIL NIL)
     (unravel (assoc "PRED" (gethash 20 tab) :test #'equal) tab))
    (lisp-unit:assert-equal
     '("PRED" "kata" 8 NIL NIL)
     (unravel (assoc "PRED" (gethash 3 tab) :test #'equal) tab))))

(lisp-unit:define-test test-topnode
  (let* ((tab (open-and-import "dev/TEST_parse.pl"))
	 (tree (maketree tab)))
    (lisp-unit:assert-equal
     '(34 "qePa-2746-3" (21))
     (topnode 15 tab tree))
    (lisp-unit:assert-equal
     '(144 "PROPP" NIL (2 "PROP" NIL (1 "abramsma" (1))))
     (topnode 3 tab tree))))

(lisp-unit:define-test test-maketree
  (multiple-value-bind (tree refs)
      (maketree
       (open-and-import "dev/TEST_parse.pl"))
    (lisp-unit:assert-equal
     '((37 38 RIGHT) (24 23 RIGHT) (26 25 RIGHT) (28 27 RIGHT) (30 29 RIGHT)
 (32 31 RIGHT) (34 33 RIGHT) (33 172 RIGHT) (172 173 LEFT) (31 173 RIGHT)
 (173 174 LEFT) (29 174 RIGHT) (174 175 LEFT) (27 175 RIGHT) (175 176 LEFT)
 (25 176 RIGHT) (176 177 LEFT) (23 177 RIGHT) (177 378 RIGHT) (378 379 RIGHT)
 (1 2 RIGHT) (2 144 RIGHT) (144 149 RIGHT) (149 381 LEFT) (379 381 RIGHT)
 (381 385 RIGHT) (385 387 LEFT) (38 387 RIGHT))
     refs)
    (lisp-unit:assert-equal
     '(387 "ROOT"
       (385 "ROOT" NIL
        (381 "IPfoc[main,-]"
         (149 "IPfoc[main,-]" NIL
          (144 "PROPP" NIL (2 "PROP" NIL (1 "abramsma" (1)))))
         (379 "Ibar[main,-]" NIL
          (378 "I[main,-]" NIL
           (177 "V"
            (176 "V"
             (175 "V"
              (174 "V"
               (173 "V" (172 "V" NIL (33 "V_BASE" NIL (34 "qePa-2746-3" (21))))
                (31 "V_SUFF_BASE" NIL (32 "+V" (21))))
               (29 "V_SUFF_BASE" NIL (30 "+Unerg" (21))))
              (27 "V_SUFF_BASE" NIL (28 "+Aor" (21))))
             (25 "V_SUFF_BASE" NIL (26 "+Subj3Sg" (21))))
            (23 "V_SUFF_BASE" NIL (24 "+Obj3" (21))))))))
       (38 "PERIOD" NIL (37 "." (37))))
     tree)))