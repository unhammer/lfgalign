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
  (let* ((f-vars (adjoin f-var
			 (dset3-findall f-var (gethash '|eq-sets| tab))))
	 (c-ids				; TODO: mapcar-true
	  (mapcar #'car
		  (remove-if (lambda (phi) (not (member (cdr phi) f-vars)))
			     (gethash '|phi| tab)))))
    (treefind c-ids tree)))

(defun skip-suff_base (tree)
  (when tree
    (if (search "SUFF_BASE" (second (fourth tree)))
	(skip-SUFF_BASE (third tree))
      (list (first tree) (second tree)
	    (skip-suff_base (third tree))
	    (skip-suff_base (fourth tree))))))

(defun pretty-topnode (f-var tab tree)
  "Skip the more boring nodes."
  (skip-suff_base (topnode f-var tab tree)))

(defun unravel-helper (att stack seen tab)
  "Call with empty `seen' and a `stack' containing the variable you
want to unravel the attribute `att' of. This function makes sure to
add all equivalent variables and their possible expansions."
  (when stack
    (let ((x (pop stack)))
      (pushnew x seen)
      (if (numberp x)
	  (progn (awhen (assoc att (gethash x tab) :test #'equal) ; add rhs
		   (pushnew (cdr it) stack))
		 (mapcar (lambda (equiv)             ; add all eq variables
			   (unless (member equiv seen)
			     (pushnew equiv stack)))
			 (dset3-findall x (gethash '|eq-sets| tab)))
		 (unravel-helper att stack seen tab))
	  (cons x (unravel-helper att stack seen tab))))))

(defun unravel (att val tab)
  (awhen (remove-duplicates (unravel-helper att (list val) nil tab)
			    :test #'equal)
    (when (cdr it) (error "Found superfluous, non-equal unravellings: ~A~%" it))
    (cons att (car it))))

(defun get-pred (var tab)
  (if (equal "NULL" var)
      (error 'unexpected-input "NULL-pred TODO")
      (aif (unravel "PRED" var tab)
	   (cons var (cdr it))
	   (error 'no-pred-error-todo var))))

(defun all-preds (tab)
  "TODO: cache/memoise in table?"
  (mapcar-true (lambda (x)
		 (when (and (numberp (car x))
			    (assoc "PRED" (cdr x) :test #'equal))
		   (get-pred (car x) tab)))
	       (table-to-alist tab)))

(defun get-children (pred)
  (append (fourth pred) (fifth pred)))

(defun references (parentv childv tab)
  "Give a list of attributes of var `parentv' in `tab' which refer to
var `childv'."
  (loop
     for attval in (gethash parentv tab)
     when (eq childv (cdr attval))
     collect attval))

(defun L (Pr tab)
  "Return the lexical expression of PRED `Pr', and the 'lemma' of Pr
as the second return value. Note: a \"pro\" argument will return its
verb as the first value!"
  (let* ((semform_id (third Pr))
	 (semform (assoc semform_id (gethash '|semform_data| tab)))
	 (preterminal (assoc (second semform)
			     (gethash '|subtree| tab)))
	 (terminal (assoc (fourth preterminal)
			  (gethash '|terminal| tab)))
	 (surfaceform (assoc (car (third terminal))
			     (gethash '|surfaceform| tab))))
    (when nil				; debug
      (print (list Pr semform_id semform preterminal terminal surfaceform)))
    (values (second surfaceform) (second Pr))))

(defun in-LPT (l1 l2 LPTs)
  "For now, LPTs is just a cons of hash-tables, where the first lets
you look up a list of possible `l2' matches using `l1' as key, and the
second vice versa. Does not assume all keys in the first table are
values in the second (and vice versa). If neither `l1' nor `l2' are in
there, it's a trivial match."
  (let ((LPT1 (car LPTs))
	(LPT2 (cdr LPTs)))
    (or (member l2 (gethash l1 LPT1) :test #'equal)
	(member l1 (gethash l2 LPT2) :test #'equal)
	(and (null (gethash l1 LPT1))
	     (null (gethash l2 LPT2))
	     (or (warn "No translations recorded for ~A and ~A" l1 l2) t)))))

(defun noun? (var tab)
  "TODO: do all and only nouns have an NTYPE?"
  (assoc "NTYPE" (gethash var tab) :test #'equal))

(defun LPT? (var1 tab1 var2 tab2 LPTs)
  "Are the lexical expressions of the pred of `var1' and of `var2',
L(Pr1) and L(Pr2), Linguistically Predictable Translations? 

True if we find no translations in `LPTs' using `in-LPT', or they're
both `in-LPT' as translations, or one is a pro and the other is a
noun (see `noun?')."
  (let ((Pr1 (get-pred var1 tab1))
	(Pr2 (get-pred var2 tab2)))
    (multiple-value-bind (LPr1 lem1) (L Pr1 tab1)
      (multiple-value-bind (LPr2 lem2) (L Pr2 tab2)
	(or (and (equal LPr1 "pro")
		 (equal LPr2 "pro"))
	    (and (equal LPr1 "pro")
		 (noun? var2 tab2))
	    (and (equal LPr2 "pro")
		 (noun? var1 tab1))
	    (in-LPT LPr1 LPr2 LPTs)
	    (in-LPT lem1 lem2 LPTs))))))

(defun f-align (var1 tab1 var2 tab2)
  "`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively.
TODO: cache/memoise maketree"
  (let* ((pred1 (get-pred var1 tab1))
	 (pred2 (get-pred var2 tab2)))
    (format t "Align ~A with ~A~%" pred1 pred2)
    (format t "Align tree ~A~%" (pretty-topnode var1 tab1 (maketree tab1)))
    (format t " with tree ~A~%" (pretty-topnode var2 tab2 (maketree tab2)))
    (when (and pred1 pred2)
      (loop
       for child1 in (get-children pred1)
       for child2 in (get-children pred2)
       do (format t "...aligning ~A_~A and ~A_~A...~%"
		  var1 (references var1 child1 tab1)
		  var2 (references var2 child2 tab2))
       collect (f-align child1 tab1 child2 tab2)))))
  
(defun test ()
  "Assumes outermost f-str has a var(0) containing a PRED"
  (f-align '0 (open-and-import "ka/23.pl")
	   '0 (open-and-import "nb/24.pl"))
  (format t "---~%")
  (f-align '0 (open-and-import "ka/1.pl")
	   '0 (open-and-import "nb/1.pl"))
  (format t "---~% This one will be troublesome, head-switching:~%~%")
  (f-align '0 (open-and-import "ka/4.pl")
	   '0 (open-and-import "nb/5.pl")))
  


;;;;;;;; TESTING:
(lisp-unit:define-test test-unravel
  (let ((tab (dup-alist-to-table
	      '((20 ("PRED" . 4))
		(|eqvar| (4 "qePa" 8 NIL NIL))
		(3 ("CASE" . "erg"))
		(|eqvar| (7 . 3))
		(3 ("PRED" "kata" 8 NIL NIL))
		(99 ("PRED" "kata" 9 NIL NIL))
		(0 ("PRED" . 18))
		(|eqvar| (18 "rekke-hand" 6 (20 19 21) ("NULL")))
		(2 ("PRED" . 150))
		(|eqvar| (150 . 18))
		(|eqvar| (150 . 100))
		(|eqvar| (11 "PanJara" 0 NIL NIL))
		(51 ("PRED" "PanJara" 0 NIL NIL))
		(50 ("PRED" . 11))
		(|eqvar| (51 . 50))))))
    (lisp-unit:assert-equal
     '(20 "qePa" 8 NIL NIL) (get-pred 20 tab))
    (lisp-unit:assert-equal
     '("PRED" "qePa" 8 NIL NIL) (unravel "PRED" 20 tab))
    (lisp-unit:assert-equal
     '("PRED" "kata" 9 NIL NIL) (unravel "PRED" 99 tab))
    (lisp-unit:assert-equal
     '("PRED" "kata" 8 NIL NIL) (unravel "PRED" 3 tab))
    (lisp-unit:assert-equal
     '("PRED" "kata" 8 NIL NIL) (unravel "PRED" 7 tab))
    (lisp-unit:assert-equal
     '("PRED" "rekke-hand" 6 (20 19 21) ("NULL")) (unravel "PRED" 18 tab))
    (lisp-unit:assert-equal
     '("PRED" "rekke-hand" 6 (20 19 21) ("NULL")) (unravel "PRED" 150 tab))
    (lisp-unit:assert-equal
     '("PRED" "rekke-hand" 6 (20 19 21) ("NULL")) (unravel "PRED" 100 tab))
    (lisp-unit:assert-equal
     '("PRED" "PanJara" 0 NIL NIL) (unravel "PRED" 51 tab))
    (lisp-unit:assert-equal
     '("PRED" "PanJara" 0 NIL NIL) (unravel "PRED" 11 tab))
    (lisp-unit:assert-equal
     '("PRED" "PanJara" 0 NIL NIL) (unravel "PRED" 50 tab))))

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
