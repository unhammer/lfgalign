;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

(define-condition several-topnodes (unexpected-input) ()
  (:report (lambda (condition stream)
	     (format stream "Found superfluous topmost nodes: ~A" (text condition)))))

(defun get-equivs (val tab)
  (dset3-findall val (gethash '|eq-sets| tab)))

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
			 (get-equivs f-var tab)))
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
			 (get-equivs x tab))
		 (unravel-helper att stack seen tab))
	  (cons x (unravel-helper att stack seen tab))))))

(defun unravel (att val tab)
  (awhen (remove-duplicates (unravel-helper att (list val) nil tab)
			    :test #'equal)
    (when (cdr it)
      (error "Found superfluous, non-equal unravellings of ~A ~A:~%~A~%" val att it))
    (cons att (car it))))

(defun get-pred (var tab &optional no-error)
  "The `no-pred-error-todo' happens with ka/3.pl -- this file has no
selected parse (still ambiguous?). Use `no-error' to return nil if no
PRED was found."
  (if (equal "NULL" var)
      var
      (aif (unravel "PRED" var tab)
	   (cons var (cdr it))
	   (unless no-error (error 'no-pred-error-todo var)))))

(defun all-preds (tab)
  "TODO: cache/memoise in table?"
  (let (seen)
    (mapcar-true
     (lambda (x) (let ((var (car x)))
		   (when (and (numberp var)
			      (assoc "PRED" (cdr x) :test #'equal))
		     (unless (intersection seen (get-equivs var tab))
		       (pushnew var seen)
		       (get-pred var tab)))))
     (table-to-alist tab))))

(defun unreferenced-preds (tab)
  "Return a list of variables of those PRED's which are not children
of others."
  (let (vars backrefs)
    (mapcar (lambda (Pr)
	      (pushnew (first Pr) vars)
	      (setq backrefs (union (fourth Pr) (union (fifth Pr) backrefs))))
	    (all-preds tab))
    (remove-if (lambda (v) (member v backrefs)) vars)))


(defun outer> (Pr1 Pr2 tab) 
  "Is `Pr1' a predecessor of `Pr2' (outermore in the f-structure)?
Note: we actually only look at the car of `Pr2', which has to be its
variable (ie. what `get-pred' returns)."
  (let ((children (get-children Pr1)))
    (or (member (car Pr2) children)
	(loop for c in children
	   for Prc = (unravel "PRED" c tab)
	   thereis (and Prc
			(not (equal "NULL" c))
			(outer> Prc Pr2 tab))))))


(defun get-children (pred)
  (union (fourth pred) (fifth pred)))

(defun references (parentv childv tab)
  "Give a list of attributes of var `parentv' in `tab' which refer to
var `childv'."
  (loop
     for attval in (gethash parentv tab)
     when (eq childv (cdr attval))
     collect attval))


;;; LPT stuff:

(defun lemma (Pr) (second Pr))

(defun L (Pr tab)
  "Return the lexical expression of PRED `Pr'. Note: a \"pro\"
argument will return its verb as the first value!"
  (let* ((semform_id (third Pr))
	 (semform (assoc semform_id (gethash '|semform_data| tab)))
	 (preterminal (assoc (second semform)
			     (gethash '|subtree| tab)))
	 (terminal (assoc (fourth preterminal)
			  (gethash '|terminal| tab)))
	 (surfaceform (assoc (car (third terminal))
			     (gethash '|surfaceform| tab))))
    (second surfaceform)))

(defun get-LPT (w1 w2 LPTs)
  "For now, LPTs is just a cons of hash-tables, where the first lets
you look up a list of possible `w2' matches using `w1' as key, and the
second vice versa. Does not assume all keys in the first table are
values in the second (and vice versa). If neither `w1' nor `w2' are in
there, it's a trivial match.

As with hash tables, the second return value tells us whether either
\"key\" was in its table."
  (let* ((LPT1 (car LPTs))
	 (LPT2 (cdr LPTs))
	 (tr1 (gethash w1 LPT1))
	 (tr2 (gethash w2 LPT2)))
    (values
     (or (member w2 tr1 :test #'equal)
	 (member w1 tr2 :test #'equal)
	 (and (null tr1)
	      (null tr2)))
     (or tr1 tr2))))

(defun noun? (var tab)
  "TODO: do all and only nouns have an NTYPE?"
  (assoc "NTYPE" (gethash var tab) :test #'equal))

(defun LPT? (Pr_s tab_s Pr_t tab_t LPTs)
  "Are the lexical expressions of `Pr_s' and `Pr_t',
L(Pr_s) and L(Pr_t), Linguistically Predictable Translations?

True if both are in `get-LPT' as translations (or neither is), or one
is a pro and the other is a noun (see `noun?') or a pro.

TODO: The pro of a verb has that verb as its L, while the pro of a
reflexive has that reflexive... At the moment, we look up the L of
the pro no matter what, but will this give us trouble?"
  (let ((LPr_s (L Pr_s tab_s)) (lem_s (lemma Pr_s))
	(LPr_t (L Pr_t tab_t)) (lem_t (lemma Pr_t)))
    (or (and (equal lem_s "pro")
	     (equal lem_t "pro"))
	(and (equal lem_s "pro")
	     (noun? (car Pr_t) tab_t))
	(and (equal lem_t "pro")
	     (noun? (car Pr_s) tab_s))
	(multiple-value-bind (L-tr L-in) (get-LPT LPr_s LPr_t LPTs)
	  (multiple-value-bind (lem-tr lem-in) (get-LPT lem_s lem_t LPTs)
	    (unless (or L-in lem-in)
	      (warn "Neither ~A/~A nor ~A/~A are in LPTs" LPr_s lem_s LPr_t lem_t))
	    (and L-tr lem-tr))))))

(defun all-LPT (tab_s tab_t LPTs)
  (mapcan-true
   (lambda (Pr_s)
     (mapcar-true
      (lambda (Pr_t) (when (LPT? Pr_s tab_s Pr_t tab_t LPTs)
		      (list Pr_s Pr_t)))
      (all-preds tab_t)))
   (all-preds tab_s)))

(defun outer>-LPT (Pr_s tab_s var_t tab_t LPTs)
  "Return a list of the outermost possible `LPTs' of `Pr_s' in `tab_t'
starting at `var_s'."
  (let ((Pr_t (get-pred var_t tab_t 'noerror)))
    (when (and Pr_s Pr_t)  
      (if (LPT? Pr_s tab_s Pr_t tab_t LPTs)
	  (list var_t)
	  (loop 
	     for c in (get-children Pr_t)
	     for outer = (outer>-LPT Pr_s tab_s c tab_t LPTs)
	     when outer append it)))))

(defun all-outer>-LPT (tab_s tab_t LPTs)
  "Return an association list of all possible outermost LPTs, using
the variables of the PRED entries from `tab_s' as keys. So the
alist-entry (0 9 8) means that var 0 is an outermost PRED in `tab_s',
and 9 and 8 are outermost PRED's in `tab_t', and they are all possible
LPT's."
  (loop
     for var_s in (unreferenced-preds tab_s)
     for Pr_s = (get-pred var_s tab_s 'no-error)
     collect (cons var_s
		   (loop 
		      for var_t in (unreferenced-preds tab_t) 
		      for o = (outer>-LPT Pr_s tab_s var_t tab_t LPTs)
		      when o append it))))


;;; The actual alignment:

(defun foo (tab_s tab_t LPTs)
  "Intuition: Starting the alignment from outers and then going
inwards should have the effect that  

1. we don't get 'crossing' alignments, as we would have if 
   a-d and b-c where tab_s:[a [b ]] tab_t:[c [d ]], and
2. we prioritise alignments of outer elements with outer 
   elements.

However, it might turn out to be really hard in practice to keep
possible pairings prioritised when aligning. Eg. if we have

tab_s:[a [b]] tab_t:[c [d [e]] [f [g]]
and all LPT's are allowed except a-d, do we go
a-c,a-e,a-f,a-g
or
a-c,a-f,a-e,a-g 
or 
a-c,a-f,a-g,a-e (here counting e as deeper than g)
?

And if tab_s were [a [b]] [h [i]], when do we start considering h?

----

The alternative to ordering is of course just trying all permutations
of LPT-pairings. But would it still be simplest to follow some sort of
ordering? And how do we prioritise the full over the half-finished
alignments?

Proposal: do all pairings. Start with a pair of unreferenced-preds,
aligning inwards.

Another difficulty is that we can merge certain pairings, eg. where we
have a single causative PRED that aligns to a make PRED and a do PRED,
or an object PRED that's incorporated into an 'action'
PRED ('bilvaskingen'). 

Proposal: `LPT-permute' churns out all configurations of single
pairings allowed by LPT, while a `merge' function creates new possible
configurations (pushed last in the queue) based on sane merges. So
far, I don't think we'll want to merge where where two PRED's are on
the same level (eg. a subject and an object of the same PRED we
wouldn't merge), but a PRED and XCOMP (causative) or a PRED and its
object ('bilvaskingen') would be mergeable, these have exactly one
level between them (ie. one is the parent of another). I'm not sure
that we want to stop there though (we might want to go both deeper in
the f-structure and wider, ie. merge a pred with both its subject and
its object, or with both its daughter and granddaughter); but we can
leave that for later probably. ADJUNCTs should probably be mergeable
too.
"
  (loop for (Pr_s Pr_t) in (all-lpt tab_s tab_t LPTs)
       

       )
  )

(defun f-align (var1 tab1 var2 tab2)
  "`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively.
TODO: cache/memoise maketree"
  (let* ((pred1 (get-pred var1 tab1))
	 (pred2 (get-pred var2 tab2)))
    (format t "Align ~A with ~A~%" pred1 pred2)
    (unless (or (equal pred1 "NULL") (equal pred2 "NULL"))
      (format t "Align tree ~A~%" (pretty-topnode var1 tab1 (maketree tab1)))
      (format t " with tree ~A~%" (pretty-topnode var2 tab2 (maketree tab2)))
      (when (and pred1 pred2)
	(loop
	   for child1 in (get-children pred1)
	   for child2 in (get-children pred2)
	   do (format t "...aligning ~A_~A and ~A_~A...~%"
		      var1 (references var1 child1 tab1)
		      var2 (references var2 child2 tab2))
	   collect (f-align child1 tab1 child2 tab2))))))
  
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
  
(defun sanity-check ()
  (loop for i from 1 to 106
     for f = (concatenate 'string "nb/" (prin1-to-string i) ".pl")
     for unref = (unreferenced-preds (open-and-import f))
     when (not (equal '(0) unref)) do
     (format t "f:~A unref:~A~%" f unref)))

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

(lisp-unit:define-test test-L
  (let* ((tab (open-and-import "dev/TEST_parse.pl")))
    (lisp-unit:assert-equal "abramsma"
			    (L (get-pred 3 tab) tab))
    (lisp-unit:assert-equal "iqePa"
			    (L (get-pred 0 tab) tab))))

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
