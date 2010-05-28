;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

(define-condition several-topnodes (unexpected-input) ()
  (:report (lambda (condition stream)
	     (format stream "Found superfluous topmost nodes: ~A" (text condition)))))

(defvar *no-warnings* t)
(defvar *debug* nil)

;;;;;;;; C-STRUCTURE TREE:
;;;;;;;; -----------------

(defun terminal? (tree)
  "Return true if `tree' is a terminal node."
  (and (numberp (first tree))
       (stringp (second tree))
       (listp (third tree))
       (not (fourth tree))))

(defun maketree (tab)
  "Returns a binary tree created from the |subtree| and |terminal|
alists of the table `tab'. The second value returned contains the
back-references from each branch ID. Does not modify the input
table. Efficiency: 100 real-life trees takes about 0.5 seconds on a
laptop, should be OK."
  (when *debug* (loop for tr in (gethash '|terminal| tab)
		      when (not (terminal? tr)) do (error "terminal? failed on ~A" tr)))
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

(defun maketrim (tree)
  (cons 'snip (car tree)))
(defun trim? (tree)
  (not (listp (cdr tree))))

(defun trimtree (c-ids tree)
  "Trim off the branches of the tree that aren't in `c-ids'.
Where we've chopped off branches, we get a pair where the cdr is a
c-id number referring to what used to be there."
  (when tree
    (if (cddddr tree) (error "Non-binary tree!") t) ; none of these in my test-set
    (or (and (listp tree)
	     (member (car tree) c-ids)	; in-domain
	     (if (terminal? tree)
		 tree
		 (list (first tree)
		       (second tree)
		       (trimtree c-ids (third tree))
		       (trimtree c-ids (fourth tree)))))
	;; out-of-domain:
	(maketrim tree))))

(defun treefind (c-id tree)
  "Just return the subtree of `tree' starting with `c-id'."
  (if (and tree (listp tree))
      (if (eq (car tree) c-id)
	  tree
	  (or (treefind c-id (third tree))
	      (treefind c-id (fourth tree))))))

(defun topnodes (c-ids tree)
  "`c-ids' (given by `phi^-1') describes a functional domain, find the
first/topmost of the nodes in the c-structure which project this
domain (member of `c-ids').

Unfortunately, id's aren't sorted in any smart way :-/

TODO: instead of finding the topmost subtree, this should return the
list of \"all topmost\" such subtrees. However, in all of the MRS
suite test set, I can find no examples of discontinuous
constituents (or, functional domains described by discontinuous
subtrees)."
  (if (and tree (listp tree))
      (if (member (car tree) c-ids)
	  (values tree 0)		; gotcha!
	  (multiple-value-bind (Ltree Ldepth) (topnodes c-ids (third tree))
	    (multiple-value-bind (Rtree Rdepth) (topnodes c-ids (fourth tree))
	      (cond ((and Ltree Rtree) ; exists in both, choose shallowest
		     (error "Yay, found a discontinuous constituent! Boo, have work to do...")
		     (when (= Ldepth Rdepth) (warn "Equal depth, arbitrarily going right"))
		     (if (< Ldepth Rdepth)
			 (values Ltree (1+ Ldepth))
			 (values Rtree (1+ Rdepth))))
		    ;; not in both, but try either left or right
		    (Ltree (values Ltree (1+ Ldepth)))
		    (Rtree (values Rtree (1+ Rdepth)))))))))

(defun phi (c-id tab)
  "Returns the f-var given by phi of `c-id', use (gethash f-var `tab')
to find the content, but the f-var might have equivs..."
  (cdr (assoc c-id (gethash '|phi| tab))))

(defun phi^-1 (f-var tab)
  "The inverse phi (c-structure id's that map to `f-var' in
`tab'). Includes c-ids that map to variables that are equivalent to
`f-var'."
  (let ((f-vars (adjoin f-var
			(get-equivs f-var tab))))
    (mapcar-true #'car
		 (remove-if (lambda (phi)
			      (not (member (cdr phi) f-vars)))
			    (gethash '|phi| tab)))))

(defun skip-suff_base (tree)
  "Trees are either nil, a fourtuple, or a cons cell where the cdr is
a number (pointing to where the trimmed tree was cut off)."
  (when tree
    (if (or (trim? tree) (terminal? tree))
	tree
      (if (and (not (trim? (fourth tree)))
	       (search "SUFF_BASE" (second (fourth tree))))
	  (skip-SUFF_BASE (third tree))
	(list (first tree) (second tree)
	      (skip-suff_base (third tree))
	      (skip-suff_base (fourth tree)))))))

(defun pretty-topnode (f-var tab tree)
  "Skip the more boring nodes."
  (skip-suff_base (topnodes (phi^-1 f-var tab) tree)))


;;;;;;;; VARIOUS HELPERS:
;;;;;;;; ----------------

(defun null-pred? (Pr)
  (equal Pr "NULL"))

(defun get-equivs (val tab)
  (dset3-findall val (gethash '|eq-sets| tab)))

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

(defun pred-equal (Pr1 Pr2 tab)
  (labels ((args-equal (args1 args2)
	    (loop for arg1 in args1
		  for arg2 in args2
		  always (or (and (null-pred? arg1) (null-pred? arg2))
			     (member arg1 (get-equivs arg2 tab))))))
    (and (equal (first Pr1) (first Pr2))
	 (eq (second Pr1) (second Pr2))
	 (args-equal (third Pr1) (third Pr2))
	 (args-equal (fourth Pr1) (fourth Pr2)))))

(defun unravel (att val tab)
  (awhen (remove-duplicates (unravel-helper att (list val) nil tab)
			    :test (lambda (a b) (pred-equal a b tab)))
    (when (cdr it)
      (error "Found superfluous, non-equal unravellings of ~A ~A:~%~A~%" val att it))
    (cons att (car it))))

(defun get-pred (var tab &optional no-error)
  "Use `no-error' to return nil if no PRED was found."
  (if (null-pred? var)
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
  "Return a list of variables of those PRED's which are not arguments
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
  (let ((args (get-args Pr1)))
    (or (member (car Pr2) args)
	(loop for c in args
	   for Prc = (unravel "PRED" c tab)
	   thereis (and Prc
			(not (null-pred? c))
			(outer> Prc Pr2 tab))))))


(defun get-args (Pr &optional no-nulls)
  (if no-nulls
      (remove-if (lambda (p) (null-pred? p))
		 (union (fourth Pr) (fifth Pr)))
    (union (fourth Pr) (fifth Pr))))

(defun skip-adj-prep (var tab)
  "Should skip prepositions of adjuncts, as defined in footnote 3 in
dyvik2009lmp."
  (out "TODO: skip adj prep for ~A~%" var)
  var
  )

(defun get-adjs (var tab &optional no-error)
  "Use `no-error' to return nil if no ADJUNCT was found.
TODO: find example to test where we need `unravel' / eq-sets.
TODO: Skip prepositions of adjuncts, as defined in footnote 3 in
dyvik2009lmp, with `skip-adj-prep'."
  (let ((adjvar (assoc "ADJUNCT" (gethash var tab) :test #'equal)))
    (if adjvar
	(if (get-equivs (cdr adjvar) tab)
	    (error 'unexpected-input "eqvar of ADJUNCT, TODO")
	    (mapcar-true (lambda (pair) (when (eq (cdr pair) (cdr adjvar))
					  (skip-adj-prep (car pair) tab)))
			 (cdr (gethash '|in_set| tab))))
	(unless no-error (error 'no-adjs-error-todo var)))))

(defun references (parentv childv tab)
  "Give a list of attributes of var `parentv' in `tab' which refer to
var `childv'."
  (loop
     for attval in (gethash parentv tab)
     when (eq childv (cdr attval))
     collect attval))

(defun lemma (Pr) (second Pr))

(defun L (Pr tab)
  "Return the lexical expression (ie. surfaceform) of PRED `Pr'. 
Note: a \"pro\" argument will return its verb!"
  (let* ((semform_id (third Pr))
	 (semform (assoc semform_id (gethash '|semform_data| tab)))
	 (preterminal (assoc (second semform)
			     (gethash '|subtree| tab)))
	 (terminal (assoc (fourth preterminal)
			  (gethash '|terminal| tab)))
	 (surfaceform (assoc (car (third terminal))
			     (gethash '|surfaceform| tab))))
    (second surfaceform)))


;;;;;;;; LPT:
;;;;;;;; ----

(defun make-LPT ()
  "Just make an empty LPT pair"
  (cons (make-hash-table :test #'equal)
	(make-hash-table :test #'equal)))

(defun get-LPT (w_s w_t LPTs)
  "For now, LPTs is just a cons of hash-tables, where the first lets
you look up a list of possible `w_t' matches using `w_s' as key, and the
second vice versa. Does not assume all keys in the first table are
values in the second (and vice versa). If neither `w_s' nor `w_t' are in
there, it's a trivial match.

As with hash tables, the second return value tells us whether either
\"key\" was in its table."
  (let* ((LPT-s-t (car LPTs))
	 (LPT-t-s (cdr LPTs))
	 (tr_s (gethash w_s LPT-s-t))
	 (tr_t (gethash w_t LPT-t-s)))
    (values
     (or (member w_t tr_s :test #'equal)
	 (member w_s tr_t :test #'equal)
	 (and (null tr_s)
	      (null tr_t)))
     (or tr_s tr_t))))

(defun add-to-LPT (w_s w_t LPTs)
  "Add `w_s' and `w_t' to `LPTs' as translations of each other."
  (setf (gethash w_s (car LPTs))
	(pushnew w_t (gethash w_s (car LPTs)) :test #'equal))
  (setf (gethash w_t (cdr LPTs))
	(pushnew w_t (gethash w_t (cdr LPTs)) :test #'equal))
  LPTs)

(defun noun? (var tab)
  "TODO: do all and only nouns have an NTYPE?"
  (assoc "NTYPE" (gethash var tab) :test #'equal))

(defun LPT? (src tab_s trg tab_t LPTs)
  "Are the lexical expressions of `Pr_s' and `Pr_t',
L(Pr_s) and L(Pr_t), Linguistically Predictable Translations?

True if both are in `get-LPT' as translations (or neither is), or one
is a pro and the other is a noun (see `noun?') or a pro.

TODO: The pro of a verb has that verb as its L, while the pro of a
reflexive has that reflexive... At the moment, we look up the L of
the pro no matter what, but will this give us trouble?"
  (let* ((Pr_s (if (numberp src) (get-pred src tab_s) src))
	 (Pr_t (if (numberp trg) (get-pred trg tab_t) trg))
	 (LPr_s (L Pr_s tab_s)) (lem_s (lemma Pr_s))
	 (LPr_t (L Pr_t tab_t)) (lem_t (lemma Pr_t)))
    (or (and (equal lem_s "pro")
	     (equal lem_t "pro"))
	(and (equal lem_s "pro")
	     (noun? (car Pr_t) tab_t))
	(and (equal lem_t "pro")
	     (noun? (car Pr_s) tab_s))
	(multiple-value-bind (L-tr L-in) (get-LPT LPr_s LPr_t LPTs)
	  (multiple-value-bind (lem-tr lem-in) (get-LPT lem_s lem_t LPTs)
	    (cond ((and L-in lem-in) (or L-tr lem-tr))
		  (L-in L-tr)
		  (lem-in lem-tr)
		  (t (unless *no-warnings*
		       (warn "Neither ~A/~A nor ~A/~A are in LPTs" LPr_s lem_s LPr_t lem_t))
		     'trivial)))))))

(defun all-LPT (tab_s tab_t LPTs)
  "Return all possible LPTs of each PRED, as pairs of PREDs."
  (mapcan-true
   (lambda (Pr_s)
     (mapcar-true
      (lambda (Pr_t) (when (LPT? Pr_s tab_s Pr_t tab_t LPTs)
		      (list Pr_s Pr_t)))
      (all-preds tab_t)))
   (all-preds tab_s)))

(defun all-LPT-vars (tab_s tab_t LPTs)
  "Return an association list of all possible LPTs, using the
variables of the PRED entries from `tab_s' as keys. So the
alist-entry (0 9 8) means that var 0 is a PRED in `tab_s', and 9 and 8
are outermost PRED's in `tab_t', and they are all possible LPT's."
  (loop
     for Pr_s in (all-preds tab_s)
     collect (cons (car Pr_s)
		   (loop 
		    for Pr_t in (all-preds tab_t)
		    for o = (LPT? Pr_s tab_s Pr_t tab_t LPTs)
		    when o append (list (car Pr_t))))))

(defun longest-sublists (lists)
  (let ((maxlen (loop for l in lists maximize (length l))))
    (mapcar-true (lambda (l) (when (>= (length l) maxlen) l))
		 lists)))




;;;;;;;; ALIGNMENT:
;;;;;;;; ----------

(defun argalign (link tab_s tab_t LPTs)
  "Return all possible combinations of links from `args_s'/`adjs_s' to
`args_t'/`adjs_t' that include all members of `args_s' and `args_t',
ie. return all pairs of

 (args_s X (args_t U adjs_t)) U ((args_s U adjs_s) X args_t) 

s.t. all args are in the set.  An arg_s may be linked to a member of
`adjs_t' (and vice versa), but no pairs of adj_s and adj_t are
included.

Return nil if no alignments are possible, and (list nil) if no
alignments are necessary (there are no arguments to align).

Only returns pairs which are LPT, as defined by `LPTs'. This should
cover (iii) and (iv). See `argalign-p'."
  (let* ((var_s (car link))
	 (var_t (cdr link))
	 (adjs_s (get-adjs var_s tab_s 'no-error))
	 (adjs_t (get-adjs var_t tab_t 'no-error))
	 (args_s (get-args (get-pred var_s tab_s t) 'no-nulls))
	 (args_t (get-args (get-pred var_t tab_t t) 'no-nulls)))
    (when *debug* (format t "~A ~A~%~A ~A~%" args_s adjs_s args_t adjs_t))
    (argalign-p args_s adjs_s args_t adjs_t tab_s tab_t LPTs)))

(defun argalign-p (args_s adjs_s args_t adjs_t &optional tab_s tab_t LPTs)
  "Helper for `argalign'. If `LPTs', `tab_s' and `tab_t' are supplied,
only return those combinations where all pairs are LPT. "
  (macrolet
      ((mapalign (srcs trgs)
	 "Within one call, we have the same src, but loop through possible `trgs',
the recursion loops through all possible `srcs'."
	 `(mapcan			; for each target arg/adj
	   (lambda (trg)
	     (when (or (not LPTs)
		       (LPT? (get-pred (car ,srcs) tab_s) tab_s
			     (get-pred     trg     tab_t) tab_t LPTs))
	       (mapcar-true	 ; for each permuation w/o src and trg
		(lambda (perm)
		  (cons (cons (car ,srcs) trg)
			perm))
		;; recurse, removing the arg/adj that we used:
		(argalign-p (if (eq ,srcs args_s) (cdr args_s) args_s)
			    (if (eq ,srcs adjs_s) (cdr adjs_s) adjs_s)
			    (if (eq ,trgs args_t) (remove trg args_t :count 1) args_t)
			    (if (eq ,trgs adjs_t) (remove trg adjs_t :count 1) adjs_t)
			    tab_s tab_t LPTs))))
	   ,trgs)))
    (if args_s
	(append (mapalign args_s args_t)
		(mapalign args_s adjs_t))
	(if args_t			; no args_s
	    (if adjs_s
		(mapalign adjs_s args_t)
		;; no adjs_s, fail:
		nil)
	    ;; all args_s and args_t used up, make end-of-list:
	    (list nil)))))

(defun adjalign (exclude link tab_s tab_t LPTs)
  "Return all possible combinations of links between adjuncts that use
id's from `exclude' (a list of links).

Return nil if no alignments are possible.

Only returns pairs which are LPT, as defined by `LPTs'. See
`adjalign-p'."
  (let* ((var_s (car link))
	 (var_t (cdr link))
	 (adjs_s (get-adjs var_s tab_s 'no-error))
	 (adjs_t (get-adjs var_t tab_t 'no-error)))
    (when *debug* (out "~A~%~A~%" adjs_s adjs_t))
    (adjalign-p exclude adjs_s adjs_t tab_s tab_t LPTs)))

(defun adjalign-p (exclude adjs_s adjs_t &optional tab_s tab_t LPTs)
  "Helper for `adjalign'. If `LPTs', `tab_s' and `tab_t' are supplied,
only return those combinations where all pairs are LPT."
  (when (and adjs_s adjs_t)
    (mapcan				; for each target adj
     (lambda (trg)
       (let ((link (cons (car adjs_s) trg)) ; "this one"
	     ;; recursion, w/o this one:
	     (perms (adjalign-p exclude
				(cdr adjs_s)
				(remove trg adjs_t :count 1)
				tab_s tab_t LPTs)))
	 (append		; Collecting:
	  perms			; - all but this one
	  (when (and
		 (not (member-either link exclude))
		 (or (not LPTs)
		     (LPT? (get-pred (car adjs_s) tab_s) tab_s
			   (get-pred     trg      tab_t) tab_t LPTs)))
	    (append
	     (list (list link))	; - just this one
	     (mapcar-true       ; - this added to each permuation w/o this
	      (lambda (perm)
		(cons link perm))
	      perms))))))
     adjs_t)))

(defun make-aligntab () (make-hash-table :test #'equal))

(defun f-align (link tab_s tab_t LPTs &optional aligntab)
  "Optional hash table `aligntab' (with :test #'equal) is
destructively modified to store the alignments of all linkings, and lets
you check whether each linking was possible to sub-align (in addition
to being LPT).

 (i) the number of arguments n and m may or may not differ
is trivially true
 (ii), LPT, should be covered for `link' on all calls.
TODO: (v) the LPT-correspondences can be aligned one-to-one
TODO: adj-adj alignments?? (unaligned adjuncts are OK)."
  (let* ((aligntab (or aligntab
		       (make-hash-table :test #'equal)))
	 (argperms (argalign link tab_s tab_t LPTs)) ; argalign covers (iii) and (iv)
	 alignments)
    (when argperms		  ; don't do anything unless argperms are OK
      (flet ((sub-f (perm)
	       ;; Try to recursively align links in perm, but keep unaligned links
	       (loop for link_a in perm
		  do (unless (gethash link_a aligntab)
		       (setf (gethash link_a aligntab)
			     (f-align link_a tab_s tab_t LPTs aligntab)))
		  collect (or (gethash link_a aligntab)
			      (or (unless *no-warnings* (warn "sub-f failed:~A" link_a))
				  link_a)))))
	(if (equal argperms '(nil))
	    ;; no recursion on args needed, try adjs
	    (loop for adjperm in (adjalign nil link tab_s tab_t LPTs)
	       do (pushnew (sub-f adjperm) alignments :test #'equal))
	    ;; possible with recursion on args
	    (loop for argperm in argperms
	       for new = (sub-f argperm)
	       do (pushnew new alignments :test #'equal)
	       ;; try to fill up adj alignments
	       (loop for adjperm in (adjalign argperm link tab_s tab_t LPTs)
		  do (pushnew (append (sub-f adjperm) new) alignments :test #'equal)))))
      (if alignments
	  (cons link alignments)
	link))))

(lisp-unit:define-test test-f-align
 (let ((tab_s (open-and-import "dev/TEST_simple_s.pl"))
       (tab_t (open-and-import "dev/TEST_simple_t.pl")))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (5 . 3)))
    (flatten (f-align '(0 . 0) tab_s tab_t (make-LPT)))))
 (let ((tab_s (open-and-import "dev/TEST_regargadj_s.pl"))
       (tab_t (open-and-import "dev/TEST_regargadj_t.pl"))
       (LPT (make-LPT)))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (11 . 6) (10 . 9) (9 . 3)) ((0 . 0) (11 . 6) (10 . 3) (9 . 9))
      ((0 . 0) (11 . 9) (10 . 6) (9 . 3)) ((0 . 0) (11 . 9) (10 . 3) (9 . 6))
      ((0 . 0) (11 . 3) (10 . 6) (9 . 9)) ((0 . 0) (11 . 3) (10 . 9) (9 . 6)))
    (flatten (f-align '(0 . 0) tab_s tab_t LPT)))
   (add-to-lpt "Browne" "Browne" LPT)
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (11 . 6) (10 . 3) (9 . 9))
      ((0 . 0) (11 . 9) (10 . 3) (9 . 6)))
    (flatten (f-align '(0 . 0) tab_s tab_t LPT))))
 (let ((tab_s (open-and-import "dev/TEST_optadj_s.pl"))
       (tab_t (open-and-import "dev/TEST_optadj_t.pl"))
       (LPT (make-LPT)))
   (out "~%~%TODO: skip prepositions when aligning adjs (id 30 below)~%~%")
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (8 . 2) (30 . 8))	; adjuncts optionally align
      ((0 . 0) (8 . 8) (30 . 2))
      ((0 . 0) (8 . 8)))
    (flatten (f-align '(0 . 0) tab_s tab_t LPT)))))

(defun f-link? (x)
  (and (atom (car x))
       (atom (cdr x))))

(defun rank (f-alignments aligntab)
  (unless *no-warnings* (warn "mock ranking, just selects first option"))
  (car (flatten f-alignments)))

(defun spread (flatperm)
  (when flatperm
    (if (cdr flatperm)
	(if (f-link? (car flatperm))
	    (mapcar (lambda (rest)
		      (cons (car flatperm) rest))
		    (spread (cdr flatperm)))
	  (mapcan (lambda (first)
		    (mapcar (lambda (rest)
			      (append first rest))
			    (spread (cdr flatperm))))
		  (car flatperm)))
      (if (f-link? (car flatperm))
	  (list flatperm)
	(car flatperm)))))

(defun flatten (f-alignments)
  "`f-alignments' is a _member_ of an assoc-list, where the car is an `f-link?'."
  (when f-alignments
    (let ((elt (car f-alignments))
	  (perms (cdr f-alignments)))
      (if perms
	  (let* ((flatperms (mapcan
			     (lambda (links)
			       (spread (mapcar (lambda (a)
						 (if (f-link? a) a (flatten a)))
					       links)))
			     perms)))
	    (mapcar (lambda (p)
		      (cons elt p))
		    flatperms))
	  (list elt)))))

(defun subnodes-list (tree)
  "Debug function, deprecated. Return id's of this node and all
subnodes of `tree' as a list."
  (when (trim? tree) (error "Trimmed tree sent to subnodes-list, don't do that."))
  (if tree
      (adjoin (car tree)
	      (union (subnodes-list (third tree))
		     (subnodes-list (fourth tree))))))

(defun preterms (tree)
  "Debug function, deprecated. Return id's of all pre-terminals under
the subtree `tree' as a list."
  (when (trim? tree) (error "Trimmed tree sent to preterms, don't do that."))
  (if tree
      (if (or (terminal? (third tree)) (terminal? (fourth tree)))
	  (if (and (third tree) (fourth tree))
	      (error "Unexpected branching in preterminal, TODO")
	      (list (car tree)))
	  (append (preterms (third tree))
		  (preterms (fourth tree))))))

(defun LL-single (c-id f-alignment tree tab from)
  "Debug function, deprecated. Find the set of linked preterminals
dominated by `c-id', return their links as a list.  The linking is
defined by `f-alignment', which must have been through `flatten';
`tree' is given by `maketree'. The argument `from' is an atom, either
'src or 'trg, giving the side of `c-id', `tree' and `tab' in
`f-alignment'."
  (labels ((get-link (src)
	     (let* ((getter (if (eq from 'trg)
				#'rassoc
				#'assoc))
		    (links (mapcar-true (lambda (var) (funcall getter var f-alignment))
				       (adjoin src (get-equivs src tab)))))
	       (when (cdr links) (error "More than one link: ~A for f-var: ~A" links src))
	       (car links))))
    (let* ((prets (preterms (treefind c-id tree)))
	   (phis (remove-duplicates (mapcar (lambda (p) (phi p tab))
					    prets))))
      (mapcar #'get-link phis))))


(defclass LL-splits ()
  ;"A table with lists of links as keys, and lists of nodes as values,
;used by `c-align'" 
  ((table
    :accessor LL-tab
    :initform (make-hash-table :test #'equal))))

(defmethod add (key val (splits LL-splits))
  (let* ((skey (sort (copy-seq key)
		     (lambda (a b) (or (< (car a) (car b))
				       (and (= (car a) (car b))
					    (< (cdr a) (cdr b)))))))
	 (old (gethash skey (LL-tab splits))))
    (if old	
	(setf (gethash skey (LL-tab splits)) (pushnew val old))
	(setf (gethash skey (LL-tab splits)) (list val)))))

(defmethod get-val (key (splits LL-splits))
  (let ((skey (sort (copy-seq key)
		    (lambda (a b) (or (< (car a) (car b))
				      (and (= (car a) (car b))
					   (< (cdr a) (cdr b))))))))
    (gethash skey (LL-tab splits))))

(defmethod add-links (f-alignment tree tab (splits LL-splits) from)
  "Return links of all pre-terminals under the subtree `tree' as a
list. As a side-effect, if any of the preterminals under this node
have an `f-alignment' (this must have been through `flatten'), add the
current node to `splits'. The argument `from' is an atom, either 'src
or 'trg, giving the side of `tree' (see `maketree') and `tab' in
`f-alignment'.

dyvik2009lmt says linked _lexical_ nodes, I use preterminals since
some times we don't get all the way down terminals (eg in dev/TEST_simple_s.pl in
the MRS suite, Abrams is a different f-domain from its mother, while
in dev/TEST_simple_t.pl, qePa is a different f-domain; I don't know why, but their
phi's don't match anything in the files)."
  (when (trim? tree) (error "Trimmed tree sent to add-links, don't do that."))
  (labels ((get-link (src)
	     (let* ((getter (if (eq from 'trg)
				#'rassoc
				#'assoc))
		    (links (mapcar-true (lambda (var) (funcall getter var f-alignment))
					(adjoin src (get-equivs src tab)))))
	       (when (cdr links) (error "More than one link: ~A for f-var: ~A" links src))
	       (car links))))
    (when tree
      (awhen (if (or (terminal? (third tree)) (terminal? (fourth tree)))
		 (if (and (third tree) (fourth tree))
		     (error "Unexpected branching in preterminal, TODO")
		     (list (get-link (phi (car tree) tab))))
		 (union (add-links f-alignment (third tree) tab splits from)
			(add-links f-alignment (fourth tree) tab splits from)))
	(add it (car tree) splits)
	it))))


(defun c-align-all (flat-alignments tab_s tab_t)
  "`f-alignments' must have been through `flatten'."
  (let ((tree_s (maketree tab_s))
	(tree_t (maketree tab_t)))
    (mapcar (lambda (f-alignment)
	      (c-align-ranked f-alignment tree_s tab_s tree_t tab_t))
	    flat-alignments)))

(defun hash-key-intersect (tab1 tab2)
  (loop for k being the hash-keys of tab1 collect (gethash k tab2)))

(defun c-align-ranked (f-alignment tree_s tab_s tree_t tab_t)
  "Align trees for a single, flat `f-alignment'."
  (let ((splits_s (make-instance 'LL-splits))
	(splits_t (make-instance 'LL-splits)))
    (add-links f-alignment tree_s tab_s splits_s 'src)
    (add-links f-alignment tree_t tab_t splits_t 'trg)
    (let ((linkable (hash-key-intersect (LL-tab splits_s)
					(LL-tab splits_t))))
      (mapcar
       (lambda (links)
	 (cons (get-val links splits_s)
	       (get-val links splits_t)))
       linkable))))

(defun align (tab_s tab_t LPT)
  (let* ((aligntab (make-aligntab))
	 (f-alignments (f-align '(0 . 0) tab_s tab_t LPT aligntab))
	 (best-f-alignment (rank f-alignments))
	 (c-alignments (c-align-ranked best-f-alignment 
				       (maketree tab_s) tab_s
				       (maketree tab_t) tab_t))
	 ;; TODO: pretty-print
	 c-alignments)))


(defun f-align-naive (var1 tab1 var2 tab2)
  "Just to show the difference between naive alignment, and the complete alignment.
`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively.  TODO: cache/memoise maketree"
  (let* ((pred1 (get-pred var1 tab1))
	 (pred2 (get-pred var2 tab2)))
    (format t "Align ~A with ~A~%" pred1 pred2)
    (unless (or (null-pred? pred1) (null-pred? pred2))
      (format t "Align tree ~A~%" (pretty-topnode var1 tab1 (maketree tab1)))
      (format t " with tree ~A~%" (pretty-topnode var2 tab2 (maketree tab2)))
      (when (and pred1 pred2)
      (loop
	 for arg1 in (get-args pred1)
	 for arg2 in (get-args pred2)
	 do (format t "...aligning ~A_~A and ~A_~A...~%"
		    var1 (references var1 arg1 tab1)
		    var2 (references var2 arg2 tab2))
	 collect (f-align-naive arg1 tab1 arg2 tab2))))))

(defun test-f-align-naive ()
  "Assumes outermost f-str has a var(0) containing a PRED"
  (f-align-naive '0 (open-and-import "ka/23.pl")
		 '0 (open-and-import "nb/24.pl"))
  (format t "---~%")
  (f-align-naive '0 (open-and-import "dev/TEST_simple_t.pl")
		 '0 (open-and-import "dev/TEST_simple_s.pl"))
  (format t "---~% This one will be troublesome, head-switching:~%~%")
  (f-align-naive '0 (open-and-import "dev/TEST_regargadj_t.pl")
		 '0 (open-and-import "nb/5.pl")))


;;;;;;;; TESTING:
;;;;;;;; --------
(defun set-equal (a1 a2)
  (ignore-errors (not (set-exclusive-or a1 a2 :test #'equal))))
(defun set-of-set-equal (as1 as2)
  (ignore-errors (not (set-exclusive-or as1 as2 :test #'set-equal))))

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

(lisp-unit:define-test test-pred-equal
  (let ((tab (open-and-import "dev/TEST_pred-equal.pl")))
    (lisp-unit:assert-true
     (pred-equal '("perf" 6 (28) (37))
     '("perf" 6 (28) (29))
     tab))
    (lisp-unit:assert-true
     (pred-equal '("perf" 6 (28) (37))
     '("perf" 6 (1) (29))
     tab))
    (lisp-unit:assert-false
     (pred-equal '("perf" 6 (28) (37))
     '("perf" 6 (28) (9))
     tab))))

(lisp-unit:define-test test-L
  (let* ((tab (open-and-import "dev/TEST_parse.pl")))
    (lisp-unit:assert-equal "abramsma"
			    (L (get-pred 3 tab) tab))
    (lisp-unit:assert-equal "iqePa"
			    (L (get-pred 0 tab) tab))))

(lisp-unit:define-test test-LPT
  (let* ((tab_s (open-and-import "dev/TEST_regargadj_s.pl"))
	 (tab_t (open-and-import "dev/TEST_regargadj_t.pl"))
	 (Pr_s (get-pred 0 tab_s))
	 (Pr_t (get-pred 0 tab_t))
	 (Pr_s_wrong (get-pred 10 tab_s)))
    (let ((LPT (make-LPT)))
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT))
      (lisp-unit:assert-true (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT))
      (add-to-lpt "rekke-hand" "mi-codeba" LPT)
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT))
      (lisp-unit:assert-false (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT)))
    (let ((LPT (make-LPT)))
      (add-to-lpt "rakte" "miacoda" LPT)
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT)) 
      (lisp-unit:assert-false (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT))
      (add-to-lpt "rekke-hand" "mi-codeba" LPT)
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT))
      (lisp-unit:assert-false (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT)))
    (let ((LPT (make-LPT)))
      (add-to-lpt "rakte" "miacoda" LPT)
      (add-to-lpt "rekke-hand" "foobar" LPT)
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT))
      (lisp-unit:assert-false (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT)))
    (let ((LPT (make-LPT)))
      (add-to-lpt "rakte" "foobar" LPT)
      (add-to-lpt "rekke-hand" "mi-codeba" LPT)
      (lisp-unit:assert-true (LPT? Pr_s tab_s Pr_t tab_t LPT))
      (lisp-unit:assert-false (LPT? Pr_s_wrong tab_s Pr_t tab_t LPT)))))

(lisp-unit:define-test test-argalign
 (let ((tab_s (open-and-import "dev/TEST_permute_s.pl"))
       (tab_t (open-and-import "dev/TEST_permute_t.pl")))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '()
    (argalign '(0 . 0) tab_s tab_t (make-LPT))))
 (let ((tab_s (open-and-import "dev/TEST_argadj_s.pl"))
       (tab_t (open-and-import "dev/TEST_argadj_t.pl")))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((30 . 46) (27 . 10) (29 . 37) (28 . 2))
      ((30 . 46) (27 . 10) (29 . 2) (28 . 37))
      ((30 . 46) (27 . 37) (29 . 10) (28 . 2))
      ((30 . 46) (27 . 37) (29 . 2) (28 . 10))
      ((30 . 46) (27 . 2) (29 . 10) (28 . 37))
      ((30 . 46) (27 . 2) (29 . 37) (28 . 10))
      ((30 . 10) (27 . 46) (29 . 37) (28 . 2))
      ((30 . 10) (27 . 46) (29 . 2) (28 . 37))
      ((30 . 10) (27 . 37) (29 . 46) (28 . 2))
      ((30 . 10) (27 . 37) (29 . 2) (28 . 46))
      ((30 . 10) (27 . 2) (29 . 46) (28 . 37))
      ((30 . 10) (27 . 2) (29 . 37) (28 . 46))
      ((30 . 37) (27 . 46) (29 . 10) (28 . 2))
      ((30 . 37) (27 . 46) (29 . 2) (28 . 10))
      ((30 . 37) (27 . 10) (29 . 46) (28 . 2))
      ((30 . 37) (27 . 10) (29 . 2) (28 . 46))
      ((30 . 37) (27 . 2) (29 . 46) (28 . 10))
      ((30 . 37) (27 . 2) (29 . 10) (28 . 46))
      ((30 . 2) (27 . 46) (29 . 10) (28 . 37))
      ((30 . 2) (27 . 46) (29 . 37) (28 . 10))
      ((30 . 2) (27 . 10) (29 . 46) (28 . 37))
      ((30 . 2) (27 . 10) (29 . 37) (28 . 46))
      ((30 . 2) (27 . 37) (29 . 46) (28 . 10))
      ((30 . 2) (27 . 37) (29 . 10) (28 . 46)))
    (argalign '(0 . 0) tab_s tab_t (make-LPT)))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((30 . 46) (27 . 10) (29 . 37) (28 . 2))
      ((30 . 46) (27 . 10) (29 . 2) (28 . 37))
      ((30 . 46) (27 . 37) (29 . 10) (28 . 2))
      ((30 . 46) (27 . 37) (29 . 2) (28 . 10))
      ((30 . 46) (27 . 2) (29 . 10) (28 . 37))
      ((30 . 46) (27 . 2) (29 . 37) (28 . 10)))
    (argalign '(0 . 0) tab_s tab_t (add-to-LPT "regnet" "cvimda" (make-LPT))))))

(lisp-unit:define-test test-argalign-p
  (lisp-unit:assert-equality
   #'set-of-set-equal			; ignore (C . 3), both adj
   '(((A . 1) (B . 2)) ((A . 1) (B . 3) (C . 2)) 
     ((A . 2) (B . 1)) ((A . 2) (B . 3) (C . 1))
                       ((A . 3) (B . 1) (C . 2))
                       ((A . 3) (B . 2) (C . 1)))
   (argalign-p '(a b) '(c) '(1 2) '(3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal			; ignore (C . 3) and (C . 4)
   '(((A . 1) (B . 2)) ((A . 1) (B . 3) (C . 2)) ((A . 1) (B . 4) (C . 2))
     ((A . 2) (B . 1)) ((A . 2) (B . 3) (C . 1)) ((A . 2) (B . 4) (C . 1))
     ((A . 3) (B . 1) (C . 2)) ((A . 3) (B . 2) (C . 1))
     ((A . 4) (B . 1) (C . 2)) ((A . 4) (B . 2) (C . 1)))
   (argalign-p '(a b) '(c) '(1 2) '(3 4)))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '()					; breaks constraint (iii)
   (argalign-p '(a b) '(c) '(1) '()))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '()					; breaks constraint (iv)
   (argalign-p '(a) '() '(1 2) '(3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal			; ignore (C . 3), both adj
   '(((A . 1) (C . 2)) ((A . 2) (C . 1)))
   (argalign-p '(a) '(c) '(1 2) '(3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal			; ignore (C . 3), both adj
   '(((A . 1) (B . 3)) ((A . 3) (B . 1)))
   (argalign-p '(a b) '(c) '(1) '(3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((A . 1) (B . 3)) ((A . 3) (B . 1)))
   (argalign-p '(a b) '() '(1) '(3))))

(lisp-unit:define-test test-adjalign-p
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((A . 2)) ((B . 2)) ((A . 3)) ((B . 3)) ((A . 2) (B . 3)) ((B . 2) (A . 3)))
   (adjalign-p '((c . 1)) '(a b) '(1 2 3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((A . 1)) ((B . 1)) ((A . 2)) ((B . 2)) ((A . 3)) ((B . 3))
     ((A . 1)(B . 2)) ((A . 2)(B . 1))((A . 3)(B . 1))
     ((A . 1)(B . 3)) ((A . 2)(B . 3))((A . 3)(B . 2)))
   (adjalign-p nil '(a b) '(1 2 3)))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((A . 2)) ((B . 2)))
   (adjalign-p '((c . 1)) '(a b) '(1 2)))
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((A . 1)) ((A . 1) (B . 2)) ((B . 2))
     ((A . 2)) ((A . 2) (B . 1)) ((B . 1)))
   (adjalign-p nil '(a b) '(1 2))))


(lisp-unit:define-test test-spread
 (lisp-unit:assert-equal '(((21 . 37) (19 . 46) (A . B) (20 . 46))
			   ((21 . 37) (19 . 46) (A . C) (20 . 46)))
			 (spread '((21 . 37)
				   (((19 . 46) (A . B)) ((19 . 46) (A . C)))
				   (20 . 46))))
 (lisp-unit:assert-equal '(((19 . 46) (A . B))
			   ((19 . 46) (A . C)))
			 (spread '((((19 . 46) (A . B)) ((19 . 46) (A . C))))))
 (lisp-unit:assert-equal '(((21 . 37) (19 . 46) (A . B))
			   ((21 . 37) (19 . 46) (A . C)))
			 (spread '((21 . 37)
				   (((19 . 46) (A . B)) ((19 . 46) (A . C))))))
 (lisp-unit:assert-equal '(((21 . 37) (19 . 46) (A . B) (20 . 46)))
			 (spread '((21 . 37) (((19 . 46) (A . B))) (20 . 46))))
 (lisp-unit:assert-equal '(((G . H) (D . Y) (19 . 46) (A . B) (20 . 46))
			   ((G . H) (D . Y) (19 . 46) (A . C) (20 . 46)))
			 (spread '((((G . H) (D . Y)))
				   (((19 . 46) (A . B)) ((19 . 46) (A . C)))
				   (20 . 46)))))

(lisp-unit:define-test test-flatten
  (lisp-unit:assert-equal '((e . f)) (flatten '((e . f))))
  (lisp-unit:assert-equal '(((0 . 0) (5 . 3))) (flatten '((0 . 0) ((5 . 3)))))
  (lisp-unit:assert-equal
   '(((A . B) (C . D) (E . F)) ((A . B) (5 . 6)))
   (flatten '((a . b) ((c . d) (e . f)) ((5 . 6)))))
  (lisp-unit:assert-equal
   '(((A . B) (C . D) (E . F)) ((A . B) (5 . 6) (7 . 8)))
   (flatten '((a . b) ((c . d) (e . f)) ((5 . 6) (7 . 8)))))
  (lisp-unit:assert-equal
   '(((0 . 0) (11 . 6) (10 . 9) (a . b) (9 . 3))
     ((0 . 0) (11 . 6) (10 . 3) (9 . 9))
     ((0 . 0) (11 . 9) (10 . 6) (9 . 3)) ((0 . 0) (11 . 9) (10 . 3) (9 . 6))
     ((0 . 0) (11 . 3) (10 . 6) (9 . 9)) ((0 . 0) (11 . 3) (10 . 9) (9 . 6)))
   (flatten '((0 . 0)
	      ((11 . 6) ((10 . 9) ((a . b))) (9 . 3))
	      ((11 . 6) (10 . 3) (9 . 9))
	      ((11 . 9) (10 . 6) (9 . 3)) ((11 . 9) (10 . 3) (9 . 6))
	      ((11 . 3) (10 . 6) (9 . 9)) ((11 . 3) (10 . 9) (9 . 6)))))
  (lisp-unit:assert-equal
   '(((0 . 0) (11 . 6) (10 . 9) (9 . 3)) ((0 . 0) (11 . 6) (10 . 3) (9 . 9))
     ((0 . 0) (11 . 9) (10 . 6) (9 . 3)) ((0 . 0) (11 . 9) (10 . 3) (9 . 6))
     ((0 . 0) (11 . 3) (10 . 6) (9 . 9)) ((0 . 0) (11 . 3) (10 . 9) (9 . 6)))
   (flatten '((0 . 0)
	      ((11 . 6) (10 . 9) (9 . 3)) ((11 . 6) (10 . 3) (9 . 9))
	      ((11 . 9) (10 . 6) (9 . 3)) ((11 . 9) (10 . 3) (9 . 6))
	      ((11 . 3) (10 . 6) (9 . 9)) ((11 . 3) (10 . 9) (9 . 6))))))

(lisp-unit:define-test test-trimtree
 (let* ((tab_s (open-and-import "dev/TEST_simple_s.pl"))
        (tree_s (maketree tab_s))
	(c0 (phi^-1 0 tab_s))
	(c5 (phi^-1 5 tab_s)))
   (lisp-unit:assert-equal
   '(553 "ROOT"
     (591 "ROOT" NIL
      (1094 "IP" (715 "IP" NIL (SNIP . 235))
       (1073 "I\\'" NIL
        (869 "Vfin"
         (868 "Vfin"
          (867 "Vfin" NIL (22 "V_BASE" NIL (23 "bjeffe" (17))))
          (20 "V_SUFF_BASE" NIL (21 "+Verb" (17))))
         (18 "V_SUFF_BASE" NIL (19 "+Past" (17)))))))
     (62 "PERIOD" NIL (55 "." (55))))
   (trimtree c0 (topnodes c0 tree_s)))
   (lisp-unit:assert-equal
    '(235 "PROPP" NIL (13 "PROP" NIL (SNIP . 1)))
    (trimtree c5 (topnodes c5 tree_s)))))

(lisp-unit:define-test test-topnode
 (let* ((tab (open-and-import "dev/TEST_parse.pl"))
	(tree (maketree tab)))
   (lisp-unit:assert-equal
    '(34 "qePa-2746-3" (21))
    (topnodes (phi^-1 15 tab) tree))
   (lisp-unit:assert-equal
    '(144 "PROPP" NIL (2 "PROP" NIL (1 "abramsma" (1))))
    (topnodes (phi^-1 3 tab) tree))))

(lisp-unit:define-test test-preterms
  (let ((tree_s (maketree (open-and-import "dev/TEST_regargadj_s.pl"))))
    (lisp-unit:assert-equality
     #'set-equal
     '(52 50 48)
     (preterms (treefind 1077 tree_s)))))

(lisp-unit:define-test test-LL-splits
  (let ((s (make-instance 'LL-splits)))
    (add '( (10 . 6)(9 . 3) (11 . 9) (0 . 0)) 1077 s)
    (add '( (10 . 6)(9 . 3) (0 . 0) (11 . 9)) 1078 s)
    (lisp-unit:assert-equality
     #'set-equal
     '(1078 1077)
     (get-val '((9 . 3) (10 . 6) (11 . 9) (0 . 0)) s))))

(lisp-unit:define-test test-add-links
  (let* ((tab (open-and-import "dev/TEST_regargadj_s.pl"))
	 (tree (maketree tab))
	 (splits (make-instance 'LL-splits))
	 (f-alignment '((9 . 6) (10 . 3) (11 . 9) (0 . 0))))
    (add-links f-alignment tree tab splits 'src)
    (lisp-unit:assert-equality #'set-equal
     '(254 13 1225)
     (get-val '((9 . 6)) splits))
    (lisp-unit:assert-equality
     #'set-equal     
     '(52 479 482 483 1077 1805 50 48)
     (get-val '((10 . 3)) splits))
    (lisp-unit:assert-equality
     #'set-equal     
     '(995 56 475 474 473 64 62 60 58 445 66)
     (get-val '((11 . 9)) splits))
    (lisp-unit:assert-equality
     #'set-equal     
     '(1789 1792)
     (get-val '((10 . 3)(11 . 9)) splits))
    (lisp-unit:assert-equality
     #'set-equal     
     '(1815)
     (get-val '((10 . 3)(0 . 0)(11 . 9)) splits))
    (lisp-unit:assert-equality
     #'set-equal	   ; note: PERIOD, 81, is part of the above
     '(1141 1165 1817)	   ; c-nodes here, but unaligned, so no split!
     (get-val '((10 . 3)(0 . 0)(11 . 9)(9 . 6)) splits))))

(lisp-unit:define-test test-LL
  (let* ((tab_s (open-and-import "dev/TEST_regargadj_s.pl"))
	 ;; (tab_t (open-and-import "dev/TEST_regargadj_t.pl"))
	 (f-alignment '((0 . 0) (11 . 9) (10 . 3) (9 . 6))) ; flattened
	 (tree_s (maketree tab_s))
	 ;; (tree_t (maketree tab_t))
	 )
    (lisp-unit:assert-equality
     #'set-equal
     '((0 . 0) (11 . 9) (10 . 3) (9 . 6))
     (LL-single (car (topnodes (phi^-1 0 tab_s) tree_s)) f-alignment tree_s tab_s 'src))
    (lisp-unit:assert-equality
     #'set-equal
     '((0 . 0) (11 . 9) (10 . 3) (9 . 6))
     (LL-single 1817 f-alignment tree_s tab_s 'src))
    (lisp-unit:assert-equality
     #'set-equal
     '((0 . 0) (11 . 9) (10 . 3)) ; 1815 is the I', sister to PROPP which gives (9 . 6)
     (LL-single 1815 f-alignment tree_s tab_s 'src))))

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


;;;;;;;; DEPRECATED:
;;;;;;;; -----------
(defun find-multiple-unreferenced () "Fluff"
  (loop for i from 1 to 106
     for f = (concatenate 'string "nb/" (prin1-to-string i) ".pl")
     for unref = (unreferenced-preds (open-and-import f))
     when (not (equal '(0) unref)) do
     (format t "f:~A unref:~A~%" f unref)))

(defun outer>-LPT (Pr_s tab_s var_t tab_t LPTs)
  "Return a list of the outermost possible `LPTs' of `Pr_s' in `tab_t'
starting at `var_s'."
  (let ((Pr_t (get-pred var_t tab_t 'noerror)))
    (when (and Pr_s Pr_t)  
      (if (LPT? Pr_s tab_s Pr_t tab_t LPTs)
	  (list var_t)
	  (loop 
	     for c in (get-args Pr_t)
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



;;;;;;;; DEPRECATED BOTTOM UP-METHOD (might be useful for sanity tests)
;;;;;;;; --------------------------------------------------------------

(defun LPT-permute (all-LPT)
  "This quickly gets too slow to be usable. Even for just the longest
sublists, there are 40320 results for nb/6.pl vs ka/5.pl."
  (labels ((unseen (new seq)
	     (dolist (old seq t) (if (or (eq (car old) (car new))
					 (eq (cdr old) (cdr new)))
				     (return nil)))))
    (let ((pairs (mapcan (lambda (l)
			   (mapcar (lambda (var_t) (cons (car l) var_t)) (cdr l)))
			 all-LPT))
	  seqs)
      (loop for p in pairs
	 do (setq seqs (append
			seqs
			(mapcar-true (lambda (seq) (and (unseen p seq)
							(cons p seq)))
				     seqs)
			(list (list p))))
	 finally (return (longest-sublists seqs))))))

(lisp-unit:define-test test-LPT-permute
  (lisp-unit:assert-equality
   #'set-of-set-equal
   '(((0 . 1) (4 . 5))
     ((0 . 1) (4 . 6))
     ((0 . 2) (4 . 5))
     ((0 . 2) (4 . 6))
     ((0 . 3) (4 . 5))
     ((0 . 3) (4 . 6)))
   (LPT-permute '((0 1 2 3)
		  (4 5 6))))
  (let ((tab_s (open-and-import "dev/TEST_permute_s.pl"))
	(tab_t (open-and-import "dev/TEST_permute_t.pl")))
    (lisp-unit:assert-equality
     #'set-of-set-equal
     '(((5 . 0) (6 . 3))
       ((5 . 0) (0 . 3))
       ((0 . 0) (5 . 3))
       ((0 . 0) (6 . 3))
       ((6 . 0) (5 . 3))
       ((6 . 0) (0 . 3)))
     (LPT-permute (all-LPT-vars tab_s tab_t (make-LPT))))))

(defun try-f-align-all (tab_s tab_t &optional LPT)
  (mapcar (lambda (perm)
	    (try-f-align-perm perm tab_s tab_t))
	  (LPT-permute (all-LPT-vars tab_s tab_t (or LPT
						     (make-LPT))))))

(defun try-f-align-perm (perm tab_s tab_t)
  (when (loop for link in perm
	      always (try-f-align-link link tab_s tab_t perm))
    perm))

(defun try-f-align-link (link tab_s tab_t perm)
  " (i) the number of arguments n and m may or may not differ
is trivially true, while 
 (ii) there is LPT-correspondence between L(Pr_s) and L(Pr_t)
we already know is true because `perm' came from `LPT-permute'."
  (let* ((var_s (car link))
	 (var_t (cdr link))
	 (Pr_s (get-pred var_s tab_s))
	 (Pr_t (get-pred var_t tab_t))
	 (adjuncts_t (get-adjs var_t tab_t 'no-error))
	 (adjuncts_s (get-adjs var_s tab_s 'no-error))
	 (args_t (get-args Pr_t 'no-nulls))
	 (args_s (get-args Pr_s 'no-nulls)))
    (when *debug* (out "~A ~A~%" Pr_s Pr_t))
    (when
	(and
	 ;; these loops have overlapping responsibilities, TODO
	 (or 
	  (loop for c_s in args_s	      ; (iii)
		always (awhen (assoc c_s perm) ; this assumes <=1-1 PRED alignments
			      (or (member (cdr it) args_t)
				  (member (cdr it) adjuncts_t))))
	  (when *debug*
	    (out "iii, args_s: ~A, args_t: ~A, adjs_t: ~A~%" args_s args_t adjuncts_t)))	 
	 (or
	  (loop for c_t in args_t	       ; (iv)
		always (awhen (rassoc c_t perm) ; this assumes <=1-1 PRED alignments
			      (or (member (car it) args_s)
				  (member (car it) adjuncts_s))))
	  (when *debug*
	    (out "iv, args_t: ~A, args_s: ~A, adjs_s: ~A~%" args_t args_s adjuncts_s)))
					; TODO: (v) the LPT-correspondences can be aligned one-to-one
	 (or
	  (loop for adj_s in adjuncts_s	; (vi)
		always (aif (assoc adj_s perm) ; this assumes <=1-1 PRED alignments
			    (not (outer> (get-pred it tab_t)
					 Pr_t tab_t))
					; unaligned adjuncts are OK:
			    t))
	  (when *debug* (out "vi")))
	 (or
	  (loop for adj_t in adjuncts_t	; (vi) vice versa
		always (aif (rassoc adj_t perm) ; this assumes <=1-1 PRED alignments
			    (not (outer> (get-pred it tab_s)
					 Pr_s tab_s))
					; unaligned adjuncts are OK:
			    t))
	  (when *debug* (out "vi"))))
      link)))

(lisp-unit:define-test test-try-f-align-perm
 (let ((tab_s (open-and-import "dev/TEST_permute_s.pl"))
       (tab_t (open-and-import "dev/TEST_permute_t.pl")))
   (lisp-unit:assert-false
    (try-f-align-perm '((0 . 0) (5 . 3)) tab_s tab_t))))




;;;;;;;; DEPRECATED TOP-DOWN METHOD
;;;;;;;; --------------------------

(defun unseen (new seq)
  "Return true as long as neither (member new seq :key #'car) 
nor (member new seq :key #'cdr)."
  (dolist (old seq t) (if (or (eq (car old) (car new))
			      (eq (cdr old) (cdr new)))
			  (return nil))))

(defun expand (link news olds)
  (loop for old in olds
     when (unseen link old)
       append (mapcar (lambda (new) (append new old)) news)))

