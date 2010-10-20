;;; quickstart:
; (swank:operate-on-system-for-emacs "lfgalign" (quote load-op)) (swank:set-package "LFGALIGN") (lisp-unit:run-tests)

;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

(define-condition several-topnodes (unexpected-input) ()
  (:report (lambda (condition stream)
	     (format stream "Found superfluous topmost nodes: ~A" (text condition)))))

(defvar *no-warnings* t)
(defvar *debug* nil)
(defvar *pro-affects-c-linking* t)

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

(defun f-tag-tree (node tab)
  "Debug function, just conses the phi(node) onto the node and 
any non-terminal child under it."
  (when node
    (if (terminal? node)
	node
      (list (phi (first node) tab)
	    (first node)
	    (second node)
	    (f-tag-tree (third node) tab)
	    (f-tag-tree (fourth node) tab)))))


(defun treefind (c-id tree)
  "Just return the subtree of `tree' starting with `c-id'."
  (if (and tree (listp tree))
      (if (eq (car tree) c-id)
	  tree
	  (or (treefind c-id (third tree))
	      (treefind c-id (fourth tree))))))

(defun topnodes (c-ids tree)
  "`c-ids' (given by `phi^-1') describes a functional domain, find the
topmost of the nodes in the c-structure which project this
domain (member of `c-ids'). 

Since we may have discontiguous domains, returns a list.

Unfortunately, id's aren't sorted in any smart way :-/"
  (when (and tree (listp tree))
    (if (member (car tree) c-ids)
	(list tree)
      (let ((Ltree (topnodes c-ids (third tree)))
	    (Rtree (topnodes c-ids (fourth tree))))
	(cond ((and Ltree Rtree)
	       (append Ltree Rtree))
	      ;; Not in both, but try either left or right:
	      (Ltree Ltree)
	      (Rtree Rtree)
	      ;; Not in either: return nil
	      )))))

(defun pred-tag-alignment (f-alignment tab_s tab_t)
  "Given an `f-alignment' from `f-align', exchange var numbers for the
names in the pred values. Also handles output from `flatten'/`rank'."
  (when f-alignment
      (if (f-link? f-alignment)
	  (cons (second (get-pred (car f-alignment) tab_s))
		(second (get-pred (cdr f-alignment) tab_t)))	  
	  (cons (pred-tag-alignment (car f-alignment) tab_s tab_t)
		(if (f-link? (first (cdr f-alignment)))
		    (mapcar (lambda (sub) ; flat
			      (pred-tag-alignment sub tab_s tab_t))
			    (cdr f-alignment))   
		    (mapcar (lambda (branch) ; branching
			      (mapcar (lambda (sub)
					(pred-tag-alignment sub tab_s tab_t))
				      branch))
			    (cdr f-alignment)))))))


(defun eq-phi (c-id1 c-id2 tab)
    "`c-id1' and `c-id2' are two c-structure id-s in `tab'. 
Return `nil' iff the phi of these are non-equal."
  (intersection (get-equivs (phi c-id1 tab) tab)
		(get-equivs (phi c-id2 tab) tab)))

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
  (mapcar #'skip-suff_base
	  (topnodes (phi^-1 f-var tab) tree)))


;;;;;;;; VARIOUS HELPERS:
;;;;;;;; ----------------

(defun null-pred? (Pr)
  (equal Pr "NULL"))

(defun get-equivs (val tab &optional include-this)
  (let ((equivs
	 (dset3-findall val (gethash '|eq-sets| tab))))
    (if include-this
	(adjoin val equivs)
      equivs)))

(defun eq-f (f-id1 f-id2 tab)
  "`f-id1' and `f-id2' are two f-structure id-s in `tab'. 
Return `nil' iff non-equal."
  (intersection (get-equivs f-id1 tab)
		(get-equivs f-id2 tab)))

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
			     (eq-f arg1 arg2 tab)))))
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

(defun get-pred (var tab &optional nil-on-none)
  "Use `nil-on-none' to return nil if no PRED was found."
  (if (null-pred? var)
      var
      (aif (unravel "PRED" var tab)
	   (cons var (cdr it))
	   (unless nil-on-none
	     (unless *no-warnings*
	       (warn "No PRED for var ~A, treating it as a pro~%" var))
	     (list var "pro" nil NIL NIL)))))





(defun predp (Pr)
  (and (listp Pr)
       (numberp (first Pr))
       (stringp (second Pr))
       (numberp (third Pr))
       (listp (fourth Pr))
       (listp (fifth Pr))
       (null (sixth Pr))
       (eq 5 (length Pr))))

(defun get-args (Pr tab &optional no-nulls)
  "The argument `Pr' is either a pred or a variable id that we can
look up to get a pred."
  (let ((Pr (if (predp Pr) Pr (get-pred Pr tab))))
    (if no-nulls
	(remove-if (lambda (p) (null-pred? p))
		   (union (fourth Pr) (fifth Pr)))
      (mapcar (lambda (var)
		(skip-pp var tab))
	      (union (fourth Pr) (fifth Pr))))))

(defun skip-pp (var tab)
  "Skip adpositions, as defined in footnote 3 in
dyvik2009lmp. 

TODO: do all prepositions/postpositions have PFORM/CHECK->_POSTP and
OBJ, or are there other ways of representing them? This really ought
to be user-configurable."
  (if (and (or (awhen (assoc-equal "CHECK" (gethash var tab))
		      (assoc-equal "_POSTP" (gethash (cdr it) tab)))
	       (assoc-equal "PFORM" (gethash var tab)))       
	   (assoc-equal "OBJ" (gethash var tab)))
      (cdr (assoc-equal "OBJ" (gethash var tab)))
    var))

(defun get-adjs (var tab &optional no-error)
  "Use `no-error' to return nil if no ADJUNCT was found.
TODO: find example to test where we need `unravel' / eq-sets.
TODO: Skip prepositions, as defined in footnote 3 in
dyvik2009lmp, with `skip-pp'."
  (let ((adjvar (assoc "ADJUNCT" (gethash var tab) :test #'equal)))
    (if adjvar
	(if (get-equivs (cdr adjvar) tab)
	    (error 'unexpected-input "eqvar of ADJUNCT, TODO")
	    (mapcar-true (lambda (pair) (when (eq (cdr pair) (cdr adjvar))
					  (skip-pp (car pair) tab)))
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
Note: a real \"pro\" argument will return its verb!  A fake \"pro\"
element (created by get-pred for var's that have no PRED element) will
return \"pro\"."
  (if (third Pr)
      (let* ((semform_id (third Pr))
	     (semform (assoc semform_id (gethash '|semform_data| tab)))
	     (preterminal (assoc (second semform)
				 (gethash '|subtree| tab)))
	     (terminal (assoc (fourth preterminal)
			      (gethash '|terminal| tab)))
	     (surfaceform (assoc (car (third terminal))
				 (gethash '|surfaceform| tab))))
	(second surfaceform))
    (second Pr)))


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
	 (args_s (get-args var_s tab_s 'no-nulls))
	 (args_t (get-args var_t tab_t 'no-nulls)))
    (when *debug* (format t "args_s:~A adjs_s:~A args_t:~A adjs_t:~A~%" args_s adjs_s args_t adjs_t))
    (argalign-p args_s adjs_s args_t adjs_t tab_s tab_t LPTs)))

(defun margalign (link1 link2 tab_s tab_t LPTs)
  "Like argalign, but `link1' and `link2' describe a two-to-one link, 
so we merge the adjunct/argument lists.

Note: this should work even for a two-to-two link.

Note: we assume here that this is never used to merge adjuncts, thus
we don't set-difference from adjs"
  (let* ((var_s1 (car link1))
	 (var_s2 (car link2))
	 (var_t1 (cdr link1))
	 (var_t2 (cdr link2))
	 (adjs_s (union (get-adjs var_s1 tab_s 'no-error)
			(get-adjs var_s2 tab_s 'no-error)))
	 (adjs_t (union (get-adjs var_t1 tab_t 'no-error)
			(get-adjs var_t2 tab_t 'no-error)))
	 (args_s (set-difference 
		  (union (get-args var_s1 tab_s 'no-nulls)
			 (get-args var_s2 tab_s 'no-nulls))
		  (union (get-equivs var_s1 tab_s 'include-this)
			 (get-equivs var_s2 tab_s 'include-this))))
	 (args_t (set-difference 
		  (union (get-args var_t1 tab_t 'no-nulls)
			 (get-args var_t2 tab_t 'no-nulls))
		  (union (get-equivs var_t1 tab_t 'include-this)
			 (get-equivs var_t2 tab_t 'include-this)))))
    (when *debug* (format t "args_s:~A adjs_s:~A args_t:~A adjs_t:~A~%" args_s adjs_s args_t adjs_t))
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

(defun adjalign (exclude link mlink tab_s tab_t LPTs)
  "Return all possible combinations of links between adjuncts that use
id's from `exclude' (a list of links).

Return nil if no alignments are possible.

Only returns pairs which are LPT, as defined by `LPTs'. See
`adjalign-p'.

Optional argument `mlink' is a merged link, which together with `link'
describes a two-to-one link, so we merge their adjunct lists; there is
no separate madjalign function."
  (let ((adjs_s (if mlink (union (get-adjs (car link) tab_s 'no-error)
				 (get-adjs (car mlink) tab_s 'no-error))
		  (get-adjs (car link) tab_s 'no-error)))
	(adjs_t (if mlink (union (get-adjs (cdr link) tab_t 'no-error)
				 (get-adjs (cdr mlink) tab_t 'no-error))
		  (get-adjs (cdr link) tab_t 'no-error))))
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
  argalign covers (iii) and (iv)
TODO: (v) the LPT-correspondences can be aligned one-to-one -- isn't this covered by the way we handle links? Need example..."
  (let* ((F_s (car link))
	 (F_t (cdr link))
	 (aligntab (or aligntab (make-aligntab)))
	 alignments)
    
    (flet ((sub-f (perm)
		  "Try to recursively align links in perm, but keep unaligned links"
		  (loop for link_a in perm
			do (unless (gethash link_a aligntab)
			     (setf (gethash link_a aligntab)
				   (f-align link_a tab_s tab_t LPTs aligntab)))
			collect (or (gethash link_a aligntab)
				    (or (unless *no-warnings* (warn "sub-f failed:~A" link_a))
					link_a))))
	   
	   (store (subalignment) (pushnew subalignment alignments :test #'equal)))
      
      (flet
	  ((argloop (argperms mlink)
	    "For each permutation of arg/adj links, store link + sub-alignment in `alignments'."
	    (if (equal argperms '(nil))
		;; '(nil) means no recursion on args needed, try adjs
		(loop for adjperm in (adjalign nil link mlink tab_s tab_t LPTs)
		      do (store (sub-f adjperm)))
	      ;; try recursion on argperms (if there are any)
	      (loop for argperm in argperms
		    for new = (if mlink
				  (cons mlink (sub-f argperm))
				(sub-f argperm))
		    do (store new)
		    ;; try to fill up adj alignments
		    (loop for adjperm in (adjalign argperm link mlink tab_s tab_t LPTs)
			  do (store (append (sub-f adjperm) new)))))
	    ; The caller will want to know whether arg-matching was even possible:
	    argperms))
	
	(when ; If there are no argperms/margperms, this `when' fails and nil is returned
	    (or
	     ;; Either one-to-one is possible, we don't try merging:
	     (argloop (argalign link tab_s tab_t LPTs)
		      nil)
	     ;; One-to-one is not possible, we try merging:
	     (let ((mlinks_s (loop for arg in (get-args F_s tab_s 'no-nulls)
				   collect (cons arg F_t)))
		   ;; TODO: merging both a_s AND a_t at once... should we?
		   (mlinks_t (loop for arg in (get-args F_t tab_t 'no-nulls)
				   collect (cons F_s arg))))
	       (when *debug* (out "One-to-one f-align failed; merging...~%"))
	       ;; This will return nil if none of the margaligns succeed:
	       (mapcar-true (lambda (mlink)
			      (when (or (not LPTs)
					(LPT? (get-pred (car mlink) tab_s) tab_s
					      (get-pred (cdr mlink) tab_t) tab_t LPTs))
				(argloop (margalign link mlink tab_s tab_t LPTs)
					 mlink)))
			    (append mlinks_s mlinks_t))))
	  (if alignments
	      ;; Return the full sub-alignment:
	      (cons link alignments)
	    ;; Either sub-f failed, or there were no args to align,
	    ;; check get-args F_t/F_s to find out:
	    link))))))

(lisp-unit:define-test test-merge
 (let ((tab_s (open-and-import "dev/TEST_merge_s.pl"))
       (tab_t (open-and-import "dev/TEST_merge_t.pl"))
       (LPT (make-LPT)))
   (lisp-unit:assert-equal
    '(((9 . 3)))
    (margalign '(0 . 0) '(10 . 0) tab_s tab_t LPT))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (10 . 0) (9 . 3))  ; perf-qePa, bjeffe-qePa, hund-jaGli (correct)
      ((0 . 0) (10 . 3) (9 . 0))) ; perf-qePa, bjeffe-jaGli, hund-qePa (wrong)
    (flatten (f-align '(0 . 0) tab_s tab_t LPT)))
   (add-to-lpt "bjeffe" "qePa" LPT)
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (10 . 0) (9 . 3))) ; perf-qePa, bjeffe-qePa, hund-jaGli
    (flatten (f-align '(0 . 0) tab_s tab_t LPT)))
   ;; Letting the merge happen on the target side:
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (0 . 10) (3 . 9))  ; qePa-perf, qePa-bjeffe, jaGli-hund (correct)
      ((0 . 0) (3 . 10) (0 . 9))) ; qePa-perf, jaGli-bjeffe, qePa-hund (wrong)
    (flatten (f-align '(0 . 0) tab_t tab_s LPT)))
   (add-to-lpt "qePa" "bjeffe" LPT)	; LPT is not symmetric, so we add again:
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (0 . 10) (3 . 9)))  ; qePa-perf, qePa-bjeffe, jaGli-hund (correct)
    (flatten (f-align '(0 . 0) tab_t tab_s LPT)))))

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
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((0 . 0) (8 . 2) (31 . 8))	; adjuncts optionally align
      ((0 . 0) (8 . 8) (31 . 2))
      ((0 . 0) (8 . 8)))
    (flatten (f-align '(0 . 0) tab_s tab_t LPT)))))


(defun f-link? (x)
  (and (consp x)
       (atom (car x))
       (atom (cdr x))))

(lisp-unit:define-test test-rank-sub-f
 ;; Should select the alignment where sub-f are really aligned
 (let* ((tab_s (open-and-import "dev/TEST_merge_s.pl"))
        (tab_t (open-and-import "dev/TEST_merge_t.pl"))
	(LPT (make-LPT))
	(aligntab (make-aligntab))
	(f-alignments (f-align '(0 . 0) tab_s tab_t LPT aligntab)))
   (lisp-unit:assert-equal ; make sure next test doesn't give false negative
    '((0 . 0) ((9 . 0) (10 . 3)) ((10 . 0) (9 . 3)))
    f-alignments)
   (lisp-unit:assert-equality
    #'set-equal
    '((0 . 0) (10 . 0) (9 . 3))	; perf-qePa, bjeffe-qePa, hund-jaGli (correct)
    (values (rank f-alignments aligntab tab_s tab_t)))))

(lisp-unit:define-test test-rank-argorder
 ;; Should select the alignment where argument orders match
 (let* ((tab_s (open-and-import "dev/TEST_regargadj_s.pl"))
        (tab_t (open-and-import "dev/TEST_regargadj_t.pl"))
	(LPT (make-LPT))
	(aligntab (make-aligntab))
	(f-alignments (f-align '(0 . 0) tab_s tab_t LPT aligntab)))
   (lisp-unit:assert-equality ; make sure next test doesn't give false negative
    #'set-of-set-equal
    '(((0 . 0) (11 . 6) (10 . 9) (9 . 3)) ((0 . 0) (11 . 6) (10 . 3) (9 . 9))
      ((0 . 0) (11 . 9) (10 . 6) (9 . 3)) ((0 . 0) (11 . 9) (10 . 3) (9 . 6))
      ((0 . 0) (11 . 3) (10 . 6) (9 . 9)) ((0 . 0) (11 . 3) (10 . 9) (9 . 6)))
    (flatten f-alignments))
   (lisp-unit:assert-equality
    #'set-equal
    '((0 . 0) (11 . 3) (10 . 9) (9 . 6))
    (values (rank f-alignments aligntab tab_s tab_t)))))

(lisp-unit:define-test test-rank-recursive
 (lisp-unit:assert-equality
  #'set-equal
  '((0 . 0) (46 . 30) (116 . 128))
  (values (rank-helper nil '((0 . 0) (((46 . 30) ((116 . 128))))))))
 (lisp-unit:assert-equality
  #'set-equal
  '((0 . 0) (verb . verb) (subj . subj) (obj . obj) (adv . adv))
  (values (rank-helper
	   nil '((0 . 0) (((verb . verb) ((subj . subj) (obj . obj))) (adv . adv))))))
  (lisp-unit:assert-equality
  #'set-equal
  '((0 . 0) (46 . 27) (10 . 30) (37 . 29) (2 . 28))
  (values (rank-helper
	   nil '((0 . 0) ((46 . 28) (10 . 29) (37 . 27) ((2 . 30) ((5 . 128))))
		 ((46 . 28) (10 . 29) (37 . 30) (2 . 27))
		 ((46 . 28) (10 . 27) (37 . 29) ((2 . 30) ((5 . 128))))
		 ((46 . 28) (10 . 27) (37 . 30) (2 . 29))
		 ((46 . 28) (10 . 30) (37 . 29) (2 . 27))
		 ((46 . 28) (10 . 30) (37 . 27) (2 . 29))
		 ((46 . 29) (10 . 28) (37 . 27) ((2 . 30) ((5 . 128))))
		 ((46 . 29) (10 . 28) (37 . 30) (2 . 27))
		 ((46 . 29) (10 . 27) (37 . 28) ((2 . 30) ((5 . 128))))
		 ((46 . 29) (10 . 27) (37 . 30) (2 . 28))
		 ((46 . 29) (10 . 30) (37 . 28) (2 . 27))
		 ((46 . 29) (10 . 30) (37 . 27) (2 . 28))
		 ((46 . 27) (10 . 28) (37 . 29) ((2 . 30) ((5 . 128))))
		 ((46 . 27) (10 . 28) (37 . 30) (2 . 29))
		 ((46 . 27) (10 . 29) (37 . 28) ((2 . 30) ((5 . 128))))
		 ((46 . 27) (10 . 29) (37 . 30) (2 . 28))
		 ((46 . 27) (10 . 30) (37 . 28) (2 . 29))
		 ((46 . 27) (10 . 30) (37 . 29) (2 . 28))
		 (((46 . 30) ((116 . 128))) (10 . 28) (37 . 29) (2 . 27))
		 (((46 . 30) ((116 . 128))) (10 . 28) (37 . 27) (2 . 29))
		 (((46 . 30) ((116 . 128))) (10 . 29) (37 . 28) (2 . 27))
		 (((46 . 30) ((116 . 128))) (10 . 29) (37 . 27) (2 . 28))
		 (((46 . 30) ((116 . 128))) (10 . 27) (37 . 28) (2 . 29))
		 (((46 . 30) ((116 . 128))) (10 . 27) (37 . 29) (2 . 28)))))))




(defun longest-sublists (lists)
  "Return the longest sublists in `lists'. Could do this in one loop,
but meh."
  (let ((maxlen (loop for l in lists maximize (length l))))
    (mapcar-true (lambda (l) (when (>= (length l) maxlen) l))
		 lists)))

(defun rank (f-alignments aligntab tab_s tab_t)
  "This could be done in a million different ways. For now, this is
the procedure: We start with input like '((0 . 0) branch1 branch2 ...)
where branch1 is e.g. '((9 . 0) (10 . 3)), 
or '((9 . 0) ((10 . 3) ((1 . 2) (7 . 8)) ((7 . 2) (1 . 8))))

`rank-helper' selects the best-ranking branch of branch1, branch2
etc. It gets a score for each branch from `rank-branch' which
calculates a score for (0 . 0) and a specific branch (set of
argument/adjunct links), multiplied with the ranks of each member of
that branch.

If there are several with equal rank, choose the first of the longest
branches (thus trying to align as many adjuncts as possible)."
  (rank-helper nil f-alignments aligntab tab_s tab_t))

(defun rank-helper (seen f-alignments &optional aligntab tab_s tab_t)
  (if (f-link? f-alignments)
      (values (list f-alignments)
	      1)
    (let* ((link (car f-alignments))
	   (outer-links (cons link seen)))
      ;; If (cdr f-alignments) were nil, this would return nil
      (loop with best-rate = 0 ; worst possible sub-f-score, all failed
	    with best-branches = nil
	    for branch in (cdr f-alignments)
	    do (setf (values newbranch
			     rate)
		     (rank-branch link outer-links branch aligntab tab_s tab_t))
	    if (= rate best-rate)
	    do (setq best-branches (cons newbranch best-branches))
	    else if (> rate best-rate)
	    do (setq best-rate rate
		     best-branches (cons newbranch nil))
	    finally
	    ;; for now, just return the first of the longest of the best..
	    ;; not sure what else to do when there's nothing left to rank on:
	    (return (values (cons link
				  (car (longest-sublists best-branches)))
			    best-rate))))))

(defun rank-branch (link seen branch &optional aligntab tab_s tab_t)
  "The individual branch score is the product of
- `sub-f-rate' which gives how many recursive sub-alignments were
  successful as opposed to simply having LPT-correspondence,
- `arg-order-rate' which gives how close a match there is between the
  argument orders of the src and trg pred's of the branch parent
  `link', and
- the weighted sum of the sub-alignment scores, achieved by recursive
  `rank-helper' calls

So we calculate pretty much the same as a PCFG: the score of this node
is multiplied with the sum of sub-node scores (weighted by branch
length, since we don't want the length of the argument list to play a
role here).

Returns both the ranked sub-alignments from `rank-helper' and the
score."
  ;; Sum over individual sub-alignments in the branch -- retrieved
  ;; recursively -- and weight by number of sub-alignments (arguments):
  (loop for f-alignment in branch
	with newsub
	with sub-rate
	do (setf (values newsub sub-rate)
	      (rank-helper seen f-alignment aligntab tab_s tab_t))
	appending newsub into subs
	summing sub-rate into sub-rate-sum
	finally
	(return
	 (values subs
		 ;; Multiply this branch with those from children:
		 (* (sub-f-rate seen branch tab_s tab_t)
		    (arg-order-rate link seen branch tab_s tab_t)
		    (/ sub-rate-sum (length subs)))))))

(defun argpos (var args tab)
  "Return the position of `var' in an argument list `args'. Also
handles equivalences."
  (loop for equiv in (get-equivs var tab 'include-this)
	thereis (position equiv args)))

(defun arg-order-rate (link seen branch &optional tab_s tab_t)
  "How close does the argument order of `branch' correspond with that
of src and trg in `link'? Return 1 if arguments are completely
aligned, if not: 
sum matches in argument position, weighted by length of linked arg/adj-list

Adjunct-argument links are counted as a non-match.

TODO: How should merges score here? For now, just bail out and return
1 if branch is a merge."
  (if (and tab_s tab_t)
      (let ((args_s (get-args (car link) tab_s 'no-nulls)) ; TODO: no-nulls OK?
	    (args_t (get-args (cdr link) tab_t 'no-nulls)))
	(loop for sub in branch
	      for pair = (if (f-link? sub)
			     sub
			   (first sub))
	      for src = (car pair)
	      for trg = (cdr pair)
	      for pos_s = (argpos src args_s tab_s)
	      for pos_t = (argpos trg args_t tab_t)
	      if (member-either pair seen)
	      do (return 1)		; this is a merge, bail out
	      end
	      summing (if (or
			   (and (not pos_s) ; trivial match: both are adjuncts
				(not pos_t))
			   (and pos_s ; both are arguments, and have the
				pos_t ; same position in the arg-structures
				(equal pos_s pos_t))) 
			  1
			0)
	      into matches
	      counting sub into total
	      finally (return (/ matches
				 total))))
    1))

(defun sub-f-rate (seen branch &optional tab_s tab_t)
  "How many of the alignments in this branch have sub-alignments, if
possible? If the links have no arguments, it's a trivial success;
otherwise it's a success if there are any sub-alignments at all.

TODO: differentiate between how many of the args were sub-aligned."
  (flet ((failed-sub (f-alignment)
	  (and (f-link? f-alignment)	; there are no sub-alignments
	       (not (member-either f-alignment seen)) ; not a merged link
	       tab_s ; if tables given, 
	       tab_t ; check if we could have sub-aligned but didn't:
	       (or (get-args (car f-alignment) tab_s 'no-nulls)
		   (get-args (cdr f-alignment) tab_t 'no-nulls)))))
    (loop for sub in branch
	  counting (not (failed-sub sub)) into sub-f
	  counting sub into total
	  finally (return (/ sub-f
			     total)))))

(defun spread (flatperm)
  "See `test-spread'."
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
  "`f-alignments' is a _member_ of an assoc-list, where the car is an `f-link?'.
See `test-flatten' for possible inputs and outputs."
  (when f-alignments
    (if (f-link? f-alignments)
	f-alignments
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
	  (list elt))))))

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
				        (get-equivs src tab 'include-this))))
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

(defmethod get-links (c-id (splits LL-splits))
  "Return the first key (list of links) that has val (a c-id) as a member of its values."
  (maphash (lambda (k v)
	     (when (member c-id v) (return-from get-links k)))
	   (LL-tab splits)))

(defun nodeless-args (f-id tab)
  "Return arguments of PRED with `f-id' that have no c-structure nodes.
TODO: could pro-arguments ever have pro-arguments of their own? 
Seems unlikely, but worth a footnote..."
   (remove-if (lambda (arg) (phi^-1 arg tab))
	      (get-args (get-pred f-id tab) tab 'no-nulls)))

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
  (labels ((get-links (f-id)
	     (let* ((getter (if (eq from 'trg)
				#'rassoc
			      #'assoc))
		    (f-ids (union
			    (get-equivs f-id tab 'include-this)
			    (when *pro-affects-c-linking*
			      (nodeless-args f-id tab))))
		    (links (mapcar-true (lambda (var) (funcall getter var f-alignment))
					f-ids)))
	       links)))
    (when tree
      (awhen (if (or (terminal? (third tree)) (terminal? (fourth tree)))
		 (if (and (third tree) (fourth tree))
		     (error "Unexpected branching in preterminal, TODO")
		   (awhen (get-links (phi (car tree) tab))
			  it))
		 (union (add-links f-alignment (third tree) tab splits from)
			(add-links f-alignment (fourth tree) tab splits from)))
	(add it (car tree) splits)
	it))))


(lisp-unit:define-test test-c-align-ranked ()
  (let* 
      ((tab_s (open-and-import "dev/TEST_subord-c-align_s.pl"))
       (tree_s (maketree tab_s))
       (tab_t (open-and-import "dev/TEST_subord-c-align_t.pl"))
       (tree_t (maketree tab_t))
       (f-alignment '((0 . 0) (7 . 14) (8 . 9))))
    ;; det åpnet seg --- gaiGo
    (lisp-unit:assert-equality
     #'set-of-set-equal
     '(((1236 1262 1338)		; just IP
	(186 42 183 180 178 161 157 2 156 4 155 6 154 8 153 10 152 12 150 16)))
     (c-align-ranked f-alignment tree_s tab_s tree_t tab_t)))
  (let* 
      ((tab_s (open-and-import "dev/TEST_subord-c-align_s.pl"))
       (tree_s (maketree tab_s))
       (tab_t (open-and-import "dev/TEST_subord-c-align_t2.pl"))
       (tree_t (maketree tab_t))
       (f-alignment '((0 . 0)(7 . 12)(8 . 3))))
    ;; det åpnet seg --- PanJara gaiGo
    (lisp-unit:assert-equality
     #'set-of-set-equal
     '(((1236 1262 1338)		; IP
	(662 659 595))
       ((1461)				; Ibar
	(58 592 487 482 18 481 20 480 22 479 24 478 26 477 28 475 32))
       ((1353 1477 25)			; det/PanJara
	(331 321 318 6 168 9 166 10 164 12)))
     (c-align-ranked f-alignment tree_s tab_s tree_t tab_t))))

(defun hash-key-intersect (tab1 tab2)
  (loop for k being the hash-keys of tab1
	when (gethash k tab2) collect k))

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
	 (list (get-val links splits_s)
	       (get-val links splits_t)))
       linkable))))

(defun align (tab_s tab_t LPT)
  (let* ((aligntab (make-aligntab))
	 ; TODO: instead of (0 . 0), the topmost LPT-correspondents;
	 ; they may have to be merged too
	 (f-alignments (f-align '(0 . 0) tab_s tab_t LPT aligntab))
	 (best-f-alignment (rank f-alignments aligntab tab_s tab_t))
	 (tree_s (maketree tab_s))
	 (tree_t (maketree tab_t))
	 (c-alignments (c-align-ranked best-f-alignment 
				       tree_s tab_s
				       tree_t tab_t)))
    ;; TODO: pretty-print
    (out "ALIGN f: ~A~%" best-f-alignment)
    (mapcar (lambda (pair)
	      (out "ALIGN c_s: ~A~%      c_t: ~A~%"
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (car pair) tree)))
			   (topnodes (car pair) tree_s))
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (cadr pair) tree)))
			   (topnodes (cadr pair) tree_t))))
	    c-alignments)
    (list best-f-alignment c-alignments)))

(defun foo ()
  (let ((tab_s (open-and-import "eval/ka/0.pl"))
	(tab_t (open-and-import "eval/nb/0.pl"))
	(LPT (make-LPT)))
    (align tab_s tab_t LPT)))

(defun f-align-naive (var1 tab1 var2 tab2)
  "Just to show the difference between naive alignment, and the complete alignment.
`var1' and `var2' are f-structure id's in `tab1' and `tab2'
respectively.  TODO: cache/memoise maketree"
  (let* ((pred1 (get-pred var1 tab1))
	 (pred2 (get-pred var2 tab2)))
    (format t "Align ~A with ~A~%" pred1 pred2)
    (unless (or (null-pred? pred1) (null-pred? pred2))
      (format t "Align tree(s) ~A~%" (pretty-topnode var1 tab1 (maketree tab1)))
      (format t " with tree(s) ~A~%" (pretty-topnode var2 tab2 (maketree tab2)))
      (when (and pred1 pred2)
      (loop
	 for arg1 in (get-args pred1 tab1)
	 for arg2 in (get-args pred2 tab2)
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
    '(((30 . 46) (27 . 10) (29 . 37) (28 . 5))
      ((30 . 46) (27 . 10) (29 . 5) (28 . 37))
      ((30 . 46) (27 . 37) (29 . 10) (28 . 5))
      ((30 . 46) (27 . 37) (29 . 5) (28 . 10))
      ((30 . 46) (27 . 5) (29 . 10) (28 . 37))
      ((30 . 46) (27 . 5) (29 . 37) (28 . 10))
      ((30 . 10) (27 . 46) (29 . 37) (28 . 5))
      ((30 . 10) (27 . 46) (29 . 5) (28 . 37))
      ((30 . 10) (27 . 37) (29 . 46) (28 . 5))
      ((30 . 10) (27 . 37) (29 . 5) (28 . 46))
      ((30 . 10) (27 . 5) (29 . 46) (28 . 37))
      ((30 . 10) (27 . 5) (29 . 37) (28 . 46))
      ((30 . 37) (27 . 46) (29 . 10) (28 . 5))
      ((30 . 37) (27 . 46) (29 . 5) (28 . 10))
      ((30 . 37) (27 . 10) (29 . 46) (28 . 5))
      ((30 . 37) (27 . 10) (29 . 5) (28 . 46))
      ((30 . 37) (27 . 5) (29 . 46) (28 . 10))
      ((30 . 37) (27 . 5) (29 . 10) (28 . 46))
      ((30 . 5) (27 . 46) (29 . 10) (28 . 37))
      ((30 . 5) (27 . 46) (29 . 37) (28 . 10))
      ((30 . 5) (27 . 10) (29 . 46) (28 . 37))
      ((30 . 5) (27 . 10) (29 . 37) (28 . 46))
      ((30 . 5) (27 . 37) (29 . 46) (28 . 10))
      ((30 . 5) (27 . 37) (29 . 10) (28 . 46)))
    (argalign '(0 . 0) tab_s tab_t (make-LPT)))
   (lisp-unit:assert-equality
    #'set-of-set-equal
    '(((30 . 46) (27 . 10) (29 . 37) (28 . 5))
      ((30 . 46) (27 . 10) (29 . 5) (28 . 37))
      ((30 . 46) (27 . 37) (29 . 10) (28 . 5))
      ((30 . 46) (27 . 37) (29 . 5) (28 . 10))
      ((30 . 46) (27 . 5) (29 . 10) (28 . 37))
      ((30 . 46) (27 . 5) (29 . 37) (28 . 10)))
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
				   (((19 . 46) (A . B))
				    ((19 . 46) (A . C)))
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
  (lisp-unit:assert-equal '(e . f) (flatten '(e . f)))
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
   (trimtree c0 (first (topnodes c0 tree_s))))
   (lisp-unit:assert-equal
    '(235 "PROPP" NIL (13 "PROP" NIL (SNIP . 1)))
    (trimtree c5 (first (topnodes c5 tree_s))))))

(lisp-unit:define-test test-topnode
 (let* ((tab (open-and-import "dev/TEST_parse.pl"))
	(tree (maketree tab)))
   (lisp-unit:assert-equal
    '(34 "qePa-2746-3" (21))
    (first (topnodes (phi^-1 15 tab) tree)))
   (lisp-unit:assert-equal
    '(144 "PROPP" NIL (2 "PROP" NIL (1 "abramsma" (1))))
    (first (topnodes (phi^-1 3 tab) tree)))))

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
     (get-val '((10 . 3)(0 . 0)(11 . 9)(9 . 6)) splits)))
  (let* ((tab (open-and-import "dev/TEST_subord-c-align_s.pl"))
	 (tree (maketree tab))
	 (splits (make-instance 'LL-splits))
	 (f-alignment '((0 . 0))))
    (add-links f-alignment tree tab splits 'src)
    (lisp-unit:assert-equality #'set-equal
    '(1236 104 1262 1338 1461 1435 735 44 734 46 733 48)
     (get-val '((0 . 0)) splits))))

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
     (LL-single (car (first (topnodes (phi^-1 0 tab_s) tree_s)))
		f-alignment tree_s tab_s 'src))
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

(defun find-multiple-unreferenced () "Fluff"
  (loop for i from 1 to 106
	for f = (concatenate 'string "nb/" (prin1-to-string i) ".pl")
	for unref = (unreferenced-preds (open-and-import f))
	when (not (equal '(0) unref)) do
	(format t "f:~A unref:~A~%" f unref)))


(defun outer> (Pr1 Pr2 tab) 
  "Is `Pr1' a predecessor of `Pr2' (outermore in the f-structure)?
Note: we actually only look at the car of `Pr2', which has to be its
variable (ie. what `get-pred' returns)."
  (let ((args (get-args Pr1 tab)))
    (or (member (car Pr2) args)
	(loop for c in args
	   for Prc = (unravel "PRED" c tab)
	   thereis (and Prc
			(not (null-pred? c))
			(outer> Prc Pr2 tab))))))

(defun outer>-LPT (Pr_s tab_s var_t tab_t LPTs)
  "Return a list of the outermost possible `LPTs' of `Pr_s' in `tab_t'
starting at `var_s'."
  (let ((Pr_t (get-pred var_t tab_t 'nil-on-none)))
    (when (and Pr_s Pr_t)  
      (if (LPT? Pr_s tab_s Pr_t tab_t LPTs)
	  (list var_t)
	(loop 
	 for c in (get-args Pr_t tab_t)
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
     for Pr_s = (get-pred var_s tab_s 'nil-on-none)
     collect (cons var_s
		   (loop 
		      for var_t in (unreferenced-preds tab_t) 
		      for o = (outer>-LPT Pr_s tab_s var_t tab_t LPTs)
		      when o append it))))


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
