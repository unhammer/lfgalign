;;; This is just a scratch file to record which files work and note
;;; problems about them
(in-package #:lfgalign)


(defun ev-align-print (path_s path_t &optional LPTs absolute)
  (let*
      ((tab_s (open-and-import path_s absolute))
       (tab_t (open-and-import path_t absolute))
       (LPTs (or LPTs (make-LPT)))
       (f-alignments (f-align '(-1 . -1) tab_s tab_t LPTs))
       (best-f-alignment (rank f-alignments tab_s tab_t LPTs))
       (tree_s (maketree tab_s))
       (tree_t (maketree tab_t))
       (c-alignments (c-align-ranked best-f-alignment 
				     tree_s tab_s
				     tree_t tab_t)))
    (out "=================================~% (ev-align-print \"~A\" \"~A\")~%"
	 path_s path_t)
    (out "~A <=> ~A~%~%"
	 (gethash '|sentence| tab_s) (gethash '|sentence| tab_t))
    (out-two-f-str tab_s tab_t)
    (out "unranked: ~A~%" f-alignments)
    (out "ranked: ~A~%~%" best-f-alignment)
    (out "~A~% ~A~%"
	 (f-tag-tree (skip-suff_base tree_s) tab_s)
	 (f-tag-tree (skip-suff_base tree_t) tab_t))
    (mapcar (lambda (pair)
	      (out "ALIGN c_s: ~A~%      c_t: ~A~%"
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (car pair) tree)))
			   (topnodes (car pair) tree_s))
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (cadr pair) tree)))
			   (topnodes (cadr pair) tree_t))))
	    c-alignments)))


;;;;;;;; MANUAL TEST SET (mrs, sofie)
;;;;;;;; ----------------------------

(defun ev-ka-nb (subdir n_s n_t)
  (ev-align-print (format nil "eval/~A/ka/~A.pl" subdir n_s)
		  (format nil "eval/~A/nb/~A.pl" subdir n_t)))

(defun ev-all ()
  (ev-ka-nb "mrs" 0 0)
  (ev-ka-nb "mrs" 1 1)
  (ev-ka-nb "mrs" 2 2) ; merge; treng LPT. TODO: bør verb og pro
		       ; alltid vere LPT ved samanføying
  (ev-ka-nb "mrs" 3 3) ; hadde >1 solutions
  (ev-ka-nb "mrs" 4 4) ; treng LPT
  (ev-ka-nb "mrs" 4 5) ; treng LPT
  (ev-ka-nb "mrs" 5 6) ; treng LPT
  (ev-ka-nb "mrs" 6 7)
  (ev-ka-nb "mrs" 7 8)
  (ev-ka-nb "mrs" 9 10)
  (ev-ka-nb "mrs" 10 11)
  (ev-ka-nb "mrs" 11 12)
  (ev-ka-nb "mrs" 16 17)
  (ev-ka-nb "mrs" 19 20)
  (ev-ka-nb "mrs" 22 23)
  (ev-ka-nb "mrs" 23 24)
  (ev-ka-nb "mrs" 24 25)
  (ev-ka-nb "mrs" 25 26)
  (ev-ka-nb "mrs" 26 27)
  (ev-ka-nb "mrs" 34 35)
  (ev-ka-nb "mrs" 37 38)
  (ev-ka-nb "mrs" 38 39)
  (ev-ka-nb "mrs" 57 58)
  (ev-ka-nb "mrs" 63 64)
  (ev-ka-nb "mrs" 67 68)
  (ev-ka-nb "mrs" 71 72)		; coord
  (ev-ka-nb "mrs" 73 74)		; coord

  (ev-ka-nb "sofie" 2 0)		; for stor avstand i argumentstruktur: være på vei hjem <=> bruneba

  ;; i 13-10 må ein leggje til (add-to-LPT "-ken" "inn" LPT) for at
  ;; ein i det heile teke skal få ut ei løysing som lenkjer desse to,
  ;; sidan det georgiske verbet tek tri arg (to er pro) medan det
  ;; norske berre tek eitt:
  (ev-ka-nb "sofie" 13 10) 

  ;; (ev-ka-nb "sofie" 3 1) ; >1 solutions
  ;; (ev-ka-nb "sofie" 4 2)
  ;; (ev-ka-nb "sofie" 5 3)
  ;; (ev-ka-nb "sofie" 6 4)
  ;; (ev-ka-nb "sofie" 7 5)
  ;; (ev-ka-nb "sofie" 8 6)
  ;; (ev-ka-nb "sofie" 9 7)
  ;; (ev-ka-nb "sofie" 21 18)
  ;; (ev-ka-nb "sofie" 32 32)
  ;; (ev-ka-nb "sofie" 41 46)

  (ev-align-print "eval/sulis/de/D0000.pl" "eval/sulis/en/E0000.pl"))


(defun ev-mrs ()
  (with-open-file (stream
		   (make-pathname
		    :name "gold"
		    :type "lisp"
		    :directory
		    (append (pathname-directory
			     (asdf:component-pathname (asdf:find-system :lfgalign)))
			    '("eval" "mrs"))))
    	      
    (loop for (srcfile trgfile gold) = (read stream nil)
       with lpt = (make-lpt)
       with pass = 0
       with fail = 0
       with dir = (append (pathname-directory
			   (asdf:component-pathname (asdf:find-system :lfgalign)))
			  '("eval" "mrs"))
       while gold
       do
       (let* ((path_s (make-pathname :name srcfile :type "pl"
				     :directory (append dir '("ka"))))
	      (path_t (make-pathname :name trgfile :type "pl"
				     :directory (append dir '("nb"))))
	      (result (align (open-and-import (format nil "~A" path_s) 'absolute)
			     (open-and-import (format nil "~A" path_t) 'absolute)
			     lpt)))
	 ;; If the gold test does not define a c-structure, don't test
	 ;; for c-structure:
	 (unless (cdr gold) (setf (cdr result) nil))
	 (if (and (set-equal (first gold) (first result))
		  (set-equal (second gold) (second result))) (incf pass)
	     (progn (incf fail)
		    (out "FAIL: ~A <=> ~A~%" srcfile trgfile))))
       finally (return (list pass '/ (+ pass fail))))))



;;;;;;;; EUROPARL WITH C-STRUCTURES
;;;;;;;; --------------------------

(defun ev-europarl (n)
  (loop
     repeat n
     with dir = (append (pathname-directory
			 (asdf:component-pathname (asdf:find-system :lfgalign)))
			'("eval" "europarl"))
     for path_s in (directory
		    (make-pathname
		     :name :wild
		     :type "pl"
		     :directory (append dir '("de"))))
     for path_t = (make-pathname
		   :name (pathname-name path_s)
		   :type "pl"
		   :directory (append dir '("en")))
       
     do
       (let*
	   ((tab_s (open-and-import (format nil "~A" path_s) 'absolute))
	    (tab_t (open-and-import (format nil "~A" path_t) 'absolute)))
	 (when t
	   (let*
	       ((LPTs (make-LPT))
		(f-alignments (f-align '(-1 . -1) tab_s tab_t LPTs))
		(best-f-alignment (rank f-alignments tab_s tab_t LPTs))
		(tree_s (maketree tab_s))
		(tree_t (maketree tab_t))
		(c-alignments (c-align-ranked best-f-alignment 
					      tree_s tab_s
					      tree_t tab_t)))
	     (out "=================================~% (ev-align-print \"~A\" \"~A\")~%"
		  path_s path_t)
	     (out "~A~% <=> ~A~%"
		  (gethash '|sentence| tab_s) (gethash '|sentence| tab_t))
	     (let ((allpairs
		    (if (consp (cdr f-alignments))
			;; Strangely, mapcan #'append hangs if the list is long enough:
			(loop for l in (flatten f-alignments) append l)
			(list f-alignments))))
	       (flet ((preds (getter tab)
			(mapcar (lambda (var)
				  (get-pred var tab))
				(remove-duplicates (mapcar getter allpairs)))))
		 (out "~%srcs: ~A~%trgs: ~A~%"
		      (preds #'car tab_s) (preds #'cdr tab_t))))
	     (out "unranked: ~A~%" (pred-tag-alignment f-alignments tab_s tab_t))
	     (out "ranked: ~A~%" (pred-tag-alignment best-f-alignment tab_s tab_t)))))

))




;;;;;;;; FOR COMPARISON WITH RIA
;;;;;;;; -----------------------

(defun random-f-align (link tab_s tab_t LPT)
  "Create a single random f-alignment (no merges of course), output
like `flatten' or `rank'"
  (flet ((shuffle (list)
	   (loop for item below (length list) do
		(rotatef
		 (elt list item)
		 (elt list (random (length list)))))
	   list))
    (loop for src in (all-pred-vars tab_s)
       for trg in (shuffle (all-pred-vars tab_t))
       collect (cons src trg))))

(defun random-rank (f-alignments tab_s tab_t LPT)
  f-alignments)


(defun ding-LPT ()
  (let ((LPT (make-LPT)))
    (with-open-file
	(stream (make-pathname 
		 :name "dev/de-en-LPT.pl"
		 :directory 
		 (pathname-directory
		  (asdf:component-pathname (asdf:find-system :lfgalign)))))
      (loop 
	 for c = (parse-pred stream)
	 while c
	 do
	   (add-to-LPT (clean-car/var (third c))
		       (clean-car/var (fourth c))
		       LPT)))
    LPT))


(defun ria-analyses (&optional n (max-frag 0))
  "Return paths to analyses in the RIA-testset. Defaults to filtering
out analyses that are have no `unreferenced-preds' (neither source nor
target). Optional argument `n' is a maximum of analyses to
return (returning all takes half a minute). Optional argument
`max-frag' may be used to increase the maximum allowed
`unreferenced-preds' (use nil for no maximum).

TODO: There are 2527 sentence pairs in RIA that do not have 'Fragment'
any where in their analyses -- why do so many of these have
unreferenced-preds then?"
  (loop for set in '("sents_0000" "sents_0001" "sents_0002" "sents_0003")
       with i = 0
       append
       (loop
	  while (or (not n) (< i n))
	  with dir = (append (pathname-directory
			      (asdf:component-pathname (asdf:find-system :lfgalign)))
			     '("eval" "ria"))
	  for path_s in (directory
			 (make-pathname
			  :name :wild
			  :type "pl"
			  :directory (append dir (list "sl_train" set))))
	  for path_t = (make-pathname
			:name (pathname-name path_s)
			:type "pl"
			:directory (append dir (list "tl_train" set)))
          for path_a = (make-pathname
			:name (substitute #\A #\S (pathname-name path_s))
			:type "pl"
			:directory (append dir (list "alignments" set)))

	  for tab_s = (open-and-import (format nil "~A" path_s) 'absolute)
	  for tab_t = (open-and-import (format nil "~A" path_t) 'absolute)
	  for unref_s = (remove-if (lambda (v) (eq 0 v)) (unreferenced-preds tab_s))
	  for unref_t = (remove-if (lambda (v) (eq 0 v)) (unreferenced-preds tab_t))
	  do (incf i)
	  when (or (not max-frag)
		   (and (<= (length unref_s) max-frag)
			(<= (length unref_t) max-frag)))
	  collect (list path_s path_t path_a)
	  end)))


(defun ev-ria (sentpairs &optional (aligner #'f-align) (ranker #'rank) LPT)
  "Measure overlap between f-links in the RIA
testsets (ria/data/sents_000{0,1,2,3}) and those given by `f-align'
and `rank', or the random baseline `random-f-align' and `random-rank'.

RIA, with the necessary testsets, is available from
http://www.computing.dcu.ie/~ygraham/software.html This function
assumes there is a symlink \"ria\" from the \"eval\" folder to the
\"data\" folder in RIA."
  (loop
     with LPT = (or LPT (make-LPT))
     for ((path_s path_t path_a)) on sentpairs
     for ria-alignment = (with-open-file
			     (stream path_a)
			   (loop for c = (parse-pred stream)
			      while c
			      for link = (cons (parse-integer (car (third c)))
					       (parse-integer (car (fourth c))))
			      collect link))
     for tab_s = (open-and-import (format nil "~A" path_s) 'absolute)
     for tab_t = (open-and-import (format nil "~A" path_t) 'absolute)
     for f-alignments = (funcall aligner '(-1 . -1) tab_s tab_t LPT)
     for best-f-alignment = (remove-if
			     (lambda (link) (equal '(-1 . -1) link))
			     (funcall ranker f-alignments tab_s tab_t LPT))

     for l_best = (length best-f-alignment)
     for l_ria = (length ria-alignment)
     for l_pred_s = (length (all-pred-vars tab_s)) 
     for l_pred_t = (length (all-pred-vars tab_t))
     for isect = (length (intersection ria-alignment
				       best-f-alignment
				       :test #'equal))
     for union = (length (union ria-alignment
				best-f-alignment
				:test #'equal))
     summing isect into isects
     summing union into unions
     summing l_best into ls_best
     summing l_ria into ls_ria
     summing (* l_pred_s l_pred_t) into possible_links
     summing l_pred_s into possible_srcs
     summing (length (unreferenced-preds tab_s)) into unref_s
     summing (length (unreferenced-preds tab_t)) into unref_t

     counting path_s into i
     ;; do (out ".") when (eq 0 (mod i 10)) do (out " ~A~%" path_s) end
     when nil do
       (out "~A~% <=> ~A~%"
	    (gethash '|sentence| tab_s) (gethash '|sentence| tab_t))
       (out-two-f-str tab_s tab_t)
       (out "~A~%~%" best-f-alignment)
     end
     finally (out "~%Intersections: ~A~%Unions: ~A~%links made by ~A: ~A~%links in RIA: ~A~%Linkable source PRED's: ~A~%Link possibilities (linkable srcs * linkable trgs): ~A~%Unreferenced sources: ~A~%Unreferenced targets: ~A~%"
		  isects unions aligner ls_best ls_ria possible_srcs possible_links
		  unref_s unref_t)
     (return (list isects unions ls_best ls_ria possible_srcs possible_links unref_s unref_t))
     ))



(lisp-unit:define-test test-ria
  (let ((*arg-order-smoothing* 0.01)
	(*sub-f-smoothing* 0.01)
	(*lpt-smoothing* 0.00))
    ;; Mostly just to show how to use ev-ria and ria-analyses
    (lisp-unit:assert-equal
     '(16 195 100 112 190 1716 82 66)
     (ev-ria (ria-analyses 20 nil)))
    (lisp-unit:assert-equal
     '(16 164 85 96 157 1341 67 52)
     (ev-ria (ria-analyses 20 5)))
    (lisp-unit:assert-true
     (ev-ria (ria-analyses 20 2) #'random-f-align #'random-rank))))



(defun foo ()
  (let ((lpt (ding-lpt)))
    (let ((sents  (ria-analyses nil 0)))
      (ev-ria sents   #'random-f-align #'random-rank nil)
      (ev-ria sents   #'f-align        #'rank        nil)
      (ev-ria sents   #'f-align        #'rank        lpt))
    (let ((sents (ria-analyses nil 1)))
      (ev-ria sents   #'random-f-align #'random-rank nil)
      (ev-ria sents   #'f-align        #'rank        nil)
      (ev-ria sents   #'f-align        #'rank        lpt))
    (let ((sents (ria-analyses nil 2)))
      (ev-ria sents   #'random-f-align #'random-rank nil)
      (ev-ria sents   #'f-align        #'rank        nil)
      (ev-ria sents   #'f-align        #'rank        lpt))
    (let ((sents (ria-analyses nil 3)))
      (ev-ria sents   #'random-f-align #'random-rank nil)
      (ev-ria sents   #'f-align        #'rank        nil)
      (ev-ria sents   #'f-align        #'rank        lpt))
    (let ((sents (ria-analyses nil nil)))
      (ev-ria sents   #'random-f-align #'random-rank nil)
      (ev-ria sents   #'f-align        #'rank        nil)
      (ev-ria sents   #'f-align        #'rank        lpt))
    ))