;;; This is just a scratch file to record which files work and note
;;; problems about them
(in-package #:lfgalign)

(defun evaluate (path_s path_t &optional LPTs absolute)
  (let*
      ((tab_s (open-and-import path_s absolute))
       (tab_t (open-and-import path_t absolute))
       (aligntab (make-aligntab))
       (LPTs (or LPTs (make-LPT)))
       (f-alignments (f-align '(-1 . -1) tab_s tab_t LPTs))
       (best-f-alignment (rank f-alignments aligntab tab_s tab_t))
       (tree_s (maketree tab_s))
       (tree_t (maketree tab_t))
       (c-alignments (c-align-ranked best-f-alignment 
				     tree_s tab_s
				     tree_t tab_t)))
    (out "=================================~% (evaluate \"~A\" \"~A\")~%"
	 path_s path_t)
    (out "~A~% <=> ~A~%"
	 (gethash '|sentence| tab_s) (gethash '|sentence| tab_t))
    (out "~A~% ~A~%"
	 (f-tag-tree (skip-suff_base tree_s) tab_s)
	 (f-tag-tree (skip-suff_base tree_t) tab_t))
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
    (out "ranked: ~A~%" (pred-tag-alignment best-f-alignment tab_s tab_t))
    (mapcar (lambda (pair)
	      (out "ALIGN c_s: ~A~%      c_t: ~A~%"
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (car pair) tree)))
			   (topnodes (car pair) tree_s))
		   (mapcar (lambda (tree)
			     (skip-suff_base (trimtree (cadr pair) tree)))
			   (topnodes (cadr pair) tree_t))))
	    c-alignments)))

(defun ev-ka-nb (subdir n_s n_t)
  (evaluate (format nil "eval/~A/ka/~A.pl" subdir n_s)
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
  (ev-ka-nb "mrs" 71 72)	  ; TODO: koordinering fungerer ikkje!
  ; (ev-ka-nb "mrs" 73 74)	  ; TODO: koordinering fungerer ikkje!
  ;; ^^^ krasjer i tillegg pred-tag-alignment...

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

  (evaluate "eval/sulis/de/D0000.pl" "eval/sulis/en/E0000.pl"))


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
		   :directory
		   (append (append dir '("en"))))
       
     do
       (let*
	   ((tab_s (open-and-import (format nil "~A" path_s) 'absolute))
	    (tab_t (open-and-import (format nil "~A" path_t) 'absolute)))
	 (out "~A: ~A og ~A~%" path_s
	      (length (unreferenced-preds tab_s))
	      (length (unreferenced-preds tab_t)))
	 (when nil
	   (let*
	       ((aligntab (make-aligntab))
		(LPTs (make-LPT))
		(f-alignments (f-align '(-1 . -1) tab_s tab_t LPTs))
		(best-f-alignment (rank f-alignments aligntab tab_s tab_t))
		(tree_s (maketree tab_s))
		(tree_t (maketree tab_t))
		(c-alignments (c-align-ranked best-f-alignment 
					      tree_s tab_s
					      tree_t tab_t)))
	     ;; (out "=================================~% src: ~A trg: ~A~%"
	     ;;      path_s path_t)
	     (out "=================================~%~A~% <=> ~A~%"
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
