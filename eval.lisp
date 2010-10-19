;;; This is just a scratch file to record which files work and note
;;; problems about them
(in-package #:lfgalign)

(defun evaluate (subdir n_s n_t &optional LPTs)
  (let*
      ((tab_s (open-and-import (format nil "eval/~A/ka/~A.pl" subdir n_s)))
       ;; In the thesis at least, ka is src, nb trg
       (tab_t (open-and-import (format nil "eval/~A/nb/~A.pl" subdir n_t)))
       (aligntab (make-aligntab))
       (LPTs (or LPTs (make-LPT)))
       (f-alignments (f-align '(0 . 0) tab_s tab_t LPTs))
       (best-f-alignment (rank f-alignments aligntab tab_s tab_t))
       (tree_s (maketree tab_s))
       (tree_t (maketree tab_t))
       (c-alignments (c-align-ranked best-f-alignment 
				     tree_s tab_s
				     tree_t tab_t)))
    (out "=================================~% ~A src: ka/~A.pl trg: nb/~A.pl~%"
	 subdir n_s n_t)
    (out "~A~% <=> ~A~%"
	 (gethash '|sentence| tab_s) (gethash '|sentence| tab_t))
    (out "~A~% ~A~%"
	 (f-tag-tree (skip-suff_base tree_s) tab_s)
	 (f-tag-tree (skip-suff_base tree_t) tab_t))
    (let ((allpairs (mapcan #'append (flatten f-alignments))))
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

(defun ev-all ()
  (evaluate "mrs" 0 0)
  (evaluate "mrs" 1 1)
  (evaluate "mrs" 2 2) ; merge; treng LPT. TODO: bør verb og pro
		       ; alltid vere LPT ved samanføying
  (evaluate "mrs" 3 3) ; hadde >1 solutions
  (evaluate "mrs" 4 4) ; treng LPT
  (evaluate "mrs" 5 6) ; treng LPT

  (evaluate "sofie" 2 0)

  ;; i 13-10 må ein leggje til (add-to-LPT "-ken" "inn" LPT) for at
  ;; ein i det heile teke skal få ut ei løysing som lenkjer desse to,
  ;; sidan det georgiske verbet tek tri arg (to er pro) medan det
  ;; norske berre tek eitt:
  (evaluate "sofie" 13 10) 

  ;; (evaluate "sofie" 3 1) ; >1 solutions
  ;; (evaluate "sofie" 4 2)
  ;; (evaluate "sofie" 5 3)
  ;; (evaluate "sofie" 6 4)
  ;; (evaluate "sofie" 7 5)
  ;; (evaluate "sofie" 8 6)
  ;; (evaluate "sofie" 9 7)
  ;; (evaluate "sofie" 21 18)
  ;; (evaluate "sofie" 32 32)
  ;; (evaluate "sofie" 41 46)
  )
