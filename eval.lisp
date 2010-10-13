;;; This is just a scratch file to record which files work and note
;;; problems about them
(in-package #:lfgalign)

(defun evaluate (n_s n_t)
  (let*
      ((tab_s (open-and-import (format nil "eval/ka/~A.pl" n_s)))
       ;; In the thesis at least, ka is src, nb trg
       (tab_t (open-and-import (format nil "eval/nb/~A.pl" n_t)))
       (tree_s (maketree tab_s))
       (tree_t (maketree tab_t))
       (LPTs (make-LPT))
       (aligntab (make-aligntab))
       (f-alignment (f-align '(0 . 0) tab_s tab_t LPTs))
       (flat-f ))
    (out "=================================~% src: ka/~A.pl trg: nb/~A.pl~%"
	 n_s n_t)
    (out "~A~% ~A~%"
	 (skip-suff_base tree_s)
	 (skip-suff_base tree_t))
    (let ((allpairs (mapcan #'append (flatten f-alignment))))
      (flet ((preds (getter tab)
	       (mapcar (lambda (var)
			 (get-pred var tab))
		       (remove-duplicates (mapcar getter allpairs)))))
	(out "~%srcs: ~A~%trgs: ~A~%"
	     (preds #'car tab_s) (preds #'cdr tab_t))))
    (out "~A~%" (pred-tag-alignment f-alignment tab_s tab_t))
    (out "~A~%" (rank f-alignment aligntab tab_s tab_t))))

(defun ev-all ()
  (evaluate 0 0)
  (evaluate 1 1)
  (evaluate 2 2)			; merge
  ;; (evaluate 3 3) ; >1 solutions
  (evaluate 4 4)
  )
