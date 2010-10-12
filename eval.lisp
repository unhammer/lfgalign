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
       (aligntab (make-aligntab)))
    (out "~%f-tagged ~A and ~A~%~A~% ~A~%"
	 n_s n_t
	 (f-tag-tree (skip-suff_base tree_s) tab_s)
	 (f-tag-tree (skip-suff_base tree_t) tab_t))
    (out "~A~%" (f-align '(0 . 0) tab_s tab_t LPTs))))

(defun ev-all ()
  (evaluate 1 1)
  (evaluate 2 2)			; merge
  ;; (evaluate 3 3) ; >1 solutions
  (evaluate 4 4)
  )
