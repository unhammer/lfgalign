(defun align-out (path1 path2 &optional LPTs out-format)
  (let*
      ((tab_s (open-and-import path1))
       (tab_t (open-and-import path2))
       (LPTs (or LPTs (make-LPT)))
       (f-alignments (f-align '(0 . 0) tab_s tab_t LPTs))
       (best-f-alignment (rank f-alignments tab_s tab_t))
       (tree_s (maketree tab_s))
       (tree_t (maketree tab_t))
       (c-alignments (c-align-ranked best-f-alignment 
				     tree_s tab_s
				     tree_t tab_t)))
    (cond ((equal out-format "ria")
	   (loop for ((src . trg)) on best-f-alignment do (out "alignment( ~A, ~A).~%" src trg)))
	  ((equal out-format "verbose")
	   (out "=================================~% src: ka/~A.pl trg: nb/~A.pl~%"
		path1 path2)
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
		   c-alignments))
	  (t
	   (out "'(alignment~%")
	   (loop for ((src . trg)) on best-f-alignment do (out "   (f ~A ~A)~%" src trg))
	   (loop for ((src trg)) on c-alignments do (out "   (c ~A~%      ~A)~%" src trg))
	   (out " )~%")))))

(cond ((= (length sb-ext:*posix-argv*) 3)
       (align-out (nth 1 sb-ext:*posix-argv*)
		  (nth 2 sb-ext:*posix-argv*)))
      ((= (length sb-ext:*posix-argv*) 4)
       (align-out (nth 1 sb-ext:*posix-argv*)
		  (nth 2 sb-ext:*posix-argv*)
		  nil
		  (nth 3 sb-ext:*posix-argv*)))
      (t
       (progn (out "~%")
	      (out "Usage:~%$ ./align.sh source.pl target.pl [format]~%")
	      (out "~%")
	      (out "If format (optional) is \"ria\", output only f-structure links, in the RIA format~%")
	      (out "If format (optional) is \"verbose\", show names of predicates in links, print trees, etc.~%")
	      (out "(append 2>/dev/null to ignore warnings)~%")
	      (out "~%Arguments given: ~{~a ~}"  (cdr sb-ext:*posix-argv*)))))
