;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

;;; Giza++ gives us the LPT-correspondence (criterion i), our Prolog
;;; files should have the information to use criterion ii.

(defparameter f-parse
  nil
  "The f-parse of the Prolog file.")
(defparameter c-parse
  nil
  "The c-parse of the Prolog file.")

;; (list ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'PRED'")) ("var" ("1"))))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'SUBJ'")) ("var" ("3"))))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'CHECK'")) ("var" ("4"))))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'TNS-ASP'")) ("var" ("5"))))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'CLAUSE-TYPE'")) ("'decl'")))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'NEG'")) ("'+'")))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'POLARITY'")) ("'neg'")))
;;       ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'VFORM'")) ("'fin'")))
;;       ("cf" ("1")
;; 	    ("eq" ("var" ("1"))
;; 		  ("semform" ("'qePa'") ("10") (LIST ("var" ("3"))) (LIST ("")))))
;;       ("cf" ("1")
;;        ("eq" ("attr" ("var" ("3")) ("'PRED'"))
;;         ("semform" ("'kata'") ("8") (LIST ("")) (LIST (""))))))
;; ((0
;;   (pred (var 1))
;;   (subj (var 3))
;;   (check (var 4))
;;   (tns-asp (var 5))
;;   (clause-type decl)
;;   (vform fin))
;;  (1 (semform qePa 10 ((var 3)) ()))
;;  (3 (pred (semform kata 8 (()) (())))))

;;; "'3'" and "3" are different so we can treat the first as the
;;; attribute value (e.g. pers '3') and the second as the symbol (var)

(progn
  (defun foo-pred (var tab)
    (if (assoc '|'PRED'| (gethash var tab))
	(assoc '|'PRED'| (gethash var tab))))
  (defun foo-children (pred)
    (fourth pred))

  (defun foo (var tab)
    (let* ((pred (foo-pred var tab))
	   (children (when pred
		       (print pred)
		       (loop for child in (foo-children pred)
			    collect (foo child tab)))))))
  
  (defun test ()
    (let* ((ka-tab
	    (with-open-file
		(stream (merge-pathnames "ka-1.pl"
					 (asdf:component-pathname (asdf:find-system :lfgalign))))
	      (import-f-table stream)))
	   (nb-tab
	    (with-open-file
		(stream (merge-pathnames "nb-1.pl"
					 (asdf:component-pathname (asdf:find-system :lfgalign))))
	      (import-f-table stream))))
      ;; assume outermost f-str has var(0) and contains a PRED
      (foo '|0| nb-tab)
      (foo '|0| ka-tab)


      ))
  
  
      ;; => ((|'bjeffe'| |10| (|'NULL'| |5|) NIL))

  (test))

