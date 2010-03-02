;;; Documentation for the Prolog export format:
;;; http://www2.parc.com/isl/groups/nltt/xle/doc/xle.html

;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)


(defun parse-args (stream)
  "Helper for parse-pred."
  (let* ((rv (parse-pred stream))
	 (prevc (car rv))
	 (args (cdr rv)))
    (if (or (eq prevc #\))
	    (eq prevc #\])
	    ;; can we get the dot here?
	    (eq prevc #\.))
	(return-from parse-args (list args))
	(let ((allargs (cons args
			     (parse-args stream))))
	  (return-from parse-args allargs)))))

(defun parse-pred (stream)
  "Create a list structure from a Prolog file by reading `stream'
character by character (would be faster if we slurp:
http://www.ymeme.com/slurping-a-file-common-lisp-83.html). This
function assumes that the next thing we see in the stream (apart from
comments) is a clause head or atom; ie. it shouldn't start within an
argument list, that's for the helper function `parse-args' to
handle. This function reads one clause head or atom (predicate of zero
arity), lets `parse-args' handle possible arguments, and then returns.

Returns a pair of the previous character and the created list
structure."
  (let ((instring nil)
	(args nil)
	(lastc nil)
	(head (make-array 0
			  :element-type 'character
			  :fill-pointer 0
			  :adjustable t)))
    (do ((c (read-char stream) (read-char stream nil 'eof)))
	((eq c 'eof))
      (if instring
	  (case c
	    (#\'
	     (unless (eq lastc #\\) (setq instring nil))
	     (vector-push-extend c head))
	    (otherwise
	     (vector-push-extend c head)))
	  (case c
	    (#\' (setq instring t)
		 (vector-push-extend c head))
	    (#\% (read-line stream))
	    (#\( (setq args (parse-args stream)))
	    (#\[ (setq args (parse-args stream))
		 (setq head 'list))
	    ((#\, #\) #\] #\.)
	     (return (cons c (cons head args))))
	    ((#\Tab #\Newline #\ )
	     t)
	    (otherwise
	     (vector-push-extend c head))))
      (setq lastc c))))

(defun parse-prolog (stream)
  (cdr (parse-pred stream)))

(defun raw-equiv (parse)
  "Skip the first element, LIST."
  (cdr (fifth parse)))
(defun raw-f-str (parse)
  "Skip the first element, LIST."
  (cdr (sixth parse)))
(defun raw-c-str (parse)
  "Skip the first element, LIST."
  (cdr (seventh parse)))

(defun in-disjunction (goals disjuncts)
  "See if any of the `disjuncts' are members of `goals'."
  (cond ((not (consp disjuncts)) (member disjuncts goals :test #'equal))
	((equal (car disjuncts) "and") (error 'unexpected-input disjuncts))
	((equal (car disjuncts) "or") (in-disjunction goals (cdr disjuncts)))
	(t (or 
	    (in-disjunction goals (car disjuncts))
	    (in-disjunction goals (cdr disjuncts))))))

(defun clean-equiv (raw-equiv)
  "Runs on `raw-equiv' output to create a list of Prolog variable
names which are equivalent to \"1\" (our selected parse)."
  (let ((select1s
	 (mapcar-true (lambda (x)
			(and (equal "select" (first x))
			     (equal "1" (car (third x)))
			     (car (second x))))
		      raw-equiv)))
    (append '("1")
	    select1s
	    (mapcar-true (lambda (x)
			   (and (equal "define" (first x))
				(or (not (fourth x)) (error 'unexpected-input x))
				(in-disjunction select1s (third x))
				(car (second x))))
			 raw-equiv))))

(defun filter-equiv (raw-equiv raw-cf)
  "Use on input to `clean-f-str' and `clean-c-str' to remove cf's
which are not part of the selected parse."
  (let ((equivs (clean-equiv raw-equiv)))
    (remove-if (lambda (cf)
		 (not (in-disjunction equivs (second cf))))
	       raw-cf)))

(defun clean-var (varnum)
  "Helper for `clean-f-str' and `clean-c-str'."
  (if (equal (car varnum) "'NULL'")
      "NULL"
      (parse-integer (caadr varnum))))

(defun clean-car/var (list)
  "Turn both 1 and var(1), i.e. (\"1\") and (\"var\" \"1\"), into 1;
return Prolog strings as strings, i.e. \"'PRED'\" to \"PRED\"."
  (let ((elt (car list)))
    (cond ((eq (intern elt) '|var|)
	   (clean-var list))
	  ((cdr list)	      ; if not var, we expect only one element
	   (error list))
	  ((eq #\' (aref elt 0))
	   (subseq elt 1 (1- (length elt))))
	  ((equal "-" elt)
	   nil)
	  (t
	   (parse-integer elt)))))

(defun clean-pred (semform)
  "Helper for `clean-f-str'."
  (list (clean-car/var (second semform))
	(clean-car/var (third semform))
	(if (not (equal (cadr (fourth semform)) '("")))
	    (mapcar #'clean-var (cdr (fourth semform))))
	(if (not (equal (cadr (fifth semform)) '("")))
	    (mapcar #'clean-var (cdr (fifth semform))))))

(defun clean-att-val (lhs rhs)
  "Helper for `clean-f-str'."
  (cons (clean-car/var lhs)
	(if (eq (intern (car rhs)) '|semform|)
	    (clean-pred rhs)
	    (clean-car/var rhs))))

(defun clean-f-str (raw-f)
  "Runs on `raw-f-str' output. Creates a pseudo-alist,
each var is a key but appears once for each attribute/projection etc.,
see `dup-alist-to-table' and `table-to-alist'."
  (mapcar
   (lambda (cf)
     (let* ((assig (third cf))
	    (lhs (second assig))
	    (rhs (third assig)))
       (case (intern (first assig))
	 (|eq|
	  (case (intern (car lhs))
	    (|var|
	     (list '|eqvar|
		   (clean-att-val lhs rhs)))
	    (|attr|
	     (list (clean-var (second lhs))
		   (clean-att-val (third lhs) rhs)))
	    (|proj|
	     (list (clean-var (second lhs))
		   (clean-att-val (third lhs) rhs)))))
	 (|in_set|
	  (list '|in_set|
		(clean-att-val lhs rhs))))))
   raw-f))

(defun attvalp (attval)
  (and (listp attval)
       (listp (cdr attval))
       (cdr attval)
       (listp (second attval))
       (not (cddr attval))))

(defun clean-c-str (raw-c)
  "Runs on `raw-c-str' output. Creates a pseudo-alist, each type is a
key but appears once for each attribute etc., see `dup-alist-to-table'
and `table-to-alist'."
  (mapcar
   (lambda (cf)
     (let* ((assig (third cf))
	    (type (intern (car assig)))
	    (id (clean-car/var (second assig)))
	    (rest (cddr assig)))
       (list type
	     (cons id
		   (case type
		     (|terminal| (list (clean-car/var (first rest))
				       (mapcar #'clean-car/var (cdr (second rest)))))
		     (|phi| (clean-car/var (first rest)))
		     (|cproj| (clean-car/var (first rest)))
		     (t (mapcar #'clean-car/var rest)))))))
   raw-c))



(defun dup-alist-to-table (dup-alist &optional no-eq-sets)
  "Runs on `clean-f-str' or `clean-c-str' output to create a hash
table so that we can get the full list of attributes by looking up
with the var key."
  (let ((table (make-hash-table)))
    (dolist (pair dup-alist)
      (let ((key (car pair)))
	(if (attvalp pair)
	    (setf (gethash key table)
		  (cons (second pair) (gethash key table)))
	    (if (gethash key table)
		(error 'unexpected-input key)
		(setf (gethash key table)
		      (cdr pair))))))
    (unless no-eq-sets (setf (gethash '|eq-sets| table)
			     (dset-collect (gethash '|eqvar| table))))
    table))


(defun import-table (stream)
  "Convenience function, turn a file-stream into a table where each
key is an f-str variable or a c-structure part (subtree, phi, fspan,
terminal etc.)"
  (let ((raw (parse-prolog stream)))
    (dup-alist-to-table (append (clean-f-str (filter-equiv (raw-equiv raw)
							   (raw-f-str raw)))
				(clean-c-str (filter-equiv (raw-equiv raw)
							   (raw-c-str raw)))))))

(defun table-to-alist (table &optional print)
  "Convenience function, turn a hash table into an association list
(printing it nicely along the way if `print' is true)."
  (loop for value being the hash-values of table
     using (hash-key key)
     do (when print (format t "~&~A -> ~A" key value))
     collect (cons key value)))



;;;;;;;; TESTING:
(lisp-unit:define-test test-clean-c
  (lisp-unit:assert-equal
   ;; can we assume all subtrees are (at most) binary?
   '((|subtree| (19 "AInt_BASE" NIL 20)))
   (clean-c-str '(("cf" ("1") ("subtree" ("19") ("'AInt_BASE'") ("-") ("20"))))))
  (lisp-unit:assert-equal
   '((|subtree| (387 "ROOT" 385 38)))
   (clean-c-str '(("cf" ("1") ("subtree" ("387") ("'ROOT'") ("385") ("38"))))))
  (lisp-unit:assert-equal
   '((|terminal| (1 "abramsma" (1))))
   (clean-c-str '(("cf" ("1") ("terminal" ("1") ("'abramsma'") (LIST ("1")))))))
  (lisp-unit:assert-equal
   '((|phi| (387 . 0)))
   (clean-c-str '(("cf" ("1") ("phi" ("387") ("var" ("0")))))))
  (lisp-unit:assert-equal
   '((|cproj| (34 . 13)))
   (clean-c-str '(("cf" ("1") ("cproj" ("34") ("var" ("13")))))))
  (lisp-unit:assert-equal
   '((|semform_data| (0 2 1 9)))
   (clean-c-str '(("cf" ("1") ("semform_data" ("0") ("2") ("1") ("9"))))))
  (lisp-unit:assert-equal
   '((|fspan| (0 1 16)))
   (clean-c-str '(("cf" ("1") ("fspan" ("var" ("0")) ("1") ("16"))))))
  (lisp-unit:assert-equal
   '((|surfaceform| (1 "abramsma" 1 9)))
   (clean-c-str '(("cf" ("1") ("surfaceform" ("1") ("'abramsma'") ("1") ("9")))))))

(lisp-unit:define-test test-clean-f
  (lisp-unit:assert-equal
   '((|in_set| ("NO-PV" . 19)))
   (clean-f-str '(("cf" ("1") ("in_set" ("'NO-PV'") ("var" ("19")))))))
  (lisp-unit:assert-equal
   '((|eqvar| (20 . 2))
     (0 ("PRED" . 1)))
   (clean-f-str '(("cf" ("1") ("eq"
			       ("var" ("20"))
			       ("var" ("2"))))
		  ("cf" ("1") ("eq"
			       ("attr" ("var" ("0")) ("'PRED'"))
			       ("var" ("1")))))))
  (lisp-unit:assert-equal
   '((18 ("o::" . 19)))
   (clean-f-str '(("cf" ("1") ("eq" ("proj" ("var" ("18")) ("'o::'")) ("var" ("19")))))))
  (lisp-unit:assert-equal
   '((|eqvar| (1 "bjeffe" 10 ("NULL" 5) NIL))
     (|eqvar| (1 "qePa" 10 (3) NIL)))
   (clean-f-str '(("cf" ("1")
		   ("eq" ("var" ("1"))
		    ("semform" ("'bjeffe'") ("10") (LIST ("'NULL'") ("var" ("5")))
			       (LIST ("")))))
		  ("cf" ("1") ("eq"
			       ("var" ("1"))
			       ("semform" ("'qePa'") ("10") (LIST ("var" ("3"))) (LIST (""))))))))
  (lisp-unit:assert-equal
   '((3 ("PRED" "kata" 8 NIL NIL)))
   (clean-f-str '(("cf" ("1") ("eq"
			       ("attr" ("var" ("3")) ("'PRED'"))
			       ("semform" ("'kata'") ("8") (LIST ("")) (LIST (""))))))))
  (lisp-unit:assert-equal
   '((|eqvar| (20 . "past")))
   (clean-f-str '(("cf" ("1") ("eq"
			       ("var" ("20"))
			       ("'past'"))))))
  (lisp-unit:assert-equal 
   '((5 ("CASE" . "erg")))
   (clean-f-str '(("cf" ("1") ("eq"
			       ("attr" ("var" ("5")) ("'CASE'"))
			       ("'erg'")))))))

(lisp-unit:define-test test-attvalp
  (lisp-unit:assert-true (attvalp '(5 ("'CASE'" . "'erg'"))))
  (lisp-unit:assert-false (attvalp '(2 "'qePa-dup'" 10 (3) NIL)))
  (lisp-unit:assert-false (attvalp '(20 . "'past'")))
  (lisp-unit:assert-false (attvalp '(20)))
  (lisp-unit:assert-false (attvalp '20)))

(lisp-unit:define-test test-var/pred
  (lisp-unit:assert-eq
   3
   (clean-var '("var" ("3"))))
  (lisp-unit:assert-equal
   '("kata" 8 NIL NIL)
   (clean-pred '("semform" ("'kata'") ("8") (LIST ("")) (LIST ("")))))
  (lisp-unit:assert-equal
   '("qePa" 10 (3) NIL)
   (clean-pred '("semform" ("'qePa'") ("10") (LIST ("var" ("3"))) (LIST ("")))))
  (lisp-unit:assert-equal
   '("bjeffe" 10 ("NULL" 5) NIL)
   (clean-pred '("semform" ("'bjeffe'") ("10") (LIST ("'NULL'") ("var" ("5"))) (LIST (""))))))


(lisp-unit:define-test test-make-table
  (lisp-unit:assert-equal '((|in_set| (|'NO-PV'| 19)))
			  (table-to-alist (dup-alist-to-table '((|in_set| (|'NO-PV'| 19)))
							      'no-eq-sets)))

  (lisp-unit:assert-equal '((|eqvar| (20 . 2)))
			  (table-to-alist (dup-alist-to-table '((|eqvar| (20 . 2)))
							      'no-eq-sets)))

  (lisp-unit:assert-equal '((5 (|'CASE'| . |'erg'|)))
			  (table-to-alist (dup-alist-to-table '((5 (|'CASE'| . |'erg'|)))
							      'no-eq-sets)))
  (lisp-unit:assert-equal '((0 (|'PRED'| . 1)))
			  (table-to-alist (dup-alist-to-table '((0 (|'PRED'| . 1)))
							      'no-eq-sets)))
  (lisp-unit:assert-equal
   '((18 (|'o::'| . 19))
     (1 "'bjeffe'" 10 ("'NULL'" 5) NIL)
     (3 ("'PRED'" "'kata'" 8 NIL NIL)))
   (table-to-alist
    (dup-alist-to-table
     '((18 (|'o::'| . 19))
       (1 "'bjeffe'" 10 ("'NULL'" 5) NIL)
       (3 ("'PRED'" "'kata'" 8 NIL NIL)))
     'no-eq-sets))))

(lisp-unit:define-test test-disj
  (lisp-unit:assert-true 
   (in-disjunction '("C5") (third '("define" ("CV_010")
				    ("or" ("or" ("D5") ("D1") ("D3")) ("C5") ("or" ("C1") ("C3")) ("B5")
				     ("B1") ("B3")))))))

(lisp-unit:define-test test-equiv
  (let ((raw-equiv
	 (with-open-file
	     (stream (merge-pathnames "dev/TEST_equiv.pl"
				      (asdf:component-pathname (asdf:find-system :lfgalign))))
	   (raw-equiv (parse-prolog stream)))))
    (lisp-unit:assert-equal
     '("1" "D6" "A3" "CV_004" "CV_005" "CV_007" "CV_008" "CV_009")
     (clean-equiv raw-equiv))
    (lisp-unit:assert-equal
     '((3 ("PRED" "kata" 8 NIL NIL)))
     (clean-f-str (filter-equiv	raw-equiv
				'(("cf" ("A3") ("eq"
						("attr" ("var" ("3")) ("'PRED'"))
						("semform" ("'kata'") ("8") (LIST ("")) (LIST ("")))))))))
    (lisp-unit:assert-false
     (clean-f-str (filter-equiv raw-equiv
				'(("cf" ("A4") ("eq"
					       ("attr" ("var" ("3")) ("'PRED'"))
					       ("semform" ("'kata'") ("8") (LIST ("")) (LIST ("")))))))))))

(lisp-unit:define-test test-parsefile
  (lisp-unit:assert-equal
     '("fstructure" ("'abramsma iqePa.'")
       (LIST ("'xle_version'" ("'XLE release of Jan 21, 2008 10:36.'"))
	("'grammar'" ("'/usr/local/xledir/pargram/kartuli/kartuli.lfg'"))
	("'grammar_date'" ("'Oct 28, 2008 23:47'"))
	("'statistics'"
	 ("'2+2 solutions, 0.04 CPU seconds, 34 subtrees unified'"))
	("'rootcategory'" ("'ROOT'"))
	("'max_medial_constituent_weight'" ("'35'"))
	("'max_medial2_constituent_weight'" ("'30'")))
       (LIST ("")) (LIST (""))
       (LIST ("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'PRED'")) ("var" ("1"))))
	("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'SUBJ'")) ("var" ("3"))))
	("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'CHECK'")) ("var" ("4"))))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("0")) ("'TNS-ASP'")) ("var" ("5"))))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("0")) ("'CLAUSE-TYPE'")) ("'decl'")))
	("cf" ("1") ("eq" ("attr" ("var" ("0")) ("'VFORM'")) ("'fin'")))
	("cf" ("1")
	 ("eq" ("var" ("1"))
	       ("semform" ("'qePa'") ("2") (LIST ("var" ("3"))) (LIST ("")))))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("3")) ("'PRED'"))
	       ("semform" ("'Abrams'") ("0") (LIST ("")) (LIST ("")))))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'CHECK'")) ("var" ("7"))))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'NTYPE'")) ("var" ("8"))))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'ANIM'")) ("'+'")))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'CASE'")) ("'erg'")))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'NUM'")) ("'sg'")))
	("cf" ("1") ("eq" ("attr" ("var" ("3")) ("'PERS'")) ("'3'")))
	("cf" ("1") ("eq" ("attr" ("var" ("7")) ("'_AGR-POS'")) ("'left'")))
	("cf" ("1") ("eq" ("attr" ("var" ("8")) ("'NSYN'")) ("'proper'")))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("4")) ("'_IN-SITU'")) ("var" ("9"))))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("4")) ("'_MORPH-SYNT'")) ("var" ("10"))))
	("cf" ("1") ("eq" ("attr" ("var" ("4")) ("'_AGR'")) ("'both'")))
	("cf" ("1") ("eq" ("attr" ("var" ("4")) ("'_MAIN-CL'")) ("'+'")))
	("cf" ("1") ("eq" ("attr" ("var" ("4")) ("'_PERIOD'")) ("'+'")))
	("cf" ("1") ("eq" ("attr" ("var" ("4")) ("'_TENSE'")) ("'aor'")))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("4")) ("'_TENSEGROUP'")) ("'aor'")))
	("cf" ("1") ("in_set" ("var" ("3")) ("var" ("9"))))
	("cf" ("1") ("eq" ("attr" ("var" ("10")) ("'_AGR'")) ("var" ("11"))))
	("cf" ("1") ("eq" ("attr" ("var" ("10")) ("'_CLASS'")) ("'MV'")))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("10")) ("'_LEXID'")) ("'V2746-3'")))
	("cf" ("1") ("eq" ("attr" ("var" ("10")) ("'_PERF-PV'")) ("'-'")))
	("cf" ("1") ("eq" ("attr" ("var" ("10")) ("'_SYNTAX'")) ("'unerg'")))
	("cf" ("1") ("eq" ("attr" ("var" ("11")) ("'_OBJ'")) ("var" ("12"))))
	("cf" ("1") ("eq" ("attr" ("var" ("12")) ("'NUM'")) ("'sg'")))
	("cf" ("1") ("eq" ("attr" ("var" ("12")) ("'PERS'")) ("'3'")))
	("cf" ("1") ("eq" ("attr" ("var" ("5")) ("'ASPECT'")) ("'perf'")))
	("cf" ("1")
	 ("eq" ("attr" ("var" ("5")) ("'MOOD'")) ("'indicative'")))
	("cf" ("1") ("eq" ("attr" ("var" ("5")) ("'TENSE'")) ("'past'")))
	("cf" ("1") ("eq" ("proj" ("var" ("13")) ("'o::'")) ("var" ("14"))))
	("cf" ("1") ("in_set" ("'NO-PV'") ("var" ("14")))))
       (LIST ("cf" ("1") ("subtree" ("387") ("'ROOT'") ("385") ("38")))
	("cf" ("1") ("phi" ("387") ("var" ("0"))))
	("cf" ("1") ("subtree" ("385") ("'ROOT'") ("-") ("381")))
	("cf" ("1") ("phi" ("385") ("var" ("0"))))
	("cf" ("1") ("subtree" ("381") ("'IPfoc[main,-]'") ("149") ("379")))
	("cf" ("1") ("phi" ("381") ("var" ("0"))))
	("cf" ("1") ("subtree" ("149") ("'IPfoc[main,-]'") ("-") ("144")))
	("cf" ("1") ("phi" ("149") ("var" ("0"))))
	("cf" ("1") ("subtree" ("144") ("'PROPP'") ("-") ("2")))
	("cf" ("1") ("phi" ("144") ("var" ("3"))))
	("cf" ("1") ("subtree" ("2") ("'PROP'") ("-") ("1")))
	("cf" ("1") ("phi" ("2") ("var" ("3"))))
	("cf" ("1") ("terminal" ("1") ("'abramsma'") (LIST ("1"))))
	("cf" ("1") ("phi" ("1") ("var" ("3"))))
	("cf" ("1") ("subtree" ("379") ("'Ibar[main,-]'") ("-") ("378")))
	("cf" ("1") ("phi" ("379") ("var" ("0"))))
	("cf" ("1") ("subtree" ("378") ("'I[main,-]'") ("-") ("177")))
	("cf" ("1") ("phi" ("378") ("var" ("0"))))
	("cf" ("1") ("subtree" ("177") ("'V'") ("176") ("23")))
	("cf" ("1") ("phi" ("177") ("var" ("0"))))
	("cf" ("1") ("subtree" ("176") ("'V'") ("175") ("25")))
	("cf" ("1") ("phi" ("176") ("var" ("0"))))
	("cf" ("1") ("subtree" ("175") ("'V'") ("174") ("27")))
	("cf" ("1") ("phi" ("175") ("var" ("0"))))
	("cf" ("1") ("subtree" ("174") ("'V'") ("173") ("29")))
	("cf" ("1") ("phi" ("174") ("var" ("0"))))
	("cf" ("1") ("subtree" ("173") ("'V'") ("172") ("31")))
	("cf" ("1") ("phi" ("173") ("var" ("0"))))
	("cf" ("1") ("subtree" ("172") ("'V'") ("-") ("33")))
	("cf" ("1") ("phi" ("172") ("var" ("0"))))
	("cf" ("1") ("subtree" ("33") ("'V_BASE'") ("-") ("34")))
	("cf" ("1") ("phi" ("33") ("var" ("0"))))
	("cf" ("1") ("terminal" ("34") ("'qePa-2746-3'") (LIST ("21"))))
	("cf" ("1") ("phi" ("34") ("var" ("15"))))
	("cf" ("1") ("cproj" ("34") ("var" ("13"))))
	("cf" ("1") ("subtree" ("31") ("'V_SUFF_BASE'") ("-") ("32")))
	("cf" ("1") ("phi" ("31") ("var" ("0"))))
	("cf" ("1") ("terminal" ("32") ("'+V'") (LIST ("21"))))
	("cf" ("1") ("phi" ("32") ("var" ("0"))))
	("cf" ("1") ("subtree" ("29") ("'V_SUFF_BASE'") ("-") ("30")))
	("cf" ("1") ("phi" ("29") ("var" ("0"))))
	("cf" ("1") ("terminal" ("30") ("'+Unerg'") (LIST ("21"))))
	("cf" ("1") ("phi" ("30") ("var" ("0"))))
	("cf" ("1") ("subtree" ("27") ("'V_SUFF_BASE'") ("-") ("28")))
	("cf" ("1") ("phi" ("27") ("var" ("0"))))
	("cf" ("1") ("terminal" ("28") ("'+Aor'") (LIST ("21"))))
	("cf" ("1") ("phi" ("28") ("var" ("0"))))
	("cf" ("1") ("subtree" ("25") ("'V_SUFF_BASE'") ("-") ("26")))
	("cf" ("1") ("phi" ("25") ("var" ("0"))))
	("cf" ("1") ("terminal" ("26") ("'+Subj3Sg'") (LIST ("21"))))
	("cf" ("1") ("phi" ("26") ("var" ("0"))))
	("cf" ("1") ("subtree" ("23") ("'V_SUFF_BASE'") ("-") ("24")))
	("cf" ("1") ("phi" ("23") ("var" ("0"))))
	("cf" ("1") ("terminal" ("24") ("'+Obj3'") (LIST ("21"))))
	("cf" ("1") ("phi" ("24") ("var" ("0"))))
	("cf" ("1") ("subtree" ("38") ("'PERIOD'") ("-") ("37")))
	("cf" ("1") ("phi" ("38") ("var" ("0"))))
	("cf" ("1") ("terminal" ("37") ("'.'") (LIST ("37"))))
	("cf" ("1") ("phi" ("37") ("var" ("0"))))
	("cf" ("1") ("semform_data" ("0") ("2") ("1") ("9")))
	("cf" ("1") ("semform_data" ("2") ("33") ("10") ("14")))
	("cf" ("1") ("fspan" ("var" ("0")) ("1") ("16")))
	("cf" ("1") ("fspan" ("var" ("3")) ("1") ("9")))
	("cf" ("1") ("surfaceform" ("1") ("'abramsma'") ("1") ("9")))
	("cf" ("1") ("surfaceform" ("21") ("'iqePa'") ("10") ("15")))
	("cf" ("1") ("surfaceform" ("37") ("'.'") ("15") ("16")))))
     (with-open-file
	 (stream (merge-pathnames "dev/TEST_parse.pl"
				  (asdf:component-pathname (asdf:find-system :lfgalign))))
       (parse-prolog stream))))
