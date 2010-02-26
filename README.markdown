Progress
==========

prolog-import.lisp parses an XLE Prolog file and puts everything into
a hash table. Keys are f-structure variable numbers for the
f-structure, while the c-structure parts are referenced on the names
of the parts (subtree, terminal, phi, cproj, fspan, semform_data,
surfaceform), the values being alists with unique id keys. If we turn
it all back into an assoc-list, we get e.g.:

    ((0 ("VFORM" . "fin") ("CLAUSE-TYPE" . "decl") ("TNS-ASP" . 10)
      ("POLARITY" . 5) ("CHECK" . 1) ("SUBJ" . 3) ("PRED" "qePa" 2 (3) NIL))
     (3 ("PERS" . "3") ("NUM" . "sg") ("CASE" . "erg") ("ANIM" . "+") ("NTYPE" . 6)
      ("CHECK" . 4) ("PRED" "Abrams" 0 NIL NIL))
     (4 ("_AGR-POS" . "left") ("_POLARITY" . 5)) (6 ("NSYN" . "proper"))
     (1 ("_TENSEGROUP" . "aor") ("_TENSE" . "aor") ("_PERIOD" . "+")
      ("_MAIN-CL" . "+") ("_AGR" . "both") ("_MORPH-SYNT" . 7) ("_IN-SITU" . 2))
     (|in_set| ("NO-PV" . 22) (3 . 2))
     (7 ("_SYNTAX" . "unerg") ("_PERF-PV" . "-") ("_LEXID" . "V2746-3")
      ("_CLASS" . "MV") ("_AGR" . 8))
     (8 ("_OBJ" . 9)) (9 ("PERS" . "3") ("NUM" . "sg"))
     (10 ("TENSE" . "past") ("MOOD" . "indicative") ("ASPECT" . "perf"))
     (21 ("o::" . 22))
     (|subtree| (2 "PROP" NIL 1) (4 "V_SUFF_BASE" NIL 5) (6 "V_SUFF_BASE" NIL 7)
      (8 "V_SUFF_BASE" NIL 9) (10 "V_SUFF_BASE" NIL 11) (12 "V_SUFF_BASE" NIL 13)
      (14 "V_SUFF_BASE" NIL 15) (18 "V_BASE" NIL 17) (28 "PERIOD" NIL 22)
      (118 "PROPP" NIL 2) (141 "IPfoc[main,-]" NIL 118) (281 "V" NIL 18)
      (283 "V" 281 14) (284 "V" 283 12) (285 "V" 284 10) (286 "V" 285 8)
      (287 "V" 286 6) (288 "V" 287 4) (293 "I[main,-]" NIL 288)
      (398 "Ibar[main,-]" NIL 293) (401 "IPfoc[main,-]" 141 398)
      (454 "ROOT" NIL 401) (457 "ROOT" 454 28))
     (|phi| (1 . 3) (2 . 3) (4 . 0) (5 . 0) (6 . 0) (7 . 0) (8 . 0) (9 . 0)
      (10 . 0) (11 . 0) (12 . 0) (13 . 0) (14 . 0) (15 . 0) (17 . 23) (18 . 0)
      (22 . 0) (28 . 0) (118 . 3) (141 . 0) (281 . 0) (283 . 0) (284 . 0) (285 . 0)
      (286 . 0) (287 . 0) (288 . 0) (293 . 0) (398 . 0) (401 . 0) (454 . 0)
      (457 . 0))
     (|terminal| (1 "abramsma" (1)) (5 "+Obj3" (3)) (7 "+Subj3Sg" (3))
      (9 "+Aor" (3)) (11 "+Base" (3)) (13 "+Unerg" (3)) (15 "+V" (3))
      (17 "qePa-2746-3" (3)) (22 "." (22)))
     (|cproj| (17 . 21)) (|semform_data| (2 18 10 14) (0 2 1 9))
     (|fspan| (3 1 9) (0 1 16))
     (|surfaceform| (22 "." 15 16) (3 "iqePa" 10 15) (1 "abramsma" 1 9)))

lfgalign.lisp currently just aligns PRED and their arguments
recursively, not caring about head-switching problems etc.

Plan
==========

1. Show c-structure consequences of PRED-alignments by following
 the `rassoc` of `phi` 
2. ???
3. Profit

Re: http://tlt8.unicatt.it/allegati/Proceedings_TLT8.pdf p.71--82,
Giza++ gives us the LPT-correspondence (criterion i), our Prolog files
should have the information to use criterion ii.

