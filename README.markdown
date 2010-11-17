
About
=====

`lfgalign` is a program for aligning corresponding f-structures and
c-structures of LFG analysed parallel sentences. The analyses should
be in the
[XLE format](http://www2.parc.com/isl/groups/nltt/xle/doc/xle.html#Prolog_Output),
and preferably manually disambiguated from grammars that have been
written using common analysis principles (see the
[Xpar project description](http://xpar.b.uib.no/project-description/)).

There is an
[article about lfgalign](https://github.com/unhammer/lfgalign/raw/master/article/lfgalign-art.pdf)
that describes the method; see also the
[master's thesis](https://github.com/unhammer/lfgalign/raw/master/thesis/lfgalign.pdf)
(in Norwegian).



Usage
=====

Prerequisites:

- [asdf](http://common-lisp.net/project/asdf/), this is bundled with
  [SBCL](http://www.sbcl.org/) as well as the less common Common
  Lisps.

- [lisp-unit](http://github.com/OdonataResearchLLC/lisp-unit)
  (optional, for regression tests)

Make a symlink from your `systems` directory to `lfgalign.asd` in this
directory (you can do the same for `lisp-unit`); since I installed
SBCL using [clbuild](http://common-lisp.net/project/clbuild/) this
directory was at `/path/to/clbuild/systems`, but you can find the path
by evaluating `asdf:*central-registry*` in your interpreter after
requiring `'asdf`.

Running in the interpreter
--------------------------
Load the package in your interpreter with

    (asdf:operate 'asdf:load-op 'lfgalign)

Switch to the `lfgalign` package:

    (in-package :lfgalign)

(If you're using through Emacs with Slime, you can load from the REPL
with `, load RET lfgalign RET` and switch with `, in RET lfgalign
RET`.)

You can then run the regression tests with

    (lisp-unit:run-tests)
    
The function `evaluate` in the file `eval.lisp` shows how you load two
Prolog files into analysis tables, create an empty LPT table, run
f-alignment, ranking and c-alignment, finally give some
not-very-formatted output.

Running from the command-line
-----------------------------

A very preliminary command-line interface using SBCL is available. You
should be able to align two Prolog files by simply saying

    ./align.sh source.pl target.pl
    
although it assumes SBCL is installed in `usr`; you can set the
correct paths to SBCL and your asdf systems directory (where you
symlinked to lfgalign.asd) first by doing e.g.:
    
    export LISP=/l/c/clbuild/target/bin/sbcl
    export LISPCORE=/l/c/clbuild/target/lib/sbcl/sbcl.core
    export ASDFSYSTEMS=/l/c/clbuild/systems/

Common Lisp command-line interfaces are unfortunately not very
standardised.


Central functions
=================

lfgalign
----------

lfgalign.lisp currently does the following:

- collect c-structure trees: `maketree`

- find the topmost c-node in an f-domain: `topnode`

- find a c-node referenced by f-structure variable: `treefind`

- find f-structure predicate from variable, traversing equivalent
  f-vars: `get-pred` and `unravel`
  
- find arguments, adjuncts, lemma and lexical expression of a
  predicate/f-var: `get-args`, `get-adjs`, `lemma`, `L`
  
- keep tables of LPT correspondences (lookup with `LPT?` ensures a
  "pro" is an LPT of a noun as defined by `noun?`)
  
- find all set-unique combinations of links of source arguments with
  target args/adjuncts, and target arguments with source args/adjuncts
  (excluding adj-adj links): `argalign` (if given LPT tables, this
  removes combinations where at least one link is non-LPT)
  
- `outer-pred` creates a fake "sentence pred" with id -1, that has 0
  as an argument and, as adjuncts: any unreferenced preds in the
  f-structure (preds that are not arguments/adjuncts reachable through
  0)
 
- `f-align` combines the above and recursively tries to align all
  arguments in all permutations of argument-argument/adjunct pairs,
  creating a decision tree of sorts; `flatten` spreads this out into
  several simple lists. 
  
- `rank` uses `rank-helper` and `rank-branch` to turn the output from
  `f-align` into a single flat, ranked list of links for input into
  `c-align`.
  
- `add-links` takes a flat f-alignment and a tree, and creates a table
  of type `LL-splits`. Each node `n` is added to a list in the table,
  where the index of the list is the set of alignments of pre-terminal
  nodes dominated by node `n` (so several nodes may have the same
  index).

- `c-align` takes a flat f-alignment and finds the `LL-splits`
  of source and target trees, intersecting that on the keys to find
  which nodes can be aligned.
  
prolog-import
----------
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

We collect the eq-vars (equivalent variables) into a doubly-linked
circular list (so we can easily look up a member and get all
equivalents). 

We signal an error if the file is not disambiguated (as indicated by
the `select` and `choice` fields in the Prolog file). Otherwise, we
filter out non-selected parses from the file, keeping only the ones
equivalent to the selected parse (see `filter-equiv`, `in-disjunction`
and `disambiguated?`). 


TODO
====

- Use LPT-check as a k-best ranking criterion rather than a binary
  cut-off.
  
- SPEC and POSS features may lead to PRED's that are not arguments or
  adjuncts of anything else (e.g. determiners, possessors) -- need
  some principled method of aligning these.

- The program just uses dset3 of the dsets, rename it (make a class?)
  and deprecate the others.

- Could perhaps make argument calls a bit more concise by making a
  class `alignment`, containing constants `tab_s`, `tab_t`, creating
  constants `tree_s` and `tree_t` on init, and storing `LPT` and
  `f-alignments`.

