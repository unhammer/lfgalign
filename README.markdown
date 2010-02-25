Progress
==========

prolog-import.lisp parses an XLE Prolog file and puts everything into
a hash table. Keys are f-structure variable numbers for the
f-structure, while the c-structure parts are referenced on the names
of the parts (subtree, terminal, phi, cproj, fspan, semform_data,
surfaceform), the values being alists with unique id keys. If we turn
it all back into an assoc-list, we get e.g.:

    ((|0| (|'VFORM'| . |'fin'|) (|'CLAUSE-TYPE'| . |'decl'|) (|'TNS-ASP'| . |5|)
      (|'CHECK'| . |4|) (|'SUBJ'| . |3|) (|'PRED'| . |1|))
     (|1| |'qePa'| |2| (|3|) NIL)
     (|3| (|'PERS'| . |'3'|) (|'NUM'| . |'sg'|) (|'CASE'| . |'erg'|)
      (|'ANIM'| . |'+'|) (|'NTYPE'| . |8|) (|'CHECK'| . |7|)
      (|'PRED'| |'Abrams'| |0| NIL NIL))
     (|7| (|'_AGR-POS'| . |'left'|)) (|8| (|'NSYN'| . |'proper'|))
     (|4| (|'_TENSEGROUP'| . |'aor'|) (|'_TENSE'| . |'aor'|) (|'_PERIOD'| . |'+'|)
      (|'_MAIN-CL'| . |'+'|) (|'_AGR'| . |'both'|) (|'_MORPH-SYNT'| . |10|)
      (|'_IN-SITU'| . |9|))
     (|in_set| (|'NO-PV'| |14|) (|var| |9|))
     (|10| (|'_SYNTAX'| . |'unerg'|) (|'_PERF-PV'| . |'-'|)
      (|'_LEXID'| . |'V2746-3'|) (|'_CLASS'| . |'MV'|) (|'_AGR'| . |11|))
     (|11| (|'_OBJ'| . |12|)) (|12| (|'PERS'| . |'3'|) (|'NUM'| . |'sg'|))
     (|5| (|'TENSE'| . |'past'|) (|'MOOD'| . |'indicative'|)
      (|'ASPECT'| . |'perf'|))
     (|13| (|'o::'| . |14|))
     (|subtree| (|38| |'PERIOD'| - |37|) (|23| |'V_SUFF_BASE'| - |24|)
      (|25| |'V_SUFF_BASE'| - |26|) (|27| |'V_SUFF_BASE'| - |28|)
      (|29| |'V_SUFF_BASE'| - |30|) (|31| |'V_SUFF_BASE'| - |32|)
      (|33| |'V_BASE'| - |34|) (|172| |'V'| - |33|) (|173| |'V'| |172| |31|)
      (|174| |'V'| |173| |29|) (|175| |'V'| |174| |27|) (|176| |'V'| |175| |25|)
      (|177| |'V'| |176| |23|) (|378| |'I[main,-]'| - |177|)
      (|379| |'Ibar[main,-]'| - |378|) (|2| |'PROP'| - |1|) (|144| |'PROPP'| - |2|)
      (|149| |'IPfoc[main,-]'| - |144|) (|381| |'IPfoc[main,-]'| |149| |379|)
      (|385| |'ROOT'| - |381|) (|387| |'ROOT'| |385| |38|))
     (|phi| (|37| |0|) (|38| |0|) (|24| |0|) (|23| |0|) (|26| |0|) (|25| |0|)
      (|28| |0|) (|27| |0|) (|30| |0|) (|29| |0|) (|32| |0|) (|31| |0|) (|34| |15|)
      (|33| |0|) (|172| |0|) (|173| |0|) (|174| |0|) (|175| |0|) (|176| |0|)
      (|177| |0|) (|378| |0|) (|379| |0|) (|1| |3|) (|2| |3|) (|144| |3|)
      (|149| |0|) (|381| |0|) (|385| |0|) (|387| |0|))
     (|terminal| (|37| |'.'| (|37|)) (|24| |'+Obj3'| (|21|))
      (|26| |'+Subj3Sg'| (|21|)) (|28| |'+Aor'| (|21|)) (|30| |'+Unerg'| (|21|))
      (|32| |'+V'| (|21|)) (|34| |'qePa-2746-3'| (|21|)) (|1| |'abramsma'| (|1|)))
     (|cproj| (|34| |13|)) (|semform_data| (|2| |33| |10| |14|) (|0| |2| |1| |9|))
     (|fspan| (|3| |1| |9|) (|0| |1| |16|))
     (|surfaceform| (|37| |'.'| |15| |16|) (|21| |'iqePa'| |10| |15|)
      (|1| |'abramsma'| |1| |9|)))

lfgalign.lisp currently just aligns PRED and their arguments
recursively, not caring about head-switching problems etc.

Plan
==========

1. Show c-structure consequences of PRED-alignments by following
 the `rassoc` of `phi` 
2. ???
3. Profit

