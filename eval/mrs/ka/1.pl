% -*- coding: iso-8859-1 -*-

fstructure('abramsma iqePa.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('2'),
	'statistics'('1+1 solutions, 0.04 CPU seconds, 42 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('qePa',2,[var(3)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(3),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'ANIM'),'+')),
	cf(1,eq(attr(var(3),'CASE'),'erg')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(6),'NSYN'),'proper')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(7))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'aor')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(7),'_AGR'),var(8))),
	cf(1,eq(attr(var(7),'_CLASS'),'MV')),
	cf(1,eq(attr(var(7),'_LEXID'),'V2746-3')),
	cf(1,eq(attr(var(7),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(7),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(8),'_OBJ'),var(9))),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'ASPECT'),'perf')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'past')),
	cf(1,eq(proj(var(21),'o::'),var(22))),
	cf(1,in_set('NO-PV',var(22)))
	],
	% C-Structure:
	[
	cf(1,subtree(457,'ROOT',454,28)),
	cf(1,phi(457,var(0))),
	cf(1,subtree(454,'ROOT',-,401)),
	cf(1,phi(454,var(0))),
	cf(1,subtree(401,'IPfoc[main,-]',141,398)),
	cf(1,phi(401,var(0))),
	cf(1,subtree(398,'Ibar[main,-]',-,293)),
	cf(1,phi(398,var(0))),
	cf(1,subtree(293,'I[main,-]',-,288)),
	cf(1,phi(293,var(0))),
	cf(1,subtree(288,'V',287,4)),
	cf(1,phi(288,var(0))),
	cf(1,subtree(287,'V',286,6)),
	cf(1,phi(287,var(0))),
	cf(1,subtree(286,'V',285,8)),
	cf(1,phi(286,var(0))),
	cf(1,subtree(285,'V',284,10)),
	cf(1,phi(285,var(0))),
	cf(1,subtree(284,'V',283,12)),
	cf(1,phi(284,var(0))),
	cf(1,subtree(283,'V',281,14)),
	cf(1,phi(283,var(0))),
	cf(1,subtree(281,'V',-,18)),
	cf(1,phi(281,var(0))),
	cf(1,subtree(141,'IPfoc[main,-]',-,118)),
	cf(1,phi(141,var(0))),
	cf(1,subtree(118,'PROPP',-,2)),
	cf(1,phi(118,var(3))),
	cf(1,subtree(28,'PERIOD',-,22)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(22,'.',[22])),
	cf(1,phi(22,var(0))),
	cf(1,subtree(18,'V_BASE',-,17)),
	cf(1,phi(18,var(0))),
	cf(1,terminal(17,'qePa-2746-3',[3])),
	cf(1,phi(17,var(23))),
	cf(1,cproj(17,var(21))),
	cf(1,terminal(15,'+V',[3])),
	cf(1,phi(15,var(0))),
	cf(1,subtree(14,'V_SUFF_BASE',-,15)),
	cf(1,phi(14,var(0))),
	cf(1,terminal(13,'+Unerg',[3])),
	cf(1,phi(13,var(0))),
	cf(1,subtree(12,'V_SUFF_BASE',-,13)),
	cf(1,phi(12,var(0))),
	cf(1,terminal(11,'+Base',[3])),
	cf(1,phi(11,var(0))),
	cf(1,subtree(10,'V_SUFF_BASE',-,11)),
	cf(1,phi(10,var(0))),
	cf(1,terminal(9,'+Aor',[3])),
	cf(1,phi(9,var(0))),
	cf(1,subtree(8,'V_SUFF_BASE',-,9)),
	cf(1,phi(8,var(0))),
	cf(1,terminal(7,'+Subj3Sg',[3])),
	cf(1,phi(7,var(0))),
	cf(1,subtree(6,'V_SUFF_BASE',-,7)),
	cf(1,phi(6,var(0))),
	cf(1,terminal(5,'+Obj3',[3])),
	cf(1,phi(5,var(0))),
	cf(1,subtree(4,'V_SUFF_BASE',-,5)),
	cf(1,phi(4,var(0))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(3))),
	cf(1,terminal(1,'abramsma',[1])),
	cf(1,phi(1,var(3))),
	cf(1,semform_data(0,2,1,9)),
	cf(1,semform_data(2,18,10,14)),
	cf(1,fspan(var(0),1,16)),
	cf(1,fspan(var(3),1,9)),
	cf(1,surfaceform(1,'abramsma',1,9)),
	cf(1,surfaceform(3,'iqePa',10,15)),
	cf(1,surfaceform(22,'.',15,16))
	]).
