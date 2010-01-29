
% -*- coding: iso-8859-1 -*-

fstructure('abramsma iqePa.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Oct 28, 2008 23:47'),
	'statistics'('2+2 solutions, 0.04 CPU seconds, 34 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30')
	],
	% Choices:
	[
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(1))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(4))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(5))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(var(1),semform('qePa',2,[var(3)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(7))),
	cf(1,eq(attr(var(3),'NTYPE'),var(8))),
	cf(1,eq(attr(var(3),'ANIM'),'+')),
	cf(1,eq(attr(var(3),'CASE'),'erg')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(7),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(8),'NSYN'),'proper')),
	cf(1,eq(attr(var(4),'_IN-SITU'),var(9))),
	cf(1,eq(attr(var(4),'_MORPH-SYNT'),var(10))),
	cf(1,eq(attr(var(4),'_AGR'),'both')),
	cf(1,eq(attr(var(4),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(4),'_PERIOD'),'+')),
	cf(1,eq(attr(var(4),'_TENSE'),'aor')),
	cf(1,eq(attr(var(4),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(9))),
	cf(1,eq(attr(var(10),'_AGR'),var(11))),
	cf(1,eq(attr(var(10),'_CLASS'),'MV')),
	cf(1,eq(attr(var(10),'_LEXID'),'V2746-3')),
	cf(1,eq(attr(var(10),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(10),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(11),'_OBJ'),var(12))),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(5),'ASPECT'),'perf')),
	cf(1,eq(attr(var(5),'MOOD'),'indicative')),
	cf(1,eq(attr(var(5),'TENSE'),'past')),
	cf(1,eq(proj(var(13),'o::'),var(14))),
	cf(1,in_set('NO-PV',var(14)))
	],
	% C-Structure:
	[
	cf(1,subtree(387,'ROOT',385,38)),
	cf(1,phi(387,var(0))),
	cf(1,subtree(385,'ROOT',-,381)),
	cf(1,phi(385,var(0))),
	cf(1,subtree(381,'IPfoc[main,-]',149,379)),
	cf(1,phi(381,var(0))),
	cf(1,subtree(149,'IPfoc[main,-]',-,144)),
	cf(1,phi(149,var(0))),
	cf(1,subtree(144,'PROPP',-,2)),
	cf(1,phi(144,var(3))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(3))),
	cf(1,terminal(1,'abramsma',[1])),
	cf(1,phi(1,var(3))),
	cf(1,subtree(379,'Ibar[main,-]',-,378)),
	cf(1,phi(379,var(0))),
	cf(1,subtree(378,'I[main,-]',-,177)),
	cf(1,phi(378,var(0))),
	cf(1,subtree(177,'V',176,23)),
	cf(1,phi(177,var(0))),
	cf(1,subtree(176,'V',175,25)),
	cf(1,phi(176,var(0))),
	cf(1,subtree(175,'V',174,27)),
	cf(1,phi(175,var(0))),
	cf(1,subtree(174,'V',173,29)),
	cf(1,phi(174,var(0))),
	cf(1,subtree(173,'V',172,31)),
	cf(1,phi(173,var(0))),
	cf(1,subtree(172,'V',-,33)),
	cf(1,phi(172,var(0))),
	cf(1,subtree(33,'V_BASE',-,34)),
	cf(1,phi(33,var(0))),
	cf(1,terminal(34,'qePa-2746-3',[21])),
	cf(1,phi(34,var(15))),
	cf(1,cproj(34,var(13))),
	cf(1,subtree(31,'V_SUFF_BASE',-,32)),
	cf(1,phi(31,var(0))),
	cf(1,terminal(32,'+V',[21])),
	cf(1,phi(32,var(0))),
	cf(1,subtree(29,'V_SUFF_BASE',-,30)),
	cf(1,phi(29,var(0))),
	cf(1,terminal(30,'+Unerg',[21])),
	cf(1,phi(30,var(0))),
	cf(1,subtree(27,'V_SUFF_BASE',-,28)),
	cf(1,phi(27,var(0))),
	cf(1,terminal(28,'+Aor',[21])),
	cf(1,phi(28,var(0))),
	cf(1,subtree(25,'V_SUFF_BASE',-,26)),
	cf(1,phi(25,var(0))),
	cf(1,terminal(26,'+Subj3Sg',[21])),
	cf(1,phi(26,var(0))),
	cf(1,subtree(23,'V_SUFF_BASE',-,24)),
	cf(1,phi(23,var(0))),
	cf(1,terminal(24,'+Obj3',[21])),
	cf(1,phi(24,var(0))),
	cf(1,subtree(38,'PERIOD',-,37)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(37,'.',[37])),
	cf(1,phi(37,var(0))),
	cf(1,semform_data(0,2,1,9)),
	cf(1,semform_data(2,33,10,14)),
	cf(1,fspan(var(0),1,16)),
	cf(1,fspan(var(3),1,9)),
	cf(1,surfaceform(1,'abramsma',1,9)),
	cf(1,surfaceform(21,'iqePa',10,15)),
	cf(1,surfaceform(37,'.',15,16))
	]).
