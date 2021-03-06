% -*- coding: iso-8859-1 -*-

fstructure('katebi qePen.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('2'),
	'statistics'('2+2 solutions, 0.03 CPU seconds, 53 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	choice([A1,A2], 1)
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(14))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(A2,eq(attr(var(0),'OBJth'),var(12))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(15))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(14),semform('*-qePa',2,[var(3)],[]))),
	cf(A2,eq(var(14),semform('*-qePa',2,[var(3),var(12)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('kata',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'pl')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(A2,eq(attr(var(12),'PRED'),semform('pro',6,[],[]))),
	cf(A2,eq(attr(var(12),'CHECK'),var(13))),
	cf(A2,eq(attr(var(12),'CASE'),'dat')),
	cf(A2,eq(attr(var(12),'PERS'),'3')),
	cf(A2,eq(attr(var(13),'_PRODROP'),'+')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(7))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'pres')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(7),'_AGR'),var(8))),
	cf(1,eq(attr(var(7),'_CLASS'),var(10))),
	cf(1,eq(attr(var(7),'_LEXID'),var(11))),
	cf(1,eq(attr(var(7),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(7),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(8),'_OBJ'),var(9))),
	cf(A2,eq(var(9),var(12))),
	cf(A1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(A1,eq(var(10),'MV')),
	cf(A2,eq(var(10),'RM1')),
	cf(A1,eq(var(11),'V2746-3-7')),
	cf(A2,eq(var(11),'V2746-10-11')),
	cf(1,eq(attr(var(15),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(15),'MOOD'),'indicative')),
	cf(1,eq(attr(var(15),'TENSE'),'pres')),
	cf(A2,eq(proj(var(30),'o::'),var(31))),
	cf(A2,in_set('PV',var(31))),
	cf(A1,eq(proj(var(32),'o::'),var(33))),
	cf(A1,in_set('PV',var(33)))
	],
	% C-Structure:
	[
	cf(1,subtree(490,'ROOT',487,46)),
	cf(1,phi(490,var(0))),
	cf(1,subtree(487,'ROOT',-,434)),
	cf(1,phi(487,var(0))),
	cf(1,subtree(434,'IPfoc[main,-]',170,431)),
	cf(1,phi(434,var(0))),
	cf(1,subtree(431,'Ibar[main,-]',-,326)),
	cf(1,phi(431,var(0))),
	cf(1,subtree(326,'I[main,-]',-,321)),
	cf(1,phi(326,var(0))),
	cf(1,subtree(321,'V',320,21)),
	cf(1,phi(321,var(0))),
	cf(1,subtree(320,'V',319,23)),
	cf(1,phi(320,var(0))),
	cf(1,subtree(319,'V',318,25)),
	cf(1,phi(319,var(0))),
	cf(1,subtree(318,'V',317,27)),
	cf(1,phi(318,var(0))),
	cf(1,subtree(317,'V',316,29)),
	cf(1,phi(317,var(0))),
	cf(1,subtree(316,'V',314,31)),
	cf(1,phi(316,var(0))),
	cf(1,subtree(314,'V',-,35)),
	cf(1,phi(314,var(0))),
	cf(1,subtree(170,'IPfoc[main,-]',-,160)),
	cf(1,phi(170,var(0))),
	cf(1,subtree(160,'NP',-,157)),
	cf(1,phi(160,var(3))),
	cf(1,subtree(157,'N',156,5)),
	cf(1,phi(157,var(3))),
	cf(1,subtree(156,'N',155,9)),
	cf(1,phi(156,var(3))),
	cf(1,subtree(155,'N',154,12)),
	cf(1,phi(155,var(3))),
	cf(1,subtree(154,'N',152,13)),
	cf(1,phi(154,var(3))),
	cf(1,subtree(152,'N',-,15)),
	cf(1,phi(152,var(3))),
	cf(1,subtree(46,'PERIOD',-,40)),
	cf(1,phi(46,var(0))),
	cf(1,terminal(40,'.',[40])),
	cf(1,phi(40,var(0))),
	cf(A1,terminal(39,'0-*-qePa-2746-3-7',[20])),
	cf(A1,phi(39,var(0))),
	cf(A1,cproj(39,var(32))),
	cf(A1,subtree(35,'V_BASE',-,39)),
	cf(A1,phi(35,var(0))),
	cf(A2,subtree(35,'V_BASE',-,34)),
	cf(A2,phi(35,var(0))),
	cf(A2,terminal(34,'0-*-qePa-2746-10-11',[20])),
	cf(A2,phi(34,var(0))),
	cf(A2,cproj(34,var(30))),
	cf(1,terminal(32,'+V',[20])),
	cf(1,phi(32,var(0))),
	cf(1,subtree(31,'V_SUFF_BASE',-,32)),
	cf(1,phi(31,var(0))),
	cf(1,terminal(30,'+Unerg',[20])),
	cf(1,phi(30,var(0))),
	cf(1,subtree(29,'V_SUFF_BASE',-,30)),
	cf(1,phi(29,var(0))),
	cf(1,terminal(28,'+Base',[20])),
	cf(1,phi(28,var(0))),
	cf(1,subtree(27,'V_SUFF_BASE',-,28)),
	cf(1,phi(27,var(0))),
	cf(1,terminal(26,'+Pres',[20])),
	cf(1,phi(26,var(0))),
	cf(1,subtree(25,'V_SUFF_BASE',-,26)),
	cf(1,phi(25,var(0))),
	cf(1,terminal(24,'+Subj3Pl',[20])),
	cf(1,phi(24,var(0))),
	cf(1,subtree(23,'V_SUFF_BASE',-,24)),
	cf(1,phi(23,var(0))),
	cf(1,terminal(22,'+Obj3',[20])),
	cf(1,phi(22,var(0))),
	cf(1,subtree(21,'V_SUFF_BASE',-,22)),
	cf(1,phi(21,var(0))),
	cf(1,terminal(16,'kata',[1])),
	cf(1,phi(16,var(3))),
	cf(1,subtree(15,'N_BASE',-,16)),
	cf(1,phi(15,var(3))),
	cf(1,terminal(14,'+N',[1])),
	cf(1,phi(14,var(3))),
	cf(1,subtree(13,'N_SUFF_BASE',-,14)),
	cf(1,phi(13,var(3))),
	cf(1,subtree(12,'N_SUFF_BASE',-,11)),
	cf(1,phi(12,var(3))),
	cf(1,terminal(11,'+Nom',[1])),
	cf(1,phi(11,var(3))),
	cf(1,subtree(9,'N_SUFF_BASE',-,7)),
	cf(1,phi(9,var(3))),
	cf(1,terminal(7,'+Pl',[1])),
	cf(1,phi(7,var(3))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(3))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(3))),
	cf(1,semform_data(0,15,1,6)),
	cf(A1,semform_data(2,35,8,12)),
	cf(A2,semform_data(2,35,8,12)),
	cf(A2,semform_data(6,35,8,12)),
	cf(1,fspan(var(0),1,14)),
	cf(1,fspan(var(3),1,7)),
	cf(1,surfaceform(1,'katebi',1,7)),
	cf(1,surfaceform(20,'qePen',8,13)),
	cf(1,surfaceform(40,'.',13,14))
	]).

