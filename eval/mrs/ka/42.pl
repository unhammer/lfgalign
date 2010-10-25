% -*- coding: iso-8859-1 -*-

fstructure('jaGli qePda sustad.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('2+2 solutions, 0.04 CPU seconds, 76 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),var(19))),
	cf(1,eq(attr(var(0),'SUBJ'),var(8))),
	cf(A2,eq(attr(var(0),'OBJth'),var(17))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(6))),
	cf(1,eq(attr(var(0),'INDIR-SPEECH'),var(4))),
	cf(1,eq(attr(var(0),'POLARITY'),var(10))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(20))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(19),semform('*-qePa',2,[var(8)],[]))),
	cf(A2,eq(var(19),semform('*-qePa',2,[var(8),var(17)],[]))),
	cf(1,eq(attr(var(8),'PRED'),semform('jaGli',0,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(9))),
	cf(1,eq(attr(var(8),'NTYPE'),var(11))),
	cf(1,eq(attr(var(8),'CASE'),'nom')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'PERS'),'3')),
	cf(1,eq(attr(var(9),'_POLARITY'),var(10))),
	cf(1,eq(attr(var(9),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(9),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(11),'NSYN'),'common')),
	cf(A2,eq(attr(var(17),'PRED'),semform('pro',6,[],[]))),
	cf(A2,eq(attr(var(17),'CHECK'),var(18))),
	cf(A2,eq(attr(var(17),'CASE'),'dat')),
	cf(A2,eq(attr(var(17),'PERS'),'3')),
	cf(A2,eq(attr(var(18),'_PRODROP'),'+')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('susti',7,[],[]))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(2),'NTYPE'),var(5))),
	cf(1,eq(attr(var(2),'CASE'),'adv')),
	cf(1,eq(attr(var(2),'NUM'),'sg')),
	cf(1,eq(attr(var(2),'PERS'),'3')),
	cf(1,eq(attr(var(3),'_INDIR-SPEECH'),var(4))),
	cf(1,eq(attr(var(3),'_AGR-POS'),'right')),
	cf(1,eq(attr(var(3),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(5),'NSYN'),'common')),
	cf(1,eq(attr(var(6),'_IN-SITU'),var(7))),
	cf(1,eq(attr(var(6),'_MORPH-SYNT'),var(12))),
	cf(1,eq(attr(var(6),'_AGR'),'both')),
	cf(1,eq(attr(var(6),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(6),'_PERIOD'),'+')),
	cf(1,eq(attr(var(6),'_TENSE'),'impf')),
	cf(1,eq(attr(var(6),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(8),var(7))),
	cf(1,eq(attr(var(12),'_AGR'),var(13))),
	cf(1,eq(attr(var(12),'_CLASS'),var(15))),
	cf(1,eq(attr(var(12),'_LEXID'),var(16))),
	cf(1,eq(attr(var(12),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(12),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(13),'_OBJ'),var(14))),
	cf(A2,eq(var(14),var(17))),
	cf(A1,eq(attr(var(14),'NUM'),'sg')),
	cf(1,eq(attr(var(14),'PERS'),'3')),
	cf(A1,eq(var(15),'MV')),
	cf(A2,eq(var(15),'RM1')),
	cf(A1,eq(var(16),'V2746-3-7')),
	cf(A2,eq(var(16),'V2746-10-11')),
	cf(1,eq(attr(var(20),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(20),'MOOD'),'indicative')),
	cf(1,eq(attr(var(20),'TENSE'),'past')),
	cf(A2,eq(proj(var(39),'o::'),var(40))),
	cf(A2,in_set('PV',var(40))),
	cf(A1,eq(proj(var(41),'o::'),var(42))),
	cf(A1,in_set('PV',var(42)))
	],
	% C-Structure:
	[
	cf(1,subtree(1068,'ROOT',1065,68)),
	cf(1,phi(1068,var(0))),
	cf(1,subtree(1065,'ROOT',-,983)),
	cf(1,phi(1065,var(0))),
	cf(1,subtree(983,'IPfoc[main,-]',342,934)),
	cf(1,phi(983,var(0))),
	cf(1,subtree(938,'PP',-,809)),
	cf(1,phi(938,var(2))),
	cf(1,subtree(934,'Ibar[main,-]',604,824)),
	cf(1,phi(934,var(0))),
	cf(1,subtree(824,'S',-,938)),
	cf(1,phi(824,var(0))),
	cf(1,subtree(809,'NP',-,804)),
	cf(1,phi(809,var(2))),
	cf(1,subtree(804,'N',803,45)),
	cf(1,phi(804,var(2))),
	cf(1,subtree(803,'N',649,50)),
	cf(1,phi(803,var(2))),
	cf(1,subtree(649,'N',647,54)),
	cf(1,phi(649,var(2))),
	cf(1,subtree(647,'N',645,55)),
	cf(1,phi(647,var(2))),
	cf(1,subtree(645,'N',-,57)),
	cf(1,phi(645,var(2))),
	cf(1,subtree(604,'Ibar[main,-]',-,498)),
	cf(1,phi(604,var(0))),
	cf(1,subtree(498,'I[main,-]',-,493)),
	cf(1,phi(498,var(0))),
	cf(1,subtree(493,'V',492,22)),
	cf(1,phi(493,var(0))),
	cf(1,subtree(492,'V',491,24)),
	cf(1,phi(492,var(0))),
	cf(1,subtree(491,'V',490,26)),
	cf(1,phi(491,var(0))),
	cf(1,subtree(490,'V',489,28)),
	cf(1,phi(490,var(0))),
	cf(1,subtree(489,'V',488,30)),
	cf(1,phi(489,var(0))),
	cf(1,subtree(488,'V',486,32)),
	cf(1,phi(488,var(0))),
	cf(1,subtree(486,'V',-,36)),
	cf(1,phi(486,var(0))),
	cf(1,subtree(342,'IPfoc[main,-]',-,332)),
	cf(1,phi(342,var(0))),
	cf(1,subtree(332,'NP',-,329)),
	cf(1,phi(332,var(8))),
	cf(1,subtree(329,'N',328,5)),
	cf(1,phi(329,var(8))),
	cf(1,subtree(328,'N',178,10)),
	cf(1,phi(328,var(8))),
	cf(1,subtree(178,'N',176,13)),
	cf(1,phi(178,var(8))),
	cf(1,subtree(176,'N',174,14)),
	cf(1,phi(176,var(8))),
	cf(1,subtree(174,'N',-,16)),
	cf(1,phi(174,var(8))),
	cf(1,subtree(68,'PERIOD',-,62)),
	cf(1,phi(68,var(0))),
	cf(1,terminal(62,'.',[62])),
	cf(1,phi(62,var(0))),
	cf(1,terminal(58,'susti',[41])),
	cf(1,phi(58,var(2))),
	cf(1,subtree(57,'N_BASE',-,58)),
	cf(1,phi(57,var(2))),
	cf(1,terminal(56,'+N',[41])),
	cf(1,phi(56,var(2))),
	cf(1,subtree(55,'N_SUFF_BASE',-,56)),
	cf(1,phi(55,var(2))),
	cf(1,subtree(54,'N_SUFF_BASE',-,52)),
	cf(1,phi(54,var(2))),
	cf(1,terminal(52,'+Adv',[41])),
	cf(1,phi(52,var(2))),
	cf(1,subtree(50,'N_SUFF_BASE',-,47)),
	cf(1,phi(50,var(2))),
	cf(1,terminal(47,'+Sg',[41])),
	cf(1,phi(47,var(2))),
	cf(1,subtree(45,'N_SUFF_BASE',-,43)),
	cf(1,phi(45,var(2))),
	cf(1,terminal(43,'+Full',[41])),
	cf(1,phi(43,var(2))),
	cf(A1,terminal(40,'0-*-qePa-2746-3-7',[21])),
	cf(A1,phi(40,var(0))),
	cf(A1,cproj(40,var(41))),
	cf(A1,subtree(36,'V_BASE',-,40)),
	cf(A1,phi(36,var(0))),
	cf(A2,subtree(36,'V_BASE',-,35)),
	cf(A2,phi(36,var(0))),
	cf(A2,terminal(35,'0-*-qePa-2746-10-11',[21])),
	cf(A2,phi(35,var(0))),
	cf(A2,cproj(35,var(39))),
	cf(1,terminal(33,'+V',[21])),
	cf(1,phi(33,var(0))),
	cf(1,subtree(32,'V_SUFF_BASE',-,33)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'+Unerg',[21])),
	cf(1,phi(31,var(0))),
	cf(1,subtree(30,'V_SUFF_BASE',-,31)),
	cf(1,phi(30,var(0))),
	cf(1,terminal(29,'+Base',[21])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(28,'V_SUFF_BASE',-,29)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(27,'+Impf',[21])),
	cf(1,phi(27,var(0))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,terminal(25,'+Subj3Sg',[21])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(23,'+Obj3',[21])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(17,'jaGli',[1])),
	cf(1,phi(17,var(8))),
	cf(1,subtree(16,'N_BASE',-,17)),
	cf(1,phi(16,var(8))),
	cf(1,terminal(15,'+N',[1])),
	cf(1,phi(15,var(8))),
	cf(1,subtree(14,'N_SUFF_BASE',-,15)),
	cf(1,phi(14,var(8))),
	cf(1,subtree(13,'N_SUFF_BASE',-,12)),
	cf(1,phi(13,var(8))),
	cf(1,terminal(12,'+Nom',[1])),
	cf(1,phi(12,var(8))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(8))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(8))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(8))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(8))),
	cf(1,semform_data(0,16,1,5)),
	cf(A1,semform_data(2,36,7,11)),
	cf(A2,semform_data(2,36,7,11)),
	cf(A2,semform_data(6,36,7,11)),
	cf(1,semform_data(7,57,13,18)),
	cf(1,fspan(var(0),1,20)),
	cf(1,fspan(var(8),1,6)),
	cf(1,fspan(var(2),13,19)),
	cf(1,surfaceform(1,'jaGli',1,6)),
	cf(1,surfaceform(21,'qePda',7,12)),
	cf(1,surfaceform(41,'sustad',13,19)),
	cf(1,surfaceform(62,'.',19,20))
	]).
