% -*- coding: iso-8859-1 -*-

fstructure('kata sdevda mas.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('4+4 solutions, 0.14 CPU seconds, 87 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	choice([A1,A2,A3], 1),
	choice([B1,B2], A1)
	],
	% Equivalences:
	[
	define(CV_001, or(B2,A1,A3))
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(22))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(or(A2,A3),eq(attr(var(0),'OBJ'),var(12))),
	cf(1,eq(attr(var(0),'OBJth'),var(7))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'INDIR-SPEECH'),var(9))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(23))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(B1,eq(var(22),semform('devna',2,[var(3),var(7)],[]))),
	cf(B2,eq(var(22),semform('*-devna',5,[var(3),var(7)],[]))),
	cf(or(A2,A3),eq(var(22),semform('devna',2,[var(3),var(12),var(7)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('kata',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(or(A2,A3),eq(attr(var(12),'PRED'),var(16))),
	cf(or(A2,A3),eq(attr(var(12),'CHECK'),var(13))),
	cf(A2,eq(attr(var(12),'NTYPE'),var(10))),
	cf(or(A2,A3),eq(attr(var(12),'CASE'),'dat')),
	cf(A2,eq(attr(var(12),'NUM'),'sg')),
	cf(or(A2,A3),eq(attr(var(12),'PERS'),'3')),
	cf(A2,eq(attr(var(12),'PRON-TYPE'),'pers')),
	cf(A2,eq(var(16),semform('is',12,[],[]))),
	cf(A3,eq(var(16),semform('pro',10,[],[]))),
	cf(or(A2,A3),eq(attr(var(13),'_AGR-POS'),var(14))),
	cf(A2,eq(attr(var(13),'_INDIR-SPEECH'),var(9))),
	cf(A3,eq(attr(var(13),'_PRODROP'),var(15))),
	cf(A3,eq(var(14),'left')),
	cf(A2,eq(var(14),'right')),
	cf(A3,eq(var(15),'+')),
	cf(1,eq(attr(var(10),'NSYN'),'pronoun')),
	cf(1,eq(attr(var(7),'PRED'),var(11))),
	cf(1,eq(attr(var(7),'CHECK'),var(8))),
	cf(CV_001,eq(attr(var(7),'NTYPE'),var(10))),
	cf(1,eq(attr(var(7),'CASE'),'dat')),
	cf(CV_001,eq(attr(var(7),'NUM'),'sg')),
	cf(1,eq(attr(var(7),'PERS'),'3')),
	cf(CV_001,eq(attr(var(7),'PRON-TYPE'),'pers')),
	cf(or(A1,A3),eq(var(11),semform('is',12,[],[]))),
	cf(A2,eq(var(11),semform('pro',11,[],[]))),
	cf(CV_001,eq(attr(var(8),'_INDIR-SPEECH'),var(9))),
	cf(CV_001,eq(attr(var(8),'_AGR-POS'),'right')),
	cf(A2,eq(attr(var(8),'_PRODROP'),'+')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(17))),
	cf(1,eq(attr(var(1),'_TENSE'),var(21))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(or(A1,A3),in_set(var(7),var(2))),
	cf(A2,in_set(var(12),var(2))),
	cf(1,eq(attr(var(17),'_AGR'),var(18))),
	cf(1,eq(attr(var(17),'_LEXID'),var(19))),
	cf(1,eq(attr(var(17),'_PERF-PV'),var(20))),
	cf(1,eq(attr(var(17),'_CLASS'),'T5')),
	cf(1,eq(attr(var(17),'_SYNTAX'),'trans')),
	cf(1,eq(attr(var(18),'_OBJ'),var(7))),
	cf(B1,eq(var(19),'V468-9')),
	cf(B2,eq(var(19),'V468-5/9/13-14')),
	cf(or(A2,A3),eq(var(19),'V468-19')),
	cf(or(or(B1,A2),A3),eq(var(20),'-')),
	cf(B2,eq(var(20),'*')),
	cf(B1,eq(var(21),'cond')),
	cf(or(or(B2,A2),A3),eq(var(21),'impf')),
	cf(1,eq(attr(var(23),'ASPECT'),var(24))),
	cf(1,eq(attr(var(23),'MOOD'),var(25))),
	cf(1,eq(attr(var(23),'TENSE'),var(26))),
	cf(or(or(B2,A2),A3),eq(var(24),'imperf')),
	cf(B1,eq(var(24),'perf')),
	cf(or(or(B2,A2),A3),eq(var(25),'indicative')),
	cf(B1,eq(var(25),'conditional')),
	cf(or(or(B2,A2),A3),eq(var(26),'past')),
	cf(B1,eq(var(26),'non-past')),
	cf(or(A2,A3),eq(proj(var(59),'o::'),var(60))),
	cf(or(A2,A3),in_set('NO-PV',var(60))),
	cf(or(A2,A3),in_set('OBJ-TH',var(60))),
	cf(B2,eq(proj(var(61),'o::'),var(62))),
	cf(B2,in_set('PV',var(62))),
	cf(B1,eq(proj(var(63),'o::'),var(64))),
	cf(B1,in_set('NO-PV',var(64))),
	cf(or(A1,A3),eq(var(65),var(7))),
	cf(A2,eq(var(65),var(12))),
	cf(1,eq(attr(var(65),'PRED'),semform('is',12,[],[]))),
	cf(1,eq(attr(var(65),'CHECK'),var(66))),
	cf(1,eq(attr(var(65),'NTYPE'),var(10))),
	cf(1,eq(attr(var(65),'CASE'),'dat')),
	cf(1,eq(attr(var(65),'NUM'),'sg')),
	cf(1,eq(attr(var(65),'PERS'),'3')),
	cf(1,eq(attr(var(65),'PRON-TYPE'),'pers')),
	cf(or(A1,A3),eq(var(66),var(8))),
	cf(A2,eq(var(66),var(13))),
	cf(1,eq(attr(var(66),'_INDIR-SPEECH'),var(9))),
	cf(1,eq(attr(var(66),'_AGR-POS'),'right')),
	cf(or(A1,A3),eq(var(67),var(68))),
	cf(A2,eq(var(67),var(69))),
	cf(or(A1,A3),eq(var(70),var(71))),
	cf(A2,eq(var(70),var(72)))
	],
	% C-Structure:
	[
	cf(1,subtree(1080,'ROOT',1077,77)),
	cf(1,phi(1080,var(0))),
	cf(1,subtree(1077,'ROOT',-,995)),
	cf(1,phi(1077,var(0))),
	cf(1,subtree(995,'IPfoc[main,-]',350,971)),
	cf(1,phi(995,var(0))),
	cf(1,subtree(971,'Ibar[main,-]',612,854)),
	cf(1,phi(971,var(0))),
	cf(1,subtree(854,'S',-,824)),
	cf(1,phi(854,var(0))),
	cf(1,subtree(824,'PRONP',-,822)),
	cf(1,phi(824,var(65))),
	cf(1,subtree(822,'PRON',670,52)),
	cf(1,phi(822,var(65))),
	cf(1,subtree(670,'PRON',668,59)),
	cf(1,phi(670,var(65))),
	cf(1,subtree(668,'PRON',667,60)),
	cf(1,phi(668,var(65))),
	cf(1,subtree(667,'PRON',666,64)),
	cf(1,phi(667,var(65))),
	cf(1,subtree(666,'PRON',665,66)),
	cf(1,phi(666,var(65))),
	cf(1,subtree(665,'PRON',-,70)),
	cf(1,phi(665,var(65))),
	cf(B1,subtree(660,'V',659,39)),
	cf(B1,phi(660,var(0))),
	cf(B1,subtree(659,'V',658,41)),
	cf(B1,phi(659,var(0))),
	cf(B1,subtree(658,'V',656,43)),
	cf(B1,phi(658,var(0))),
	cf(B1,subtree(656,'V',-,47)),
	cf(B1,phi(656,var(0))),
	cf(1,subtree(612,'Ibar[main,-]',-,506)),
	cf(1,phi(612,var(0))),
	cf(1,subtree(506,'I[main,-]',-,501)),
	cf(1,phi(506,var(0))),
	cf(1,subtree(501,'V',500,18)),
	cf(1,phi(501,var(0))),
	cf(1,subtree(500,'V',499,20)),
	cf(1,phi(500,var(0))),
	cf(B1,subtree(499,'V',660,37)),
	cf(B1,phi(499,var(0))),
	cf(or(or(B2,A2),A3),subtree(499,'V',498,22)),
	cf(or(or(B2,A2),A3),phi(499,var(0))),
	cf(or(or(B2,A2),A3),subtree(498,'V',497,24)),
	cf(or(or(B2,A2),A3),phi(498,var(0))),
	cf(or(or(B2,A2),A3),subtree(497,'V',496,26)),
	cf(or(or(B2,A2),A3),phi(497,var(0))),
	cf(or(or(B2,A2),A3),subtree(496,'V',494,28)),
	cf(or(or(B2,A2),A3),phi(496,var(0))),
	cf(or(or(B2,A2),A3),subtree(494,'V',-,32)),
	cf(or(or(B2,A2),A3),phi(494,var(0))),
	cf(1,subtree(350,'IPfoc[main,-]',-,340)),
	cf(1,phi(350,var(0))),
	cf(1,subtree(340,'NP',-,337)),
	cf(1,phi(340,var(3))),
	cf(1,subtree(337,'N',187,6)),
	cf(1,phi(337,var(3))),
	cf(1,subtree(187,'N',185,9)),
	cf(1,phi(187,var(3))),
	cf(1,subtree(185,'N',183,10)),
	cf(1,phi(185,var(3))),
	cf(1,subtree(183,'N',-,12)),
	cf(1,phi(183,var(3))),
	cf(1,subtree(77,'PERIOD',-,71)),
	cf(1,phi(77,var(0))),
	cf(1,terminal(71,'.',[71])),
	cf(1,phi(71,var(0))),
	cf(1,subtree(70,'PRON_BASE',-,69)),
	cf(1,phi(70,var(65))),
	cf(1,terminal(69,'is',[51])),
	cf(1,phi(69,var(65))),
	cf(1,terminal(67,'+Pron',[51])),
	cf(1,phi(67,var(65))),
	cf(1,subtree(66,'PRON_SUFF_BASE',-,67)),
	cf(1,phi(66,var(65))),
	cf(1,terminal(65,'+Pers',[51])),
	cf(1,phi(65,var(65))),
	cf(1,subtree(64,'PRON_SUFF_BASE',-,65)),
	cf(1,phi(64,var(65))),
	cf(1,terminal(61,'+Dat',[51])),
	cf(1,phi(61,var(65))),
	cf(1,subtree(60,'PRON_SUFF_BASE',-,61)),
	cf(1,phi(60,var(65))),
	cf(1,subtree(59,'PRON_SUFF_BASE',-,58)),
	cf(1,phi(59,var(65))),
	cf(1,terminal(58,'+3',[51])),
	cf(1,phi(58,var(65))),
	cf(1,terminal(53,'+Sg',[51])),
	cf(1,phi(53,var(65))),
	cf(1,subtree(52,'PRON_SUFF_BASE',-,53)),
	cf(1,phi(52,var(65))),
	cf(B1,subtree(47,'V_BASE',-,46)),
	cf(B1,phi(47,var(0))),
	cf(B1,terminal(46,'devna-468-9',[17])),
	cf(B1,phi(46,var(73))),
	cf(B1,cproj(46,var(63))),
	cf(B1,terminal(44,'+V',[17])),
	cf(B1,phi(44,var(0))),
	cf(B1,subtree(43,'V_SUFF_BASE',-,44)),
	cf(B1,phi(43,var(0))),
	cf(B1,terminal(42,'+Trans',[17])),
	cf(B1,phi(42,var(0))),
	cf(B1,subtree(41,'V_SUFF_BASE',-,42)),
	cf(B1,phi(41,var(0))),
	cf(B1,terminal(40,'+Base',[17])),
	cf(B1,phi(40,var(0))),
	cf(B1,subtree(39,'V_SUFF_BASE',-,40)),
	cf(B1,phi(39,var(0))),
	cf(B1,terminal(38,'+Cond',[17])),
	cf(B1,phi(38,var(0))),
	cf(B1,subtree(37,'V_SUFF_BASE',-,38)),
	cf(B1,phi(37,var(0))),
	cf(B2,terminal(36,'0-*-devna-468-5/9/13-14',[17])),
	cf(B2,phi(36,var(0))),
	cf(B2,cproj(36,var(61))),
	cf(B2,subtree(32,'V_BASE',-,36)),
	cf(B2,phi(32,var(0))),
	cf(or(A2,A3),subtree(32,'V_BASE',-,31)),
	cf(or(A2,A3),phi(32,var(0))),
	cf(or(A2,A3),terminal(31,'0-devna-468-19',[17])),
	cf(or(A2,A3),phi(31,var(0))),
	cf(or(A2,A3),cproj(31,var(59))),
	cf(or(or(B2,A2),A3),terminal(29,'+V',[17])),
	cf(or(or(B2,A2),A3),phi(29,var(0))),
	cf(or(or(B2,A2),A3),subtree(28,'V_SUFF_BASE',-,29)),
	cf(or(or(B2,A2),A3),phi(28,var(0))),
	cf(or(or(B2,A2),A3),terminal(27,'+Trans',[17])),
	cf(or(or(B2,A2),A3),phi(27,var(0))),
	cf(or(or(B2,A2),A3),subtree(26,'V_SUFF_BASE',-,27)),
	cf(or(or(B2,A2),A3),phi(26,var(0))),
	cf(or(or(B2,A2),A3),terminal(25,'+Base',[17])),
	cf(or(or(B2,A2),A3),phi(25,var(0))),
	cf(or(or(B2,A2),A3),subtree(24,'V_SUFF_BASE',-,25)),
	cf(or(or(B2,A2),A3),phi(24,var(0))),
	cf(or(or(B2,A2),A3),terminal(23,'+Impf',[17])),
	cf(or(or(B2,A2),A3),phi(23,var(0))),
	cf(or(or(B2,A2),A3),subtree(22,'V_SUFF_BASE',-,23)),
	cf(or(or(B2,A2),A3),phi(22,var(0))),
	cf(1,terminal(21,'+Subj3Sg',[17])),
	cf(1,phi(21,var(0))),
	cf(1,subtree(20,'V_SUFF_BASE',-,21)),
	cf(1,phi(20,var(0))),
	cf(1,terminal(19,'+Obj3',[17])),
	cf(1,phi(19,var(0))),
	cf(1,subtree(18,'V_SUFF_BASE',-,19)),
	cf(1,phi(18,var(0))),
	cf(1,terminal(13,'kata',[1])),
	cf(1,phi(13,var(3))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(3))),
	cf(1,terminal(11,'+N',[1])),
	cf(1,phi(11,var(3))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(3))),
	cf(1,subtree(9,'N_SUFF_BASE',-,8)),
	cf(1,phi(9,var(3))),
	cf(1,terminal(8,'+Nom',[1])),
	cf(1,phi(8,var(3))),
	cf(1,subtree(6,'N_SUFF_BASE',-,3)),
	cf(1,phi(6,var(3))),
	cf(1,terminal(3,'+Sg',[1])),
	cf(1,phi(3,var(3))),
	cf(1,semform_data(0,12,1,4)),
	cf(B1,semform_data(2,47,6,11)),
	cf(or(A2,A3),semform_data(2,32,6,11)),
	cf(B2,semform_data(5,32,6,11)),
	cf(A3,semform_data(10,32,6,11)),
	cf(A2,semform_data(11,32,6,11)),
	cf(1,semform_data(12,70,13,15)),
	cf(1,fspan(var(0),1,17)),
	cf(1,fspan(var(3),1,5)),
	cf(1,fspan(var(65),13,16)),
	cf(1,surfaceform(1,'kata',1,5)),
	cf(1,surfaceform(17,'sdevda',6,12)),
	cf(1,surfaceform(51,'mas',13,16)),
	cf(1,surfaceform(71,'.',16,17))
	]).

