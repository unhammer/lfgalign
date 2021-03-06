% -*- coding: iso-8859-1 -*-

fstructure('aqePebuli jaGli brouns sdevda.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('4'),
	'statistics'('8+8 solutions, 0.15 CPU seconds, 141 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	choice([A1,A2], 1),
	choice([B1,B2], A1),
	choice([C1,C2], A2),
	choice([D1], or(C2,B2)),
	choice([E1,E2], B1),
	choice([F1,F2], C1),
	choice([G1,G2], E1),
	choice([H1,H2], F1),
	choice([I1], or(F2,G1)),
	choice([J1], or(H1,G2)),
	choice([K1], or(F2,H1,E1)),
	choice([L1], or(H2,E2)),
	choice([M1], or(or(C2,B2),I1)),
	choice([N1], or(C2,B2,K1))
	],
	% Equivalences:
	[
	define(CV_001, or(or(F1,E2,G2),or(D1,J1),L1)),
	define(CV_002, or(or(or(F2,H2),E2),G1,or(I1,D1),M1,L1)),
	define(CV_003, or(F1,E2,G2))
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(26))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(or(J1,L1),eq(attr(var(0),'OBJ'),var(17))),
	cf(1,eq(attr(var(0),'OBJth'),var(12))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(14))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(27))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(D1,eq(var(26),semform('devna',9,[var(3),var(12)],[]))),
	cf(I1,eq(var(26),semform('*-devna',12,[var(3),var(12)],[]))),
	cf(or(J1,L1),eq(var(26),semform('devna',9,[var(3),var(17),var(12)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('jaGli',5,[],[]))),
	cf(1,eq(attr(var(3),'ADJUNCT'),var(4))),
	cf(1,eq(attr(var(3),'CHECK'),var(10))),
	cf(1,eq(attr(var(3),'NTYPE'),var(11))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(A2,in_set(var(5),var(4))),
	cf(A1,in_set(var(7),var(4))),
	cf(A2,eq(attr(var(5),'PRED'),semform('aqePebuli',0,[],[]))),
	cf(A2,eq(attr(var(5),'CHECK'),var(6))),
	cf(A2,eq(attr(var(5),'ATYPE'),'attributive')),
	cf(A2,eq(attr(var(5),'CASE'),'nom')),
	cf(A2,eq(attr(var(5),'DEGREE'),'positive')),
	cf(A2,eq(attr(var(6),'_CASE-TYPE'),'reduced')),
	cf(A1,eq(attr(var(7),'PRED'),semform('a-qePeba',1,['NULL',var(3)],[]))),
	cf(A1,eq(attr(var(7),'SUBJ'),var(3))),
	cf(A1,eq(attr(var(7),'CHECK'),var(8))),
	cf(A1,eq(attr(var(7),'CASE'),'nom')),
	cf(A1,eq(attr(var(7),'NUM'),'sg')),
	cf(A1,eq(attr(var(7),'PASS'),'+')),
	cf(A1,eq(attr(var(8),'_MORPH-SYNT'),var(9))),
	cf(A1,eq(attr(var(8),'_CASE-TYPE'),'full')),
	cf(A1,eq(attr(var(9),'PARTICIPLE'),'past-part')),
	cf(A1,eq(attr(var(9),'_CLASS'),'T1')),
	cf(A1,eq(attr(var(9),'_LEXID'),'V2746-16')),
	cf(A1,eq(attr(var(9),'_PERF-PV'),'a')),
	cf(1,eq(attr(var(10),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(10),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(11),'NSYN'),'common')),
	cf(or(J1,L1),eq(attr(var(17),'PRED'),var(20))),
	cf(or(J1,L1),eq(attr(var(17),'CHECK'),var(18))),
	cf(J1,eq(attr(var(17),'NTYPE'),var(15))),
	cf(J1,eq(attr(var(17),'ANIM'),'+')),
	cf(or(J1,L1),eq(attr(var(17),'CASE'),'dat')),
	cf(J1,eq(attr(var(17),'NUM'),'sg')),
	cf(or(J1,L1),eq(attr(var(17),'PERS'),'3')),
	cf(J1,eq(var(20),semform('Browne',7,[],[]))),
	cf(L1,eq(var(20),semform('pro',17,[],[]))),
	cf(J1,eq(attr(var(18),'_POLARITY'),var(14))),
	cf(L1,eq(attr(var(18),'_PRODROP'),var(19))),
	cf(or(J1,L1),eq(attr(var(18),'_AGR-POS'),'left')),
	cf(L1,eq(var(19),'+')),
	cf(1,eq(attr(var(15),'NSYN'),'proper')),
	cf(1,eq(attr(var(12),'PRED'),var(16))),
	cf(1,eq(attr(var(12),'CHECK'),var(13))),
	cf(CV_002,eq(attr(var(12),'NTYPE'),var(15))),
	cf(CV_002,eq(attr(var(12),'ANIM'),'+')),
	cf(1,eq(attr(var(12),'CASE'),'dat')),
	cf(CV_002,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(CV_002,eq(var(16),semform('Browne',7,[],[]))),
	cf(J1,eq(var(16),semform('pro',18,[],[]))),
	cf(CV_002,eq(attr(var(13),'_POLARITY'),var(14))),
	cf(CV_002,eq(attr(var(13),'_AGR-POS'),'left')),
	cf(J1,eq(attr(var(13),'_PRODROP'),'+')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(21))),
	cf(1,eq(attr(var(1),'_TENSE'),var(25))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(CV_002,in_set(var(12),var(2))),
	cf(J1,in_set(var(17),var(2))),
	cf(1,eq(attr(var(21),'_AGR'),var(22))),
	cf(1,eq(attr(var(21),'_LEXID'),var(23))),
	cf(1,eq(attr(var(21),'_PERF-PV'),var(24))),
	cf(1,eq(attr(var(21),'_CLASS'),'T5')),
	cf(1,eq(attr(var(21),'_SYNTAX'),'trans')),
	cf(1,eq(attr(var(22),'_OBJ'),var(12))),
	cf(D1,eq(var(23),'V468-9')),
	cf(I1,eq(var(23),'V468-5/9/13-14')),
	cf(or(J1,L1),eq(var(23),'V468-19')),
	cf(CV_001,eq(var(24),'-')),
	cf(I1,eq(var(24),'*')),
	cf(D1,eq(var(25),'cond')),
	cf(or(or(I1,J1),L1),eq(var(25),'impf')),
	cf(1,eq(attr(var(27),'ASPECT'),var(28))),
	cf(1,eq(attr(var(27),'MOOD'),var(29))),
	cf(1,eq(attr(var(27),'TENSE'),var(30))),
	cf(or(or(I1,J1),L1),eq(var(28),'imperf')),
	cf(D1,eq(var(28),'perf')),
	cf(or(or(I1,J1),L1),eq(var(29),'indicative')),
	cf(D1,eq(var(29),'conditional')),
	cf(or(or(I1,J1),L1),eq(var(30),'past')),
	cf(D1,eq(var(30),'non-past')),
	cf(or(J1,L1),eq(proj(var(65),'o::'),var(66))),
	cf(or(J1,L1),in_set('NO-PV',var(66))),
	cf(or(J1,L1),in_set('OBJ-TH',var(66))),
	cf(I1,eq(proj(var(67),'o::'),var(68))),
	cf(I1,in_set('PV',var(68))),
	cf(D1,eq(proj(var(69),'o::'),var(70))),
	cf(D1,in_set('NO-PV',var(70))),
	cf(A2,eq(var(71),var(5))),
	cf(A1,eq(var(71),var(7))),
	cf(1,eq(attr(var(71),'CASE'),'nom')),
	cf(or(M1,L1),eq(var(72),var(12))),
	cf(J1,eq(var(72),var(17))),
	cf(1,eq(attr(var(72),'PRED'),semform('Browne',7,[],[]))),
	cf(1,eq(attr(var(72),'CHECK'),var(73))),
	cf(1,eq(attr(var(72),'NTYPE'),var(15))),
	cf(1,eq(attr(var(72),'ANIM'),'+')),
	cf(1,eq(attr(var(72),'CASE'),'dat')),
	cf(1,eq(attr(var(72),'NUM'),'sg')),
	cf(1,eq(attr(var(72),'PERS'),'3')),
	cf(or(M1,L1),eq(var(73),var(13))),
	cf(J1,eq(var(73),var(18))),
	cf(1,eq(attr(var(73),'_POLARITY'),var(14))),
	cf(1,eq(attr(var(73),'_AGR-POS'),'left')),
	cf(or(M1,L1),eq(var(74),var(75))),
	cf(J1,eq(var(74),var(76))),
	cf(or(M1,L1),eq(var(77),var(78))),
	cf(J1,eq(var(77),var(79)))
	],
	% C-Structure:
	[
	cf(1,subtree(1378,'ROOT',1375,106)),
	cf(1,phi(1378,var(0))),
	cf(1,subtree(1375,'ROOT',-,1272)),
	cf(1,phi(1375,var(0))),
	cf(1,subtree(1272,'IP[main,-]',1253,1059)),
	cf(1,phi(1272,var(0))),
	cf(1,subtree(1253,'IP[main,-]',-,726)),
	cf(1,phi(1253,var(0))),
	cf(D1,subtree(1196,'V',1195,88)),
	cf(D1,phi(1196,var(0))),
	cf(D1,subtree(1195,'V',1194,90)),
	cf(D1,phi(1195,var(0))),
	cf(D1,subtree(1194,'V',1192,92)),
	cf(D1,phi(1194,var(0))),
	cf(D1,subtree(1192,'V',-,96)),
	cf(D1,phi(1192,var(0))),
	cf(1,subtree(1059,'IPfoc[main,-]',982,1057)),
	cf(1,phi(1059,var(0))),
	cf(1,subtree(1057,'Ibar[main,-]',-,1040)),
	cf(1,phi(1057,var(0))),
	cf(1,subtree(1040,'I[main,-]',-,1037)),
	cf(1,phi(1040,var(0))),
	cf(1,subtree(1037,'V',1036,67)),
	cf(1,phi(1037,var(0))),
	cf(1,subtree(1036,'V',1035,69)),
	cf(1,phi(1036,var(0))),
	cf(D1,subtree(1035,'V',1196,86)),
	cf(D1,phi(1035,var(0))),
	cf(or(or(I1,J1),L1),subtree(1035,'V',1034,71)),
	cf(or(or(I1,J1),L1),phi(1035,var(0))),
	cf(or(or(I1,J1),L1),subtree(1034,'V',1033,73)),
	cf(or(or(I1,J1),L1),phi(1034,var(0))),
	cf(or(or(I1,J1),L1),subtree(1033,'V',1032,75)),
	cf(or(or(I1,J1),L1),phi(1033,var(0))),
	cf(or(or(I1,J1),L1),subtree(1032,'V',1030,77)),
	cf(or(or(I1,J1),L1),phi(1032,var(0))),
	cf(or(or(I1,J1),L1),subtree(1030,'V',-,81)),
	cf(or(or(I1,J1),L1),phi(1030,var(0))),
	cf(1,subtree(982,'IPfoc[main,-]',-,819)),
	cf(1,phi(982,var(0))),
	cf(1,subtree(819,'PROPP',-,65)),
	cf(1,phi(819,var(72))),
	cf(A1,subtree(730,'NP',-,724)),
	cf(A1,phi(730,var(3))),
	cf(A2,subtree(726,'NP',364,724)),
	cf(A2,phi(726,var(3))),
	cf(A1,subtree(726,'NP',241,730)),
	cf(A1,phi(726,var(3))),
	cf(1,subtree(724,'N',723,48)),
	cf(1,phi(724,var(3))),
	cf(1,subtree(723,'N',570,53)),
	cf(1,phi(723,var(3))),
	cf(1,subtree(570,'N',568,56)),
	cf(1,phi(570,var(3))),
	cf(1,subtree(568,'N',566,57)),
	cf(1,phi(568,var(3))),
	cf(1,subtree(566,'N',-,59)),
	cf(1,phi(566,var(3))),
	cf(A2,subtree(532,'A',376,35)),
	cf(A2,phi(532,var(5))),
	cf(A2,subtree(376,'A',374,31)),
	cf(A2,phi(376,var(5))),
	cf(A2,subtree(374,'A',373,37)),
	cf(A2,phi(374,var(5))),
	cf(A2,subtree(373,'A',-,39)),
	cf(A2,phi(373,var(5))),
	cf(A2,subtree(364,'NP',-,236)),
	cf(A2,phi(364,var(3))),
	cf(A1,subtree(241,'NP',-,227)),
	cf(A1,phi(241,var(3))),
	cf(A2,subtree(236,'AP',-,532)),
	cf(A2,phi(236,var(5))),
	cf(A1,subtree(227,'VPpart',-,225)),
	cf(A1,phi(227,var(7))),
	cf(A1,subtree(225,'Vpart',224,23)),
	cf(A1,phi(225,var(7))),
	cf(A1,subtree(224,'Vpart',223,28)),
	cf(A1,phi(224,var(7))),
	cf(A1,subtree(223,'Vpart',214,31)),
	cf(A1,phi(223,var(7))),
	cf(A1,subtree(214,'Vpart',213,10)),
	cf(A1,phi(214,var(7))),
	cf(A1,subtree(213,'Vpart',211,12)),
	cf(A1,phi(213,var(7))),
	cf(A1,subtree(211,'Vpart',-,14)),
	cf(A1,phi(211,var(7))),
	cf(1,subtree(106,'PERIOD',-,100)),
	cf(1,phi(106,var(0))),
	cf(1,terminal(100,'.',[100])),
	cf(1,phi(100,var(0))),
	cf(D1,subtree(96,'V_BASE',-,95)),
	cf(D1,phi(96,var(0))),
	cf(D1,terminal(95,'devna-468-9',[66])),
	cf(D1,phi(95,var(80))),
	cf(D1,cproj(95,var(69))),
	cf(D1,terminal(93,'+V',[66])),
	cf(D1,phi(93,var(0))),
	cf(D1,subtree(92,'V_SUFF_BASE',-,93)),
	cf(D1,phi(92,var(0))),
	cf(D1,terminal(91,'+Trans',[66])),
	cf(D1,phi(91,var(0))),
	cf(D1,subtree(90,'V_SUFF_BASE',-,91)),
	cf(D1,phi(90,var(0))),
	cf(D1,terminal(89,'+Base',[66])),
	cf(D1,phi(89,var(0))),
	cf(D1,subtree(88,'V_SUFF_BASE',-,89)),
	cf(D1,phi(88,var(0))),
	cf(D1,terminal(87,'+Cond',[66])),
	cf(D1,phi(87,var(0))),
	cf(D1,subtree(86,'V_SUFF_BASE',-,87)),
	cf(D1,phi(86,var(0))),
	cf(I1,terminal(85,'0-*-devna-468-5/9/13-14',[66])),
	cf(I1,phi(85,var(0))),
	cf(I1,cproj(85,var(67))),
	cf(I1,subtree(81,'V_BASE',-,85)),
	cf(I1,phi(81,var(0))),
	cf(or(J1,L1),subtree(81,'V_BASE',-,80)),
	cf(or(J1,L1),phi(81,var(0))),
	cf(or(J1,L1),terminal(80,'0-devna-468-19',[66])),
	cf(or(J1,L1),phi(80,var(0))),
	cf(or(J1,L1),cproj(80,var(65))),
	cf(or(or(I1,J1),L1),terminal(78,'+V',[66])),
	cf(or(or(I1,J1),L1),phi(78,var(0))),
	cf(or(or(I1,J1),L1),subtree(77,'V_SUFF_BASE',-,78)),
	cf(or(or(I1,J1),L1),phi(77,var(0))),
	cf(or(or(I1,J1),L1),terminal(76,'+Trans',[66])),
	cf(or(or(I1,J1),L1),phi(76,var(0))),
	cf(or(or(I1,J1),L1),subtree(75,'V_SUFF_BASE',-,76)),
	cf(or(or(I1,J1),L1),phi(75,var(0))),
	cf(or(or(I1,J1),L1),terminal(74,'+Base',[66])),
	cf(or(or(I1,J1),L1),phi(74,var(0))),
	cf(or(or(I1,J1),L1),subtree(73,'V_SUFF_BASE',-,74)),
	cf(or(or(I1,J1),L1),phi(73,var(0))),
	cf(or(or(I1,J1),L1),terminal(72,'+Impf',[66])),
	cf(or(or(I1,J1),L1),phi(72,var(0))),
	cf(or(or(I1,J1),L1),subtree(71,'V_SUFF_BASE',-,72)),
	cf(or(or(I1,J1),L1),phi(71,var(0))),
	cf(1,terminal(70,'+Subj3Sg',[66])),
	cf(1,phi(70,var(0))),
	cf(1,subtree(69,'V_SUFF_BASE',-,70)),
	cf(1,phi(69,var(0))),
	cf(1,terminal(68,'+Obj3',[66])),
	cf(1,phi(68,var(0))),
	cf(1,subtree(67,'V_SUFF_BASE',-,68)),
	cf(1,phi(67,var(0))),
	cf(1,subtree(65,'PROP',-,64)),
	cf(1,phi(65,var(72))),
	cf(1,terminal(64,'brouns',[64])),
	cf(1,phi(64,var(72))),
	cf(1,terminal(60,'jaGli',[44])),
	cf(1,phi(60,var(3))),
	cf(1,subtree(59,'N_BASE',-,60)),
	cf(1,phi(59,var(3))),
	cf(1,terminal(58,'+N',[44])),
	cf(1,phi(58,var(3))),
	cf(1,subtree(57,'N_SUFF_BASE',-,58)),
	cf(1,phi(57,var(3))),
	cf(1,subtree(56,'N_SUFF_BASE',-,55)),
	cf(1,phi(56,var(3))),
	cf(1,terminal(55,'+Nom',[44])),
	cf(1,phi(55,var(3))),
	cf(1,subtree(53,'N_SUFF_BASE',-,50)),
	cf(1,phi(53,var(3))),
	cf(1,terminal(50,'+Sg',[44])),
	cf(1,phi(50,var(3))),
	cf(1,subtree(48,'N_SUFF_BASE',-,46)),
	cf(1,phi(48,var(3))),
	cf(1,terminal(46,'+Full',[44])),
	cf(1,phi(46,var(3))),
	cf(A2,terminal(40,'aqePebuli',[1])),
	cf(A2,phi(40,var(5))),
	cf(A2,subtree(39,'A_BASE',-,40)),
	cf(A2,phi(39,var(5))),
	cf(A2,terminal(38,'+A',[1])),
	cf(A2,phi(38,var(5))),
	cf(A2,subtree(37,'A_SUFF_BASE',-,38)),
	cf(A2,phi(37,var(5))),
	cf(A2,subtree(35,'N_SUFF_BASE',-,33)),
	cf(A2,phi(35,var(5))),
	cf(A2,terminal(33,'+Reduced',[1])),
	cf(A2,phi(33,var(5))),
	cf(1,subtree(31,'N_SUFF_BASE',-,30)),
	cf(1,phi(31,var(71))),
	cf(1,terminal(30,'+Nom',[1])),
	cf(1,phi(30,var(71))),
	cf(A1,subtree(28,'N_SUFF_BASE',-,25)),
	cf(A1,phi(28,var(7))),
	cf(A1,terminal(25,'+Sg',[1])),
	cf(A1,phi(25,var(7))),
	cf(A1,subtree(23,'N_SUFF_BASE',-,21)),
	cf(A1,phi(23,var(7))),
	cf(A1,terminal(21,'+Full',[1])),
	cf(A1,phi(21,var(7))),
	cf(A1,terminal(15,'a-qePeba-2746-16',[1])),
	cf(A1,phi(15,var(81))),
	cf(A1,subtree(14,'Vpart_BASE',-,15)),
	cf(A1,phi(14,var(7))),
	cf(A1,terminal(13,'+VPart',[1])),
	cf(A1,phi(13,var(7))),
	cf(A1,subtree(12,'Vpart_SUFF_BASE',-,13)),
	cf(A1,phi(12,var(7))),
	cf(A1,terminal(11,'+PastPart',[1])),
	cf(A1,phi(11,var(7))),
	cf(A1,subtree(10,'Vpart_SUFF_BASE',-,11)),
	cf(A1,phi(10,var(7))),
	cf(A2,semform_data(0,39,1,9)),
	cf(A1,semform_data(1,14,1,9)),
	cf(1,semform_data(5,59,11,15)),
	cf(1,semform_data(7,65,17,23)),
	cf(D1,semform_data(9,96,24,29)),
	cf(or(J1,L1),semform_data(9,81,24,29)),
	cf(I1,semform_data(12,81,24,29)),
	cf(L1,semform_data(17,81,24,29)),
	cf(J1,semform_data(18,81,24,29)),
	cf(1,fspan(var(0),1,31)),
	cf(1,fspan(var(3),1,16)),
	cf(A2,fspan(var(5),1,10)),
	cf(A1,fspan(var(7),1,10)),
	cf(1,fspan(var(72),17,23)),
	cf(1,surfaceform(1,'aqePebuli',1,10)),
	cf(1,surfaceform(44,'jaGli',11,16)),
	cf(1,surfaceform(64,'brouns',17,23)),
	cf(1,surfaceform(66,'sdevda',24,30)),
	cf(1,surfaceform(100,'.',30,31))
	]).

