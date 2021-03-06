% -*- coding: iso-8859-1 -*-

fstructure('TambaKos baGis jaGli qePda.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('4'),
	'statistics'('3+9 solutions, 0.05 CPU seconds, 182 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	choice([A1,A2,A3], 1)
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(24))),
	cf(1,eq(attr(var(0),'SUBJ'),var(7))),
	cf(or(A1,A3),eq(attr(var(0),'OBJth'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(25))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A2,eq(var(24),semform('*-qePa',9,[var(7)],[]))),
	cf(or(A1,A3),eq(var(24),semform('*-qePa',9,[var(7),var(3)],[]))),
	cf(1,eq(attr(var(7),'PRED'),semform('jaGli',7,[],[]))),
	cf(1,eq(attr(var(7),'CHECK'),var(8))),
	cf(1,eq(attr(var(7),'NTYPE'),var(10))),
	cf(1,eq(attr(var(7),'SPEC'),var(11))),
	cf(1,eq(attr(var(7),'CASE'),'nom')),
	cf(1,eq(attr(var(7),'NUM'),'sg')),
	cf(1,eq(attr(var(7),'PERS'),'3')),
	cf(1,eq(attr(var(8),'_POLARITY'),var(9))),
	cf(1,eq(attr(var(8),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(8),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(10),'NSYN'),'common')),
	cf(1,eq(attr(var(11),'POSS'),var(12))),
	cf(1,eq(attr(var(12),'PRED'),semform('baGi',4,[],[]))),
	cf(1,eq(attr(var(12),'CHECK'),var(13))),
	cf(1,eq(attr(var(12),'NTYPE'),var(14))),
	cf(or(A2,A3),eq(attr(var(12),'SPEC'),var(15))),
	cf(1,eq(attr(var(12),'CASE'),'gen')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(13),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(14),'NSYN'),'common')),
	cf(or(A2,A3),eq(attr(var(15),'POSS'),var(16))),
	cf(or(A2,A3),eq(attr(var(16),'PRED'),semform('TambaKo',0,[],[]))),
	cf(or(A2,A3),eq(attr(var(16),'CHECK'),var(17))),
	cf(or(A2,A3),eq(attr(var(16),'NTYPE'),var(18))),
	cf(or(A2,A3),eq(attr(var(16),'CASE'),'gen')),
	cf(or(A2,A3),eq(attr(var(16),'NUM'),'sg')),
	cf(or(A2,A3),eq(attr(var(16),'PERS'),'3')),
	cf(or(A2,A3),eq(attr(var(17),'_CASE-TYPE'),'full')),
	cf(A1,eq(var(49),var(50))),
	cf(A1,eq(var(51),var(52))),
	cf(or(A2,A3),eq(attr(var(18),'NSYN'),'common')),
	cf(or(A1,A3),eq(attr(var(3),'PRED'),var(6))),
	cf(or(A1,A3),eq(attr(var(3),'CHECK'),var(4))),
	cf(A1,eq(attr(var(3),'NTYPE'),var(5))),
	cf(or(A1,A3),eq(attr(var(3),'CASE'),'dat')),
	cf(A1,eq(attr(var(3),'NUM'),'sg')),
	cf(or(A1,A3),eq(attr(var(3),'PERS'),'3')),
	cf(A3,eq(var(6),semform('pro',13,[],[]))),
	cf(A1,eq(var(6),semform('TambaKo',0,[],[]))),
	cf(A1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(A1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(A3,eq(attr(var(4),'_PRODROP'),'+')),
	cf(or(A2,A3),eq(var(5),var(18))),
	cf(1,eq(attr(var(5),'NSYN'),'common')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(19))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'impf')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(A1,in_set(var(3),var(2))),
	cf(1,in_set(var(7),var(2))),
	cf(1,eq(attr(var(19),'_AGR'),var(20))),
	cf(1,eq(attr(var(19),'_CLASS'),var(22))),
	cf(1,eq(attr(var(19),'_LEXID'),var(23))),
	cf(1,eq(attr(var(19),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(19),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(20),'_OBJ'),var(21))),
	cf(or(A1,A3),eq(var(21),var(3))),
	cf(A2,eq(attr(var(21),'NUM'),'sg')),
	cf(1,eq(attr(var(21),'PERS'),'3')),
	cf(A2,eq(var(22),'MV')),
	cf(or(A1,A3),eq(var(22),'RM1')),
	cf(A2,eq(var(23),'V2746-3-7')),
	cf(or(A1,A3),eq(var(23),'V2746-10-11')),
	cf(1,eq(attr(var(25),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(25),'MOOD'),'indicative')),
	cf(1,eq(attr(var(25),'TENSE'),'past')),
	cf(or(A1,A3),eq(proj(var(42),'o::'),var(43))),
	cf(or(A1,A3),in_set('PV',var(43))),
	cf(A2,eq(proj(var(44),'o::'),var(45))),
	cf(A2,in_set('PV',var(45))),
	cf(or(A2,A3),eq(var(46),var(16))),
	cf(A1,eq(var(46),var(3))),
	cf(1,eq(attr(var(46),'PRED'),semform('TambaKo',0,[],[]))),
	cf(1,eq(attr(var(46),'CASE'),var(47))),
	cf(1,eq(attr(var(46),'CHECK'),var(48))),
	cf(1,eq(attr(var(46),'NTYPE'),var(5))),
	cf(1,eq(attr(var(46),'NUM'),'sg')),
	cf(1,eq(attr(var(46),'PERS'),'3')),
	cf(A1,eq(var(47),'dat')),
	cf(or(A2,A3),eq(var(47),'gen')),
	cf(or(A2,A3),eq(var(48),var(17))),
	cf(A1,eq(var(48),var(4))),
	cf(A1,eq(attr(var(48),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(48),'_CASE-TYPE'),'full'))
	],
	% C-Structure:
	[
	cf(1,subtree(1521,'ROOT',1518,90)),
	cf(1,phi(1521,var(0))),
	cf(or(A2,A3),subtree(1575,'POSSP',1372,981)),
	cf(or(A2,A3),phi(1575,var(7))),
	cf(or(A2,A3),subtree(1569,'POSSP',360,668)),
	cf(or(A2,A3),phi(1569,var(12))),
	cf(A1,subtree(1536,'POSSP',799,981)),
	cf(A1,phi(1536,var(7))),
	cf(or(A2,A3),subtree(1518,'ROOT',-,1496)),
	cf(or(A2,A3),phi(1518,var(0))),
	cf(A1,subtree(1518,'ROOT',-,1387)),
	cf(A1,phi(1518,var(0))),
	cf(or(A2,A3),subtree(1496,'IPfoc[main,-]',1476,1251)),
	cf(or(A2,A3),phi(1496,var(0))),
	cf(or(A2,A3),subtree(1476,'IPfoc[main,-]',-,1575)),
	cf(or(A2,A3),phi(1476,var(0))),
	cf(A1,subtree(1387,'IP[main,-]',365,1334)),
	cf(A1,phi(1387,var(0))),
	cf(or(A2,A3),subtree(1372,'POSSP',-,1569)),
	cf(or(A2,A3),phi(1372,var(7))),
	cf(A1,subtree(1334,'IPfoc[main,-]',1319,1251)),
	cf(A1,phi(1334,var(0))),
	cf(A1,subtree(1319,'IPfoc[main,-]',-,1536)),
	cf(A1,phi(1319,var(0))),
	cf(1,subtree(1251,'Ibar[main,-]',-,1148)),
	cf(1,phi(1251,var(0))),
	cf(1,subtree(1148,'I[main,-]',-,1143)),
	cf(1,phi(1148,var(0))),
	cf(1,subtree(1143,'V',1142,65)),
	cf(1,phi(1143,var(0))),
	cf(1,subtree(1142,'V',1141,67)),
	cf(1,phi(1142,var(0))),
	cf(1,subtree(1141,'V',1140,69)),
	cf(1,phi(1141,var(0))),
	cf(1,subtree(1140,'V',1139,71)),
	cf(1,phi(1140,var(0))),
	cf(1,subtree(1139,'V',1138,73)),
	cf(1,phi(1139,var(0))),
	cf(1,subtree(1138,'V',1136,75)),
	cf(1,phi(1138,var(0))),
	cf(1,subtree(1136,'V',-,79)),
	cf(1,phi(1136,var(0))),
	cf(1,subtree(981,'NP',-,977)),
	cf(1,phi(981,var(7))),
	cf(1,subtree(977,'N',976,48)),
	cf(1,phi(977,var(7))),
	cf(1,subtree(976,'N',824,53)),
	cf(1,phi(976,var(7))),
	cf(1,subtree(824,'N',822,56)),
	cf(1,phi(824,var(7))),
	cf(1,subtree(822,'N',820,57)),
	cf(1,phi(822,var(7))),
	cf(1,subtree(820,'N',-,59)),
	cf(1,phi(820,var(7))),
	cf(A1,subtree(799,'POSSP',-,668)),
	cf(A1,phi(799,var(7))),
	cf(1,subtree(668,'NP',-,662)),
	cf(1,phi(668,var(12))),
	cf(1,subtree(662,'N',661,27)),
	cf(1,phi(662,var(12))),
	cf(1,subtree(661,'N',509,32)),
	cf(1,phi(661,var(12))),
	cf(1,subtree(509,'N',507,36)),
	cf(1,phi(509,var(12))),
	cf(1,subtree(507,'N',505,37)),
	cf(1,phi(507,var(12))),
	cf(1,subtree(505,'N',-,39)),
	cf(1,phi(505,var(12))),
	cf(A1,subtree(365,'IP[main,-]',-,354)),
	cf(A1,phi(365,var(0))),
	cf(or(A2,A3),subtree(360,'POSSP',-,354)),
	cf(or(A2,A3),phi(360,var(12))),
	cf(1,subtree(354,'NP',-,351)),
	cf(1,phi(354,var(46))),
	cf(1,subtree(351,'N',350,5)),
	cf(1,phi(351,var(46))),
	cf(1,subtree(350,'N',200,10)),
	cf(1,phi(350,var(46))),
	cf(1,subtree(200,'N',198,14)),
	cf(1,phi(200,var(46))),
	cf(1,subtree(198,'N',196,15)),
	cf(1,phi(198,var(46))),
	cf(1,subtree(196,'N',-,17)),
	cf(1,phi(196,var(46))),
	cf(1,subtree(90,'PERIOD',-,84)),
	cf(1,phi(90,var(0))),
	cf(1,terminal(84,'.',[84])),
	cf(1,phi(84,var(0))),
	cf(A2,terminal(83,'0-*-qePa-2746-3-7',[64])),
	cf(A2,phi(83,var(0))),
	cf(A2,cproj(83,var(44))),
	cf(A2,subtree(79,'V_BASE',-,83)),
	cf(A2,phi(79,var(0))),
	cf(or(A1,A3),subtree(79,'V_BASE',-,78)),
	cf(or(A1,A3),phi(79,var(0))),
	cf(or(A1,A3),terminal(78,'0-*-qePa-2746-10-11',[64])),
	cf(or(A1,A3),phi(78,var(0))),
	cf(or(A1,A3),cproj(78,var(42))),
	cf(1,terminal(76,'+V',[64])),
	cf(1,phi(76,var(0))),
	cf(1,subtree(75,'V_SUFF_BASE',-,76)),
	cf(1,phi(75,var(0))),
	cf(1,terminal(74,'+Unerg',[64])),
	cf(1,phi(74,var(0))),
	cf(1,subtree(73,'V_SUFF_BASE',-,74)),
	cf(1,phi(73,var(0))),
	cf(1,terminal(72,'+Base',[64])),
	cf(1,phi(72,var(0))),
	cf(1,subtree(71,'V_SUFF_BASE',-,72)),
	cf(1,phi(71,var(0))),
	cf(1,terminal(70,'+Impf',[64])),
	cf(1,phi(70,var(0))),
	cf(1,subtree(69,'V_SUFF_BASE',-,70)),
	cf(1,phi(69,var(0))),
	cf(1,terminal(68,'+Subj3Sg',[64])),
	cf(1,phi(68,var(0))),
	cf(1,subtree(67,'V_SUFF_BASE',-,68)),
	cf(1,phi(67,var(0))),
	cf(1,terminal(66,'+Obj3',[64])),
	cf(1,phi(66,var(0))),
	cf(1,subtree(65,'V_SUFF_BASE',-,66)),
	cf(1,phi(65,var(0))),
	cf(1,terminal(60,'jaGli',[44])),
	cf(1,phi(60,var(7))),
	cf(1,subtree(59,'N_BASE',-,60)),
	cf(1,phi(59,var(7))),
	cf(1,terminal(58,'+N',[44])),
	cf(1,phi(58,var(7))),
	cf(1,subtree(57,'N_SUFF_BASE',-,58)),
	cf(1,phi(57,var(7))),
	cf(1,subtree(56,'N_SUFF_BASE',-,55)),
	cf(1,phi(56,var(7))),
	cf(1,terminal(55,'+Nom',[44])),
	cf(1,phi(55,var(7))),
	cf(1,subtree(53,'N_SUFF_BASE',-,50)),
	cf(1,phi(53,var(7))),
	cf(1,terminal(50,'+Sg',[44])),
	cf(1,phi(50,var(7))),
	cf(1,subtree(48,'N_SUFF_BASE',-,46)),
	cf(1,phi(48,var(7))),
	cf(1,terminal(46,'+Full',[44])),
	cf(1,phi(46,var(7))),
	cf(1,terminal(40,'baGi',[23])),
	cf(1,phi(40,var(12))),
	cf(1,subtree(39,'N_BASE',-,40)),
	cf(1,phi(39,var(12))),
	cf(1,terminal(38,'+N',[23])),
	cf(1,phi(38,var(12))),
	cf(1,subtree(37,'N_SUFF_BASE',-,38)),
	cf(1,phi(37,var(12))),
	cf(1,subtree(36,'N_SUFF_BASE',-,34)),
	cf(1,phi(36,var(12))),
	cf(1,terminal(34,'+Gen',[23])),
	cf(1,phi(34,var(12))),
	cf(1,subtree(32,'N_SUFF_BASE',-,29)),
	cf(1,phi(32,var(12))),
	cf(1,terminal(29,'+Sg',[23])),
	cf(1,phi(29,var(12))),
	cf(1,subtree(27,'N_SUFF_BASE',-,25)),
	cf(1,phi(27,var(12))),
	cf(1,terminal(25,'+Full',[23])),
	cf(1,phi(25,var(12))),
	cf(A1,terminal(22,'+Dat',[1])),
	cf(A1,phi(22,var(3))),
	cf(1,terminal(18,'TambaKo',[1])),
	cf(1,phi(18,var(46))),
	cf(1,subtree(17,'N_BASE',-,18)),
	cf(1,phi(17,var(46))),
	cf(1,terminal(16,'+N',[1])),
	cf(1,phi(16,var(46))),
	cf(1,subtree(15,'N_SUFF_BASE',-,16)),
	cf(1,phi(15,var(46))),
	cf(A1,subtree(14,'N_SUFF_BASE',-,22)),
	cf(A1,phi(14,var(3))),
	cf(or(A2,A3),subtree(14,'N_SUFF_BASE',-,12)),
	cf(or(A2,A3),phi(14,var(16))),
	cf(or(A2,A3),terminal(12,'+Gen',[1])),
	cf(or(A2,A3),phi(12,var(16))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(46))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(46))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(46))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(46))),
	cf(1,semform_data(0,17,1,7)),
	cf(1,semform_data(4,39,10,13)),
	cf(1,semform_data(7,59,16,20)),
	cf(A2,semform_data(9,79,22,26)),
	cf(or(A1,A3),semform_data(9,79,22,26)),
	cf(A3,semform_data(13,79,22,26)),
	cf(1,fspan(var(0),1,28)),
	cf(or(A2,A3),fspan(var(7),1,21)),
	cf(or(A2,A3),fspan(var(12),1,15)),
	cf(or(A2,A3),fspan(var(16),1,9)),
	cf(A1,fspan(var(46),1,9)),
	cf(A1,fspan(var(7),10,21)),
	cf(A1,fspan(var(12),10,15)),
	cf(1,surfaceform(1,'TambaKos',1,9)),
	cf(1,surfaceform(23,'baGis',10,15)),
	cf(1,surfaceform(44,'jaGli',16,21)),
	cf(1,surfaceform(64,'qePda',22,27)),
	cf(1,surfaceform(84,'.',27,28))
	]).

