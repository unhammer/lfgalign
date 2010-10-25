% -*- coding: iso-8859-1 -*-

fstructure('supermarketis Semdeg maTi gzebi gaiqo.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('5'),
	'statistics'('2+4 solutions, 0.14 CPU seconds, 172 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('ga-qoPa',12,[var(20),var(9)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(20))),
	cf(1,eq(attr(var(0),'OBJ'),var(9))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(7))),
	cf(1,eq(attr(var(0),'POLARITY'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(24))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(20),'PRED'),semform('pro',13,[],[]))),
	cf(1,eq(attr(var(20),'CHECK'),var(21))),
	cf(A1,eq(attr(var(20),'NUM'),var(22))),
	cf(1,eq(attr(var(20),'PERS'),var(23))),
	cf(1,eq(attr(var(20),'CASE'),'erg')),
	cf(1,eq(attr(var(21),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(21),'_PRODROP'),'+')),
	cf(A1,eq(var(22),'sg')),
	cf(A1,eq(var(23),'2')),
	cf(A2,eq(var(23),'3')),
	cf(1,eq(attr(var(9),'PRED'),semform('gza',9,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(10))),
	cf(1,eq(attr(var(9),'NTYPE'),var(12))),
	cf(1,eq(attr(var(9),'SPEC'),var(13))),
	cf(1,eq(attr(var(9),'CASE'),'nom')),
	cf(1,eq(attr(var(9),'NUM'),'pl')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'_POLARITY'),var(11))),
	cf(1,eq(attr(var(10),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(10),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(10),'_POSSP'),'+')),
	cf(1,eq(attr(var(12),'NSYN'),'common')),
	cf(1,eq(attr(var(13),'POSS'),var(14))),
	cf(1,eq(attr(var(14),'PRED'),semform('pro',5,[],[]))),
	cf(1,eq(attr(var(14),'NTYPE'),var(15))),
	cf(1,eq(attr(var(14),'CASE'),'nom')),
	cf(1,eq(attr(var(14),'NUM'),'pl')),
	cf(1,eq(attr(var(14),'PERS'),'3')),
	cf(1,eq(attr(var(15),'NSYN'),'pronoun')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('Semdeg',3,[var(4)],[]))),
	cf(1,eq(attr(var(2),'OBJ'),var(4))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(4),'PRED'),semform('supermarketi',0,[],[]))),
	cf(1,eq(attr(var(4),'CHECK'),var(5))),
	cf(1,eq(attr(var(4),'NTYPE'),var(6))),
	cf(1,eq(attr(var(4),'CASE'),'gen')),
	cf(1,eq(attr(var(4),'NUM'),'sg')),
	cf(1,eq(attr(var(4),'PERS'),'3')),
	cf(1,eq(attr(var(5),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(1,eq(attr(var(3),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(3),'_POSTP'),'Semdeg')),
	cf(1,eq(attr(var(7),'_IN-SITU'),var(8))),
	cf(1,eq(attr(var(7),'_MORPH-SYNT'),var(16))),
	cf(1,eq(attr(var(7),'_TENSE'),var(19))),
	cf(1,eq(attr(var(7),'_AGR'),'both')),
	cf(1,eq(attr(var(7),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(7),'_PERIOD'),'+')),
	cf(1,eq(attr(var(7),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(9),var(8))),
	cf(1,eq(attr(var(16),'_AGR'),var(17))),
	cf(1,eq(attr(var(16),'_CLASS'),'T2')),
	cf(1,eq(attr(var(16),'_LEXID'),'V2793-31')),
	cf(1,eq(attr(var(16),'_PERF-PV'),'ga')),
	cf(1,eq(attr(var(16),'_SYNTAX'),'trans')),
	cf(1,eq(attr(var(17),'_OBJ'),var(18))),
	cf(1,eq(attr(var(18),'PERS'),'3')),
	cf(A1,eq(var(19),'opt')),
	cf(A2,eq(var(19),'aor')),
	cf(1,eq(attr(var(24),'MOOD'),var(25))),
	cf(1,eq(attr(var(24),'TENSE'),var(26))),
	cf(1,eq(attr(var(24),'ASPECT'),'perf')),
	cf(A2,eq(var(25),'indicative')),
	cf(A1,eq(var(25),'subjunctive')),
	cf(A2,eq(var(26),'past')),
	cf(A1,eq(var(26),'non-past')),
	cf(1,eq(proj(var(39),'o::'),var(40))),
	cf(1,in_set('PV',var(40)))
	],
	% C-Structure:
	[
	cf(1,subtree(1341,'ROOT',1338,132)),
	cf(1,phi(1341,var(0))),
	cf(1,subtree(1353,'POSSP',646,833)),
	cf(1,phi(1353,var(9))),
	cf(1,subtree(1338,'ROOT',-,1259)),
	cf(1,phi(1338,var(0))),
	cf(1,subtree(1259,'IP[main,-]',1206,1197)),
	cf(1,phi(1259,var(0))),
	cf(1,subtree(1206,'IP[main,-]',-,821)),
	cf(1,phi(1206,var(0))),
	cf(1,subtree(1197,'IPfoc[main,-]',1178,1137)),
	cf(1,phi(1197,var(0))),
	cf(1,subtree(1178,'IPfoc[main,-]',-,1353)),
	cf(1,phi(1178,var(0))),
	cf(1,subtree(1153,'V',1152,116)),
	cf(1,phi(1153,var(0))),
	cf(1,subtree(1152,'V',1150,118)),
	cf(1,phi(1152,var(0))),
	cf(1,subtree(1150,'V',-,122)),
	cf(1,phi(1150,var(0))),
	cf(A1,subtree(1146,'V',991,112)),
	cf(A1,phi(1146,var(0))),
	cf(1,subtree(1137,'Ibar[main,-]',-,1005)),
	cf(1,phi(1137,var(0))),
	cf(1,subtree(1005,'I[main,-]',-,994)),
	cf(1,phi(1005,var(0))),
	cf(1,subtree(994,'V',993,92)),
	cf(1,phi(994,var(0))),
	cf(A1,subtree(993,'V',1146,110)),
	cf(A1,phi(993,var(0))),
	cf(A2,subtree(993,'V',992,94)),
	cf(A2,phi(993,var(0))),
	cf(A2,subtree(992,'V',991,96)),
	cf(A2,phi(992,var(0))),
	cf(1,subtree(991,'V',1153,114)),
	cf(1,phi(991,var(0))),
	cf(1,subtree(833,'NP',-,830)),
	cf(1,phi(833,var(9))),
	cf(1,subtree(830,'N',829,76)),
	cf(1,phi(830,var(9))),
	cf(1,subtree(829,'N',828,80)),
	cf(1,phi(829,var(9))),
	cf(1,subtree(828,'N',827,83)),
	cf(1,phi(828,var(9))),
	cf(1,subtree(827,'N',825,84)),
	cf(1,phi(827,var(9))),
	cf(1,subtree(825,'N',-,86)),
	cf(1,phi(825,var(9))),
	cf(1,subtree(821,'PP',423,819)),
	cf(1,phi(821,var(2))),
	cf(1,subtree(819,'P',818,30)),
	cf(1,phi(819,var(2))),
	cf(1,subtree(818,'P',-,29)),
	cf(1,phi(818,var(2))),
	cf(1,subtree(646,'POSSP',-,71)),
	cf(1,phi(646,var(9))),
	cf(1,subtree(423,'PP',-,396)),
	cf(1,phi(423,var(2))),
	cf(1,subtree(396,'NP',-,393)),
	cf(1,phi(396,var(4))),
	cf(1,subtree(393,'N',392,5)),
	cf(1,phi(393,var(4))),
	cf(1,subtree(392,'N',242,10)),
	cf(1,phi(392,var(4))),
	cf(1,subtree(242,'N',240,14)),
	cf(1,phi(242,var(4))),
	cf(1,subtree(240,'N',238,15)),
	cf(1,phi(240,var(4))),
	cf(1,subtree(238,'N',-,17)),
	cf(1,phi(238,var(4))),
	cf(1,subtree(132,'PERIOD',-,126)),
	cf(1,phi(132,var(0))),
	cf(1,terminal(126,'.',[126])),
	cf(1,phi(126,var(0))),
	cf(1,subtree(122,'V_BASE',-,121)),
	cf(1,phi(122,var(0))),
	cf(1,terminal(121,'ga-qoPa-2793-31',[91])),
	cf(1,phi(121,var(43))),
	cf(1,cproj(121,var(39))),
	cf(1,terminal(119,'+V',[91])),
	cf(1,phi(119,var(0))),
	cf(1,subtree(118,'V_SUFF_BASE',-,119)),
	cf(1,phi(118,var(0))),
	cf(1,terminal(117,'+Trans',[91])),
	cf(1,phi(117,var(0))),
	cf(1,subtree(116,'V_SUFF_BASE',-,117)),
	cf(1,phi(116,var(0))),
	cf(1,terminal(115,'+Base',[91])),
	cf(1,phi(115,var(0))),
	cf(1,subtree(114,'V_SUFF_BASE',-,115)),
	cf(1,phi(114,var(0))),
	cf(A1,terminal(113,'+Opt',[91])),
	cf(A1,phi(113,var(0))),
	cf(A1,subtree(112,'V_SUFF_BASE',-,113)),
	cf(A1,phi(112,var(0))),
	cf(A1,terminal(111,'+Subj2Sg',[91])),
	cf(A1,phi(111,var(0))),
	cf(A1,subtree(110,'V_SUFF_BASE',-,111)),
	cf(A1,phi(110,var(0))),
	cf(A2,terminal(97,'+Aor',[91])),
	cf(A2,phi(97,var(0))),
	cf(A2,subtree(96,'V_SUFF_BASE',-,97)),
	cf(A2,phi(96,var(0))),
	cf(A2,terminal(95,'+Subj3Sg',[91])),
	cf(A2,phi(95,var(0))),
	cf(A2,subtree(94,'V_SUFF_BASE',-,95)),
	cf(A2,phi(94,var(0))),
	cf(1,terminal(93,'+Obj3',[91])),
	cf(1,phi(93,var(0))),
	cf(1,subtree(92,'V_SUFF_BASE',-,93)),
	cf(1,phi(92,var(0))),
	cf(1,terminal(87,'gza',[72])),
	cf(1,phi(87,var(9))),
	cf(1,subtree(86,'N_BASE',-,87)),
	cf(1,phi(86,var(9))),
	cf(1,terminal(85,'+N',[72])),
	cf(1,phi(85,var(9))),
	cf(1,subtree(84,'N_SUFF_BASE',-,85)),
	cf(1,phi(84,var(9))),
	cf(1,subtree(83,'N_SUFF_BASE',-,82)),
	cf(1,phi(83,var(9))),
	cf(1,terminal(82,'+Nom',[72])),
	cf(1,phi(82,var(9))),
	cf(1,subtree(80,'N_SUFF_BASE',-,78)),
	cf(1,phi(80,var(9))),
	cf(1,terminal(78,'+Pl',[72])),
	cf(1,phi(78,var(9))),
	cf(1,subtree(76,'N_SUFF_BASE',-,74)),
	cf(1,phi(76,var(9))),
	cf(1,terminal(74,'+Full',[72])),
	cf(1,phi(74,var(9))),
	cf(1,subtree(71,'POSS',-,32)),
	cf(1,phi(71,var(14))),
	cf(1,terminal(32,'maTi',[32])),
	cf(1,phi(32,var(14))),
	cf(1,terminal(31,'+Pp',[22])),
	cf(1,phi(31,var(2))),
	cf(1,subtree(30,'P_SUFF_BASE',-,31)),
	cf(1,phi(30,var(2))),
	cf(1,subtree(29,'P_BASE',-,28)),
	cf(1,phi(29,var(2))),
	cf(1,terminal(28,'Semdeg',[22])),
	cf(1,phi(28,var(2))),
	cf(1,terminal(18,'supermarketi',[1])),
	cf(1,phi(18,var(4))),
	cf(1,subtree(17,'N_BASE',-,18)),
	cf(1,phi(17,var(4))),
	cf(1,terminal(16,'+N',[1])),
	cf(1,phi(16,var(4))),
	cf(1,subtree(15,'N_SUFF_BASE',-,16)),
	cf(1,phi(15,var(4))),
	cf(1,subtree(14,'N_SUFF_BASE',-,12)),
	cf(1,phi(14,var(4))),
	cf(1,terminal(12,'+Gen',[1])),
	cf(1,phi(12,var(4))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(4))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(4))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(4))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(4))),
	cf(1,semform_data(0,17,1,12)),
	cf(1,semform_data(3,29,15,20)),
	cf(1,semform_data(5,71,22,26)),
	cf(1,semform_data(9,86,27,31)),
	cf(1,semform_data(12,122,33,37)),
	cf(1,semform_data(13,122,33,37)),
	cf(1,fspan(var(0),1,39)),
	cf(1,fspan(var(2),1,21)),
	cf(1,fspan(var(4),1,14)),
	cf(1,fspan(var(9),22,32)),
	cf(1,fspan(var(14),22,26)),
	cf(1,surfaceform(1,'supermarketis',1,14)),
	cf(1,surfaceform(22,'Semdeg',15,21)),
	cf(1,surfaceform(32,'maTi',22,26)),
	cf(1,surfaceform(72,'gzebi',27,32)),
	cf(1,surfaceform(91,'gaiqo',33,38)),
	cf(1,surfaceform(126,'.',38,39))
	]).
