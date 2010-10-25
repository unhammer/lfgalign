% -*- coding: iso-8859-1 -*-

fstructure('nuTu adamiani marTla ar aGemateboda raGaC manKanas?',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('7'),
	'statistics'('2+2 solutions, 0.09 CPU seconds, 157 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('mateba',5,[var(9),var(12)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(9))),
	cf(1,eq(attr(var(0),'OBJth'),var(12))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(7))),
	cf(1,eq(attr(var(0),'INDIR-SPEECH'),var(14))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(20))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'int')),
	cf(A2,eq(attr(var(0),'COMP-FORM'),'nuTu')),
	cf(1,eq(attr(var(0),'NEG'),'+')),
	cf(1,eq(attr(var(0),'POLARITY'),'neg')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(9),'PRED'),semform('adamiani',1,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(10))),
	cf(1,eq(attr(var(9),'NTYPE'),var(11))),
	cf(1,eq(attr(var(9),'ANIM'),'+')),
	cf(1,eq(attr(var(9),'CASE'),'nom')),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(10),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(11),'NSYN'),'common')),
	cf(1,eq(attr(var(12),'PRED'),semform('manKana',12,[],[]))),
	cf(1,eq(attr(var(12),'CHECK'),var(13))),
	cf(1,eq(attr(var(12),'NTYPE'),var(15))),
	cf(1,eq(attr(var(12),'SPEC'),var(16))),
	cf(1,eq(attr(var(12),'CASE'),'dat')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(13),'_INDIR-SPEECH'),var(14))),
	cf(1,eq(attr(var(13),'_AGR-POS'),'right')),
	cf(1,eq(attr(var(13),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(15),'NSYN'),'common')),
	cf(1,eq(attr(var(16),'DET'),var(17))),
	cf(1,eq(attr(var(17),'PRED'),semform('ra',8,[],[]))),
	cf(1,eq(attr(var(17),'DET-FORM'),'bound')),
	cf(1,eq(attr(var(17),'DET-TYPE'),'int')),
	cf(1,eq(attr(var(17),'MOD-SFX'),'Ga')),
	cf(1,eq(attr(var(17),'REL-SFX'),'C')),
	cf(1,in_set(var(2),var(1))),
	cf(1,in_set(var(4),var(1))),
	cf(A1,in_set(var(6),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('ar',4,[],[]))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(2),'ADV-TYPE'),'neg')),
	cf(1,eq(attr(var(3),'_PURE-NEG'),'+')),
	cf(1,eq(attr(var(4),'PRED'),semform('marTla',3,[],[]))),
	cf(1,eq(attr(var(4),'CHECK'),var(5))),
	cf(1,eq(attr(var(5),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(5),'_POLARITY'),'neg')),
	cf(A1,eq(attr(var(6),'PRED'),semform('nuTu',0,[],[]))),
	cf(1,eq(attr(var(7),'_IN-SITU'),var(8))),
	cf(1,eq(attr(var(7),'_MORPH-SYNT'),var(18))),
	cf(1,eq(attr(var(7),'_AGR'),'both')),
	cf(1,eq(attr(var(7),'_INT-MARK'),'+')),
	cf(1,eq(attr(var(7),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(7),'_TENSE'),'cond')),
	cf(1,eq(attr(var(7),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(9),var(8))),
	cf(1,in_set(var(12),var(8))),
	cf(1,eq(attr(var(18),'_AGR'),var(19))),
	cf(1,eq(attr(var(18),'_CLASS'),'RP1')),
	cf(1,eq(attr(var(18),'_LEXID'),'V1254-49')),
	cf(1,eq(attr(var(18),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(18),'_SYNTAX'),'unacc')),
	cf(1,eq(attr(var(19),'_OBJ'),var(12))),
	cf(1,eq(attr(var(20),'ASPECT'),'perf')),
	cf(1,eq(attr(var(20),'MOOD'),'conditional')),
	cf(1,eq(attr(var(20),'TENSE'),'non-past')),
	cf(1,eq(proj(var(33),'o::'),var(34))),
	cf(1,in_set('NO-PV',var(34)))
	],
	% C-Structure:
	[
	cf(1,subtree(1999,'ROOT',1995,113)),
	cf(1,phi(1999,var(0))),
	cf(A2,subtree(1995,'ROOT',-,1972)),
	cf(A2,phi(1995,var(0))),
	cf(A1,subtree(1995,'ROOT',-,1878)),
	cf(A1,phi(1995,var(0))),
	cf(A2,subtree(1972,'CPmain',215,1907)),
	cf(A2,phi(1972,var(0))),
	cf(A2,subtree(1907,'IP[sub,-]',508,1873)),
	cf(A2,phi(1907,var(0))),
	cf(A1,subtree(1878,'IP[main,-]',482,1877)),
	cf(A1,phi(1878,var(0))),
	cf(A1,subtree(1877,'IPfoc[main,-]',862,1676)),
	cf(A1,phi(1877,var(0))),
	cf(A2,subtree(1873,'IPfoc[sub,-]',604,1686)),
	cf(A2,phi(1873,var(0))),
	cf(A2,subtree(1686,'Ibar[sub,-]',1683,1410)),
	cf(A2,phi(1686,var(0))),
	cf(A2,subtree(1683,'Ibar[sub,-]',-,1669)),
	cf(A2,phi(1683,var(0))),
	cf(A1,subtree(1676,'Ibar[main,-]',1673,1410)),
	cf(A1,phi(1676,var(0))),
	cf(A1,subtree(1673,'Ibar[main,-]',-,1665)),
	cf(A1,phi(1673,var(0))),
	cf(A2,subtree(1669,'I[sub,-]',-,881)),
	cf(A2,phi(1669,var(0))),
	cf(A1,subtree(1665,'I[main,-]',-,881)),
	cf(A1,phi(1665,var(0))),
	cf(1,subtree(1544,'DPindef',968,1391)),
	cf(1,phi(1544,var(12))),
	cf(1,subtree(1410,'S',-,1544)),
	cf(1,phi(1410,var(0))),
	cf(1,subtree(1391,'NP',-,1386)),
	cf(1,phi(1391,var(12))),
	cf(1,subtree(1386,'N',1385,90)),
	cf(1,phi(1386,var(12))),
	cf(1,subtree(1385,'N',1222,95)),
	cf(1,phi(1385,var(12))),
	cf(1,subtree(1222,'N',1220,99)),
	cf(1,phi(1222,var(12))),
	cf(1,subtree(1220,'N',1218,100)),
	cf(1,phi(1220,var(12))),
	cf(1,subtree(1218,'N',-,102)),
	cf(1,phi(1218,var(12))),
	cf(1,subtree(968,'DPindef',-,60)),
	cf(1,phi(968,var(12))),
	cf(1,subtree(881,'Vneg',664,879)),
	cf(1,phi(881,var(0))),
	cf(1,subtree(879,'V',878,41)),
	cf(1,phi(879,var(0))),
	cf(1,subtree(878,'V',877,43)),
	cf(1,phi(878,var(0))),
	cf(1,subtree(877,'V',876,45)),
	cf(1,phi(877,var(0))),
	cf(1,subtree(876,'V',875,47)),
	cf(1,phi(876,var(0))),
	cf(1,subtree(875,'V',874,49)),
	cf(1,phi(875,var(0))),
	cf(1,subtree(874,'V',872,51)),
	cf(1,phi(874,var(0))),
	cf(1,subtree(872,'V',-,55)),
	cf(1,phi(872,var(0))),
	cf(A1,subtree(862,'IPfoc[main,-]',-,571)),
	cf(A1,phi(862,var(0))),
	cf(1,subtree(664,'Vneg',-,39)),
	cf(1,phi(664,var(0))),
	cf(A2,subtree(604,'IPfoc[sub,-]',-,571)),
	cf(A2,phi(604,var(0))),
	cf(1,subtree(571,'ADVP',-,33)),
	cf(1,phi(571,var(4))),
	cf(A2,subtree(508,'IP[sub,-]',-,476)),
	cf(A2,phi(508,var(0))),
	cf(A1,subtree(482,'IP[main,-]',209,476)),
	cf(A1,phi(482,var(0))),
	cf(1,subtree(476,'NP',-,473)),
	cf(1,phi(476,var(9))),
	cf(1,subtree(473,'N',472,8)),
	cf(1,phi(473,var(9))),
	cf(1,subtree(472,'N',327,13)),
	cf(1,phi(472,var(9))),
	cf(1,subtree(327,'N',325,16)),
	cf(1,phi(327,var(9))),
	cf(1,subtree(325,'N',324,19)),
	cf(1,phi(325,var(9))),
	cf(1,subtree(324,'N',322,20)),
	cf(1,phi(324,var(9))),
	cf(1,subtree(322,'N',-,22)),
	cf(1,phi(322,var(9))),
	cf(A2,subtree(215,'CPmain',-,3)),
	cf(A2,phi(215,var(0))),
	cf(A1,subtree(209,'IP[main,-]',-,2)),
	cf(A1,phi(209,var(0))),
	cf(1,subtree(113,'INT-MARK',-,107)),
	cf(1,phi(113,var(0))),
	cf(1,terminal(107,'?',[107])),
	cf(1,phi(107,var(0))),
	cf(1,terminal(103,'manKana',[86])),
	cf(1,phi(103,var(12))),
	cf(1,subtree(102,'N_BASE',-,103)),
	cf(1,phi(102,var(12))),
	cf(1,terminal(101,'+N',[86])),
	cf(1,phi(101,var(12))),
	cf(1,subtree(100,'N_SUFF_BASE',-,101)),
	cf(1,phi(100,var(12))),
	cf(1,subtree(99,'N_SUFF_BASE',-,97)),
	cf(1,phi(99,var(12))),
	cf(1,terminal(97,'+Dat',[86])),
	cf(1,phi(97,var(12))),
	cf(1,subtree(95,'N_SUFF_BASE',-,92)),
	cf(1,phi(95,var(12))),
	cf(1,terminal(92,'+Sg',[86])),
	cf(1,phi(92,var(12))),
	cf(1,subtree(90,'N_SUFF_BASE',-,88)),
	cf(1,phi(90,var(12))),
	cf(1,terminal(88,'+Full',[86])),
	cf(1,phi(88,var(12))),
	cf(1,subtree(60,'Dindef',-,59)),
	cf(1,phi(60,var(12))),
	cf(1,terminal(59,'raGaC',[59])),
	cf(1,phi(59,var(12))),
	cf(1,subtree(55,'V_BASE',-,54)),
	cf(1,phi(55,var(0))),
	cf(1,terminal(54,'mateba-1254-49',[40])),
	cf(1,phi(54,var(35))),
	cf(1,cproj(54,var(33))),
	cf(1,terminal(52,'+V',[40])),
	cf(1,phi(52,var(0))),
	cf(1,subtree(51,'V_SUFF_BASE',-,52)),
	cf(1,phi(51,var(0))),
	cf(1,terminal(50,'+Unacc',[40])),
	cf(1,phi(50,var(0))),
	cf(1,subtree(49,'V_SUFF_BASE',-,50)),
	cf(1,phi(49,var(0))),
	cf(1,terminal(48,'+Base',[40])),
	cf(1,phi(48,var(0))),
	cf(1,subtree(47,'V_SUFF_BASE',-,48)),
	cf(1,phi(47,var(0))),
	cf(1,terminal(46,'+Cond',[40])),
	cf(1,phi(46,var(0))),
	cf(1,subtree(45,'V_SUFF_BASE',-,46)),
	cf(1,phi(45,var(0))),
	cf(1,terminal(44,'+Subj3Sg',[40])),
	cf(1,phi(44,var(0))),
	cf(1,subtree(43,'V_SUFF_BASE',-,44)),
	cf(1,phi(43,var(0))),
	cf(1,terminal(42,'+Obj3',[40])),
	cf(1,phi(42,var(0))),
	cf(1,subtree(41,'V_SUFF_BASE',-,42)),
	cf(1,phi(41,var(0))),
	cf(1,subtree(39,'ADVneg',-,34)),
	cf(1,phi(39,var(2))),
	cf(1,terminal(34,'ar',[34])),
	cf(1,phi(34,var(2))),
	cf(1,subtree(33,'ADV',-,27)),
	cf(1,phi(33,var(4))),
	cf(1,terminal(27,'marTla',[27])),
	cf(1,phi(27,var(4))),
	cf(1,terminal(23,'adamiani',[4])),
	cf(1,phi(23,var(9))),
	cf(1,subtree(22,'N_BASE',-,23)),
	cf(1,phi(22,var(9))),
	cf(1,terminal(21,'+N',[4])),
	cf(1,phi(21,var(9))),
	cf(1,subtree(20,'N_SUFF_BASE',-,21)),
	cf(1,phi(20,var(9))),
	cf(1,subtree(19,'N_SUFF_BASE',-,18)),
	cf(1,phi(19,var(9))),
	cf(1,terminal(18,'+Anim',[4])),
	cf(1,phi(18,var(9))),
	cf(1,subtree(16,'N_SUFF_BASE',-,15)),
	cf(1,phi(16,var(9))),
	cf(1,terminal(15,'+Nom',[4])),
	cf(1,phi(15,var(9))),
	cf(1,subtree(13,'N_SUFF_BASE',-,10)),
	cf(1,phi(13,var(9))),
	cf(1,terminal(10,'+Sg',[4])),
	cf(1,phi(10,var(9))),
	cf(1,subtree(8,'N_SUFF_BASE',-,6)),
	cf(1,phi(8,var(9))),
	cf(1,terminal(6,'+Full',[4])),
	cf(1,phi(6,var(9))),
	cf(A2,subtree(3,'Cmain',-,1)),
	cf(A2,phi(3,var(0))),
	cf(A1,subtree(2,'ADVprt',-,1)),
	cf(A1,phi(2,var(6))),
	cf(A2,terminal(1,'nuTu',[1])),
	cf(A2,phi(1,var(0))),
	cf(A1,terminal(1,'nuTu',[1])),
	cf(A1,phi(1,var(6))),
	cf(A1,semform_data(0,2,1,5)),
	cf(1,semform_data(1,22,6,13)),
	cf(1,semform_data(3,33,15,21)),
	cf(1,semform_data(4,39,22,24)),
	cf(1,semform_data(5,55,25,35)),
	cf(1,semform_data(8,60,37,42)),
	cf(1,semform_data(12,102,43,49)),
	cf(1,fspan(var(0),1,52)),
	cf(1,fspan(var(9),6,14)),
	cf(A1,fspan(var(6),1,5)),
	cf(1,fspan(var(4),15,21)),
	cf(A1,fspan(var(0),37,51)),
	cf(1,fspan(var(2),22,24)),
	cf(1,fspan(var(12),37,51)),
	cf(1,surfaceform(1,'nuTu',1,5)),
	cf(1,surfaceform(4,'adamiani',6,14)),
	cf(1,surfaceform(27,'marTla',15,21)),
	cf(1,surfaceform(34,'ar',22,24)),
	cf(1,surfaceform(40,'aGemateboda',25,36)),
	cf(1,surfaceform(59,'raGaC',37,42)),
	cf(1,surfaceform(86,'manKanas',43,51)),
	cf(1,surfaceform(107,'?',51,52))
	]).
