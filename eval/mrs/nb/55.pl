% -*- coding: iso-8859-1 -*-

fstructure('Hunder jager hva som helst som bjeffer.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.31 CPU seconds, 267 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('25'),
	'max_medial2_constituent_weight'('20')
	],
	% Choices:
	[
	
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('jage',3,[var(7),var(8)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(7))),
	cf(1,eq(attr(var(0),'OBJ'),var(8))),
	cf(1,eq(attr(var(0),'TOPIC'),var(7))),
	cf(1,eq(attr(var(0),'CHECK'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(proj(var(0),'m::'),var(11))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(7),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(7),'CHECK'),var(12))),
	cf(1,eq(attr(var(7),'GEND'),var(13))),
	cf(1,eq(attr(var(7),'NTYPE'),var(14))),
	cf(1,eq(proj(var(7),'m::'),var(15))),
	cf(1,eq(attr(var(7),'CASE'),'nom')),
	cf(1,eq(attr(var(7),'NUM'),'pl')),
	cf(1,eq(attr(var(7),'PERS'),'3')),
	cf(1,eq(attr(var(12),'_SEL'),var(16))),
	cf(1,eq(attr(var(12),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(12),'_NOUN'),'+')),
	cf(1,eq(attr(var(16),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(16),'_ANIM'),'+')),
	cf(1,eq(attr(var(16),'_HUMAN'),'-')),
	cf(1,eq(attr(var(13),'FEM'),'-')),
	cf(1,eq(attr(var(13),'MASC'),'+')),
	cf(1,eq(attr(var(13),'NEUT'),'-')),
	cf(1,eq(attr(var(14),'NSEM'),var(17))),
	cf(1,eq(attr(var(14),'NSYN'),'common')),
	cf(1,eq(attr(var(17),'COMMON'),'count')),
	cf(1,eq(attr(var(15),'H-CONS'),var(18))),
	cf(1,eq(attr(var(15),'RELS'),var(19))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(20))),
	cf(1,eq(attr(var(15),'TOP'),var(21))),
	cf(1,eq(attr(var(15),'_QUANT'),var(22))),
	cf(1,in_set(var(1),var(18))),
	cf(1,in_set(var(2),var(18))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(23))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(24))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(23),'type'),'handle')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(25))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(26))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,in_set(var(20),var(19))),
	cf(1,eq(attr(var(20),'ARG0'),var(27))),
	cf(1,eq(attr(var(20),'LBL'),var(23))),
	cf(1,eq(attr(var(20),'_CAT'),'n')),
	cf(1,eq(attr(var(20),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(27),'DIV'),'+')),
	cf(1,eq(attr(var(27),'GRIND'),'-')),
	cf(1,eq(attr(var(27),'NATGEND'),'gender')),
	cf(1,eq(attr(var(27),'NUM'),'pl')),
	cf(1,eq(attr(var(27),'PERS'),'3')),
	cf(1,eq(attr(var(27),'type'),'ref-ind')),
	cf(1,eq(attr(var(21),'type'),'handle')),
	cf(1,eq(attr(var(22),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(22),'TOP'),var(28))),
	cf(1,eq(attr(var(6),'ARG0'),var(27))),
	cf(1,eq(attr(var(6),'BODY'),var(29))),
	cf(1,eq(attr(var(6),'LBL'),var(21))),
	cf(1,eq(attr(var(6),'RSTR'),var(24))),
	cf(1,eq(attr(var(6),'relation'),semform('bare_div_q',2,[],[]))),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(8),'PRED'),semform('pro',5,[],[]))),
	cf(1,eq(attr(var(8),'ADJUNCT'),var(30))),
	cf(1,eq(attr(var(8),'CHECK'),var(31))),
	cf(1,eq(attr(var(8),'GEND'),var(32))),
	cf(1,eq(attr(var(8),'NTYPE'),var(33))),
	cf(1,eq(attr(var(8),'NUM'),var(34))),
	cf(1,eq(proj(var(8),'m::'),var(35))),
	cf(1,eq(attr(var(8),'CASE'),'obl')),
	cf(1,eq(attr(var(8),'PERS'),'3')),
	cf(1,eq(attr(var(8),'PRON-TYPE'),'pers')),
	cf(1,eq(attr(var(8),'REF'),'+')),
	cf(1,in_set(var(36),var(30))),
	cf(1,eq(attr(var(36),'PRED'),semform('bjeffe',10,[var(37)],[]))),
	cf(1,eq(attr(var(36),'SUBJ'),var(37))),
	cf(1,eq(attr(var(36),'CHECK'),var(38))),
	cf(1,eq(attr(var(36),'TNS-ASP'),var(39))),
	cf(1,eq(attr(var(36),'TOPIC-REL'),var(37))),
	cf(1,eq(proj(var(36),'m::'),var(40))),
	cf(1,eq(attr(var(36),'CLAUSE-TYPE'),'rel')),
	cf(1,eq(attr(var(36),'COMP-FORM'),'som')),
	cf(1,eq(attr(var(36),'RESTR'),'+')),
	cf(1,eq(attr(var(36),'VFORM'),'fin')),
	cf(1,eq(attr(var(36),'VTYPE'),'main')),
	cf(1,eq(attr(var(37),'PRED'),semform('pro',8,[],[]))),
	cf(1,eq(attr(var(37),'GEND'),var(32))),
	cf(1,eq(attr(var(37),'NUM'),var(34))),
	cf(1,eq(proj(var(37),'m::'),var(41))),
	cf(1,eq(attr(var(37),'PERS'),'3')),
	cf(1,eq(attr(var(41),'H-CONS'),var(42))),
	cf(1,eq(attr(var(41),'RELS_EL'),var(43))),
	cf(1,eq(attr(var(43),'ARG0'),var(44))),
	cf(1,eq(attr(var(44),'NUM'),var(34))),
	cf(1,eq(attr(var(44),'NATGEND'),'n')),
	cf(1,eq(attr(var(44),'PERS'),'3')),
	cf(1,eq(attr(var(44),'type'),'ref-ind')),
	cf(1,eq(attr(var(38),'_MAIN-CL'),'-')),
	cf(1,eq(attr(var(39),'MOOD'),'indicative')),
	cf(1,eq(attr(var(39),'TENSE'),'pres')),
	cf(1,eq(attr(var(40),'H-CONS'),var(42))),
	cf(1,eq(attr(var(40),'INDEX'),var(45))),
	cf(1,eq(attr(var(40),'RELS'),var(46))),
	cf(1,eq(attr(var(40),'RELS_EL'),var(47))),
	cf(1,eq(attr(var(40),'TOP'),var(48))),
	cf(1,eq(attr(var(40),'_MSG'),var(49))),
	cf(1,eq(attr(var(45),'PERF'),'-')),
	cf(1,eq(attr(var(45),'SF'),'prop')),
	cf(1,eq(attr(var(45),'TENSE'),'pres')),
	cf(1,eq(attr(var(45),'type'),'event')),
	cf(1,in_set(var(47),var(46))),
	cf(1,eq(attr(var(47),'ARG0'),var(45))),
	cf(1,eq(attr(var(47),'ARG1'),var(44))),
	cf(1,eq(attr(var(47),'LBL'),var(48))),
	cf(1,eq(attr(var(47),'_CAT'),'v')),
	cf(1,eq(attr(var(47),'relation'),semform('bjeffe',11,[],[]))),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(49),'ARG0'),var(45))),
	cf(1,eq(attr(var(49),'LBL'),var(48))),
	cf(1,eq(attr(var(31),'_SEL'),var(50))),
	cf(1,eq(attr(var(31),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(50),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(50),'_ANIM'),'-')),
	cf(1,eq(attr(var(50),'_HUMAN'),'-')),
	cf(1,eq(attr(var(33),'NSYN'),'pronoun')),
	cf(1,eq(attr(var(35),'H-CONS'),var(51))),
	cf(1,eq(attr(var(35),'RELS'),var(52))),
	cf(1,eq(attr(var(35),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(35),'TOP'),var(53))),
	cf(1,in_set(var(54),var(51))),
	cf(1,eq(attr(var(54),'OUTSCPD'),var(48))),
	cf(1,eq(attr(var(54),'SC_ARG'),var(55))),
	cf(1,eq(attr(var(54),'relation'),'qeq')),
	cf(1,eq(attr(var(55),'type'),'handle')),
	cf(1,in_set(var(3),var(52))),
	cf(1,in_set(var(4),var(52))),
	cf(1,eq(attr(var(3),'ARG0'),var(44))),
	cf(1,eq(attr(var(3),'BODY'),var(56))),
	cf(1,eq(attr(var(3),'LBL'),var(53))),
	cf(1,eq(attr(var(3),'RSTR'),var(55))),
	cf(1,eq(attr(var(3),'relation'),semform('arbitrær-hvilken_q',6,[],[]))),
	cf(1,eq(attr(var(56),'type'),'handle')),
	cf(1,eq(attr(var(53),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(44))),
	cf(1,eq(attr(var(4),'LBL'),var(48))),
	cf(1,eq(attr(var(4),'relation'),semform('ting',7,[],[]))),
	cf(1,eq(attr(var(9),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'pres')),
	cf(1,eq(attr(var(11),'H-CONS'),var(18))),
	cf(1,eq(attr(var(11),'INDEX'),var(57))),
	cf(1,eq(attr(var(11),'RELS'),var(58))),
	cf(1,eq(attr(var(11),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(11),'TOP'),var(26))),
	cf(1,eq(attr(var(11),'_MSG'),var(59))),
	cf(1,eq(attr(var(11),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(57),'PERF'),'-')),
	cf(1,eq(attr(var(57),'SF'),'prop')),
	cf(1,eq(attr(var(57),'TENSE'),'pres')),
	cf(1,eq(attr(var(57),'type'),'event')),
	cf(1,in_set(var(5),var(58))),
	cf(1,in_set(var(6),var(58))),
	cf(1,eq(attr(var(5),'ARG0'),var(57))),
	cf(1,eq(attr(var(5),'ARG1'),var(27))),
	cf(1,eq(attr(var(5),'ARG2'),var(44))),
	cf(1,eq(attr(var(5),'LBL'),var(25))),
	cf(1,eq(attr(var(5),'_CAT'),'v')),
	cf(1,eq(attr(var(5),'relation'),semform('jage',4,[],[]))),
	cf(1,eq(attr(var(59),'ARG0'),var(57))),
	cf(1,eq(attr(var(59),'LBL'),var(26))),
	cf(1,eq(proj(var(60),'m::'),var(61))),
	cf(1,eq(proj(var(62),'m::'),var(61))),
	cf(1,eq(proj(var(63),'m::'),var(64))),
	cf(1,eq(proj(var(65),'m::'),var(64))),
	cf(1,eq(proj(var(66),'o::'),var(67))),
	cf(1,in_set('Mark1',var(67))),
	cf(1,eq(proj(var(68),'m::'),var(69))),
	cf(1,eq(proj(var(70),'m::'),var(69)))
	],
	% C-Structure:
	[
	cf(1,subtree(1658,'ROOT',1694,174)),
	cf(1,phi(1658,var(0))),
	cf(1,subtree(1694,'ROOT',-,2383)),
	cf(1,phi(1694,var(0))),
	cf(1,subtree(2383,'IP',3825,4059)),
	cf(1,phi(2383,var(0))),
	cf(1,subtree(3825,'IP',-,4322)),
	cf(1,phi(3825,var(0))),
	cf(1,subtree(4322,'NP',-,2)),
	cf(1,phi(4322,var(7))),
	cf(1,subtree(2,'N',370,4)),
	cf(1,phi(2,var(7))),
	cf(1,cproj(2,var(60))),
	cf(1,subtree(370,'N',366,6)),
	cf(1,phi(370,var(7))),
	cf(1,cproj(370,var(60))),
	cf(1,subtree(366,'N',362,8)),
	cf(1,phi(366,var(7))),
	cf(1,cproj(366,var(60))),
	cf(1,subtree(362,'N',359,10)),
	cf(1,phi(362,var(7))),
	cf(1,cproj(362,var(60))),
	cf(1,subtree(359,'N',-,12)),
	cf(1,phi(359,var(7))),
	cf(1,cproj(359,var(60))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(7))),
	cf(1,cproj(12,var(62))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(7))),
	cf(1,cproj(13,var(62))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(7))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(7))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(7))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(7))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(7))),
	cf(1,terminal(7,'+Indef',[3])),
	cf(1,phi(7,var(7))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(7))),
	cf(1,terminal(5,'+Pl',[3])),
	cf(1,phi(5,var(7))),
	cf(1,subtree(4059,'I\'',3908,3917)),
	cf(1,phi(4059,var(0))),
	cf(1,subtree(3908,'I\'',-,438)),
	cf(1,phi(3908,var(0))),
	cf(1,subtree(438,'Vfin',437,38)),
	cf(1,phi(438,var(0))),
	cf(1,cproj(438,var(63))),
	cf(1,subtree(437,'Vfin',436,40)),
	cf(1,phi(437,var(0))),
	cf(1,cproj(437,var(63))),
	cf(1,subtree(436,'Vfin',-,42)),
	cf(1,phi(436,var(0))),
	cf(1,cproj(436,var(63))),
	cf(1,subtree(42,'V_BASE',-,43)),
	cf(1,phi(42,var(0))),
	cf(1,cproj(42,var(65))),
	cf(1,terminal(43,'jage',[20])),
	cf(1,phi(43,var(0))),
	cf(1,cproj(43,var(65))),
	cf(1,subtree(40,'V_SUFF_BASE',-,41)),
	cf(1,phi(40,var(0))),
	cf(1,terminal(41,'+Verb',[20])),
	cf(1,phi(41,var(0))),
	cf(1,subtree(38,'V_SUFF_BASE',-,39)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(39,'+Pres',[20])),
	cf(1,phi(39,var(0))),
	cf(1,subtree(3917,'S',-,3939)),
	cf(1,phi(3917,var(0))),
	cf(1,subtree(3939,'VPmain',-,2705)),
	cf(1,phi(3939,var(0))),
	cf(1,subtree(2705,'PRONP',2696,1322)),
	cf(1,phi(2705,var(8))),
	cf(1,subtree(2696,'PRONP',-,2623)),
	cf(1,phi(2696,var(8))),
	cf(1,subtree(2623,'PRON',2622,57)),
	cf(1,phi(2623,var(8))),
	cf(1,subtree(2622,'PRON',-,59)),
	cf(1,phi(2622,var(8))),
	cf(1,subtree(59,'PRON',-,60)),
	cf(1,phi(59,var(8))),
	cf(1,terminal(60,'hva som helst',[51])),
	cf(1,phi(60,var(71))),
	cf(1,cproj(60,var(66))),
	cf(1,subtree(57,'MWE_SUFF_BASE',-,58)),
	cf(1,phi(57,var(8))),
	cf(1,terminal(58,'+Prefer',[51])),
	cf(1,phi(58,var(8))),
	cf(1,subtree(1322,'CPrel',1162,1320)),
	cf(1,phi(1322,var(36))),
	cf(1,subtree(1162,'CPrel',-,150)),
	cf(1,phi(1162,var(36))),
	cf(1,subtree(150,'Crel',-,131)),
	cf(1,phi(150,var(36))),
	cf(1,terminal(131,'som',[131])),
	cf(1,phi(131,var(36))),
	cf(1,subtree(1320,'Ssub',-,1207)),
	cf(1,phi(1320,var(36))),
	cf(1,subtree(1207,'VPfin',-,1204)),
	cf(1,phi(1207,var(36))),
	cf(1,subtree(1204,'Vfin',1203,154)),
	cf(1,phi(1204,var(36))),
	cf(1,cproj(1204,var(68))),
	cf(1,subtree(1203,'Vfin',1202,156)),
	cf(1,phi(1203,var(36))),
	cf(1,cproj(1203,var(68))),
	cf(1,subtree(1202,'Vfin',-,158)),
	cf(1,phi(1202,var(36))),
	cf(1,cproj(1202,var(68))),
	cf(1,subtree(158,'V_BASE',-,159)),
	cf(1,phi(158,var(36))),
	cf(1,cproj(158,var(70))),
	cf(1,terminal(159,'bjeffe',[153])),
	cf(1,phi(159,var(36))),
	cf(1,cproj(159,var(70))),
	cf(1,subtree(156,'V_SUFF_BASE',-,157)),
	cf(1,phi(156,var(36))),
	cf(1,terminal(157,'+Verb',[153])),
	cf(1,phi(157,var(36))),
	cf(1,subtree(154,'V_SUFF_BASE',-,155)),
	cf(1,phi(154,var(36))),
	cf(1,terminal(155,'+Pres',[153])),
	cf(1,phi(155,var(36))),
	cf(1,subtree(174,'PERIOD',-,167)),
	cf(1,phi(174,var(0))),
	cf(1,terminal(167,'.',[167])),
	cf(1,phi(167,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(2,3825,1,7)),
	cf(1,semform_data(3,42,8,11)),
	cf(1,semform_data(4,42,8,11)),
	cf(1,semform_data(5,59,14,27)),
	cf(1,semform_data(6,59,14,27)),
	cf(1,semform_data(7,59,14,27)),
	cf(1,semform_data(8,2705,14,40)),
	cf(1,semform_data(10,158,32,37)),
	cf(1,semform_data(11,158,32,37)),
	cf(1,fspan(var(0),1,40)),
	cf(1,fspan(var(7),1,7)),
	cf(1,fspan(var(8),14,40)),
	cf(1,fspan(var(36),28,40)),
	cf(1,surfaceform(3,'hunder',1,7)),
	cf(1,surfaceform(20,'jager',8,13)),
	cf(1,surfaceform(51,'hva`som`helst',14,27)),
	cf(1,surfaceform(131,'som',28,31)),
	cf(1,surfaceform(153,'bjeffer',32,40)),
	cf(1,surfaceform(167,'.',39,40))
	]).

