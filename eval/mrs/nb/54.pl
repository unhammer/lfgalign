% -*- coding: iso-8859-1 -*-

fstructure('At katten jager Browne, er gammelt.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.09 CPU seconds, 165 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('25'),
	'max_medial2_constituent_weight'('20')
	],
	% Choices:
	[
	choice([A1,A2], 1)
	],
	% Equivalences:
	[
	select(A2, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('være',13,[var(7),var(8)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(7))),
	cf(1,eq(attr(var(0),'PREDLINK'),var(8))),
	cf(1,eq(attr(var(0),'TOPIC'),var(7))),
	cf(1,eq(attr(var(0),'CHECK'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(proj(var(0),'m::'),var(11))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(7),'PRED'),semform('jage',6,[var(12),var(13)],['NULL']))),
	cf(1,eq(attr(var(7),'SUBJ'),var(12))),
	cf(1,eq(attr(var(7),'OBJ'),var(13))),
	cf(1,eq(attr(var(7),'CHECK'),var(14))),
	cf(1,eq(attr(var(7),'GEND'),var(15))),
	cf(1,eq(attr(var(7),'TNS-ASP'),var(16))),
	cf(1,eq(proj(var(7),'m::'),var(17))),
	cf(1,eq(attr(var(7),'CLAUSE-TYPE'),'nominal')),
	cf(1,eq(attr(var(7),'COMP-FORM'),'at')),
	cf(1,eq(attr(var(7),'NUM'),'sg')),
	cf(1,eq(attr(var(7),'TOPCP'),'+')),
	cf(1,eq(attr(var(7),'VFORM'),'fin')),
	cf(1,eq(attr(var(7),'VTYPE'),'main')),
	cf(1,eq(attr(var(12),'PRED'),var(18))),
	cf(1,eq(attr(var(12),'CHECK'),var(19))),
	cf(1,eq(attr(var(12),'GEND'),var(20))),
	cf(1,eq(attr(var(12),'NTYPE'),var(21))),
	cf(1,eq(proj(var(12),'m::'),var(22))),
	cf(1,eq(attr(var(12),'CASE'),'nom')),
	cf(1,eq(attr(var(12),'DEF'),'+')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(A1,eq(var(18),semform('katte',0,[],[]))),
	cf(A2,eq(var(18),semform('katt',2,[],[]))),
	cf(1,eq(attr(var(19),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(19),'_NOUN'),'+')),
	cf(1,eq(attr(var(20),'FEM'),'-')),
	cf(1,eq(attr(var(20),'MASC'),'+')),
	cf(1,eq(attr(var(20),'NEUT'),'-')),
	cf(1,eq(attr(var(21),'NSEM'),var(23))),
	cf(1,eq(attr(var(21),'NSYN'),'common')),
	cf(1,eq(attr(var(23),'COMMON'),'count')),
	cf(1,eq(attr(var(22),'H-CONS'),var(24))),
	cf(1,eq(attr(var(22),'RELS'),var(25))),
	cf(1,eq(attr(var(22),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(22),'TOP'),var(26))),
	cf(1,eq(attr(var(22),'_QUANT'),var(27))),
	cf(1,in_set(var(1),var(24))),
	cf(1,in_set(var(2),var(24))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(28))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(29))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(30))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(31))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,in_set(var(3),var(25))),
	cf(1,in_set(var(4),var(25))),
	cf(1,eq(attr(var(3),'ARG0'),var(32))),
	cf(1,eq(attr(var(3),'BODY'),var(33))),
	cf(1,eq(attr(var(3),'LBL'),var(26))),
	cf(1,eq(attr(var(3),'RSTR'),var(29))),
	cf(1,eq(attr(var(3),'relation'),semform('def_q',5,[],[]))),
	cf(1,eq(attr(var(32),'DIV'),'-')),
	cf(1,eq(attr(var(32),'GRIND'),'-')),
	cf(1,eq(attr(var(32),'NATGEND'),'gender')),
	cf(1,eq(attr(var(32),'NUM'),'sg')),
	cf(1,eq(attr(var(32),'PERS'),'3')),
	cf(1,eq(attr(var(32),'type'),'ref-ind')),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(32))),
	cf(1,eq(attr(var(4),'LBL'),var(28))),
	cf(1,eq(attr(var(4),'relation'),var(34))),
	cf(1,eq(attr(var(4),'_CAT'),'n')),
	cf(A1,eq(var(34),semform('katte',1,[],[]))),
	cf(A2,eq(var(34),semform('katt',3,[],[]))),
	cf(1,eq(attr(var(27),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(27),'TOP'),var(35))),
	cf(1,eq(attr(var(35),'type'),'handle')),
	cf(1,eq(attr(var(13),'PRED'),semform('Browne',10,[],[]))),
	cf(1,eq(attr(var(13),'CHECK'),var(36))),
	cf(1,eq(attr(var(13),'NTYPE'),var(37))),
	cf(1,eq(proj(var(13),'m::'),var(38))),
	cf(1,eq(attr(var(13),'CASE'),'obl')),
	cf(1,eq(attr(var(13),'DEF'),'+')),
	cf(1,eq(attr(var(13),'NUM'),'sg')),
	cf(1,eq(attr(var(13),'PERS'),'3')),
	cf(1,eq(attr(var(13),'REF'),'+')),
	cf(1,eq(attr(var(36),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(36),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(37),'NSEM'),var(39))),
	cf(1,eq(attr(var(37),'NSYN'),'proper')),
	cf(1,eq(attr(var(39),'PROPER'),var(40))),
	cf(1,eq(attr(var(40),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(38),'H-CONS'),var(41))),
	cf(1,eq(attr(var(38),'RELS'),var(42))),
	cf(1,eq(attr(var(38),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(38),'TOP'),var(43))),
	cf(1,eq(attr(var(38),'_ANTECEDVAR'),var(32))),
	cf(1,eq(attr(var(38),'_QUANT'),var(44))),
	cf(1,in_set(var(45),var(41))),
	cf(1,eq(attr(var(45),'OUTSCPD'),var(46))),
	cf(1,eq(attr(var(45),'SC_ARG'),var(47))),
	cf(1,eq(attr(var(45),'relation'),'qeq')),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,eq(attr(var(47),'type'),'handle')),
	cf(1,in_set(var(5),var(42))),
	cf(1,in_set(var(6),var(42))),
	cf(1,eq(attr(var(5),'ARG0'),var(48))),
	cf(1,eq(attr(var(5),'LBL'),var(46))),
	cf(1,eq(attr(var(5),'CARG'),'Browne')),
	cf(1,eq(attr(var(5),'relation'),semform('named',11,[],[]))),
	cf(1,eq(attr(var(48),'NUM'),'sg')),
	cf(1,eq(attr(var(48),'PERS'),'3')),
	cf(1,eq(attr(var(48),'type'),'ref-ind')),
	cf(1,eq(attr(var(6),'ARG0'),var(48))),
	cf(1,eq(attr(var(6),'BODY'),var(49))),
	cf(1,eq(attr(var(6),'LBL'),var(43))),
	cf(1,eq(attr(var(6),'RSTR'),var(47))),
	cf(1,eq(attr(var(6),'relation'),semform('proper_q',12,[],[]))),
	cf(1,eq(attr(var(49),'type'),'handle')),
	cf(1,eq(attr(var(43),'type'),'handle')),
	cf(1,eq(attr(var(44),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(44),'TOP'),var(50))),
	cf(1,eq(attr(var(50),'type'),'handle')),
	cf(1,eq(attr(var(14),'_MAIN-CL'),'-')),
	cf(1,eq(attr(var(14),'_OBJINSITU'),'+')),
	cf(1,eq(attr(var(15),'FEM'),'-')),
	cf(1,eq(attr(var(15),'MASC'),'-')),
	cf(1,eq(attr(var(15),'NEUT'),'+')),
	cf(1,eq(attr(var(16),'MOOD'),'indicative')),
	cf(1,eq(attr(var(16),'TENSE'),'pres')),
	cf(1,eq(attr(var(17),'H-CONS'),var(24))),
	cf(1,eq(attr(var(17),'INDEX'),var(51))),
	cf(1,eq(attr(var(17),'RELS'),var(52))),
	cf(1,eq(attr(var(17),'RELS_EL'),var(53))),
	cf(1,eq(attr(var(17),'TOP'),var(31))),
	cf(1,eq(attr(var(17),'_MSG'),var(54))),
	cf(1,eq(attr(var(17),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(51),'PERF'),'-')),
	cf(1,eq(attr(var(51),'SF'),'prop')),
	cf(1,eq(attr(var(51),'TENSE'),'pres')),
	cf(1,eq(attr(var(51),'type'),'event')),
	cf(1,in_set(var(53),var(52))),
	cf(1,eq(attr(var(53),'ARG0'),var(51))),
	cf(1,eq(attr(var(53),'ARG1'),var(32))),
	cf(1,eq(attr(var(53),'ARG2'),var(48))),
	cf(1,eq(attr(var(53),'LBL'),var(30))),
	cf(1,eq(attr(var(53),'_CAT'),'v')),
	cf(1,eq(attr(var(53),'relation'),semform('jage',7,[],[]))),
	cf(1,eq(attr(var(54),'ARG0'),var(51))),
	cf(1,eq(attr(var(54),'LBL'),var(31))),
	cf(1,eq(attr(var(8),'PRED'),semform('gammel',16,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(55))),
	cf(1,eq(attr(var(8),'GEND'),var(15))),
	cf(1,eq(proj(var(8),'m::'),var(56))),
	cf(1,eq(attr(var(8),'ATYPE'),'predicative')),
	cf(1,eq(attr(var(8),'DEF'),'-')),
	cf(1,eq(attr(var(8),'MEASDIM'),'age')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(55),'_ADVERBIAL'),'-')),
	cf(1,eq(attr(var(55),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(55),'_DEG-MORPH'),'positive')),
	cf(1,eq(attr(var(56),'RELS'),var(57))),
	cf(1,eq(attr(var(56),'RELS_EL'),var(58))),
	cf(1,eq(attr(var(56),'_ANTECEDVAR'),var(51))),
	cf(1,eq(attr(var(56),'_MSG'),var(59))),
	cf(1,in_set(var(58),var(57))),
	cf(1,eq(attr(var(58),'ARG0'),var(60))),
	cf(1,eq(attr(var(58),'ARG1'),var(31))),
	cf(1,eq(attr(var(58),'LBL'),var(61))),
	cf(1,eq(attr(var(58),'_CAT'),'a')),
	cf(1,eq(attr(var(58),'relation'),semform('gammel',17,[],[]))),
	cf(1,eq(attr(var(60),'PERF'),'-')),
	cf(1,eq(attr(var(60),'SF'),'prop')),
	cf(1,eq(attr(var(60),'TENSE'),'pres')),
	cf(1,eq(attr(var(60),'type'),'event')),
	cf(1,eq(attr(var(61),'type'),'handle')),
	cf(1,eq(attr(var(59),'ARG0'),var(60))),
	cf(1,eq(attr(var(59),'LBL'),var(62))),
	cf(1,eq(attr(var(62),'type'),'handle')),
	cf(1,eq(attr(var(9),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'pres')),
	cf(1,eq(attr(var(11),'H-CONS'),var(63))),
	cf(1,eq(attr(var(11),'INDEX'),var(60))),
	cf(1,eq(attr(var(11),'RELS'),var(64))),
	cf(1,eq(attr(var(11),'RELS_EL'),var(58))),
	cf(1,eq(attr(var(11),'TOP'),var(62))),
	cf(1,eq(attr(var(11),'_MSG'),var(59))),
	cf(1,eq(attr(var(11),'_MSGQEQ'),var(65))),
	cf(1,in_set(var(65),var(63))),
	cf(1,eq(attr(var(65),'OUTSCPD'),var(61))),
	cf(1,eq(attr(var(65),'SC_ARG'),var(62))),
	cf(1,eq(attr(var(65),'relation'),'qeq')),
	cf(1,in_set(var(66),var(64))),
	cf(1,eq(attr(var(66),'ARG1'),var(60))),
	cf(1,eq(attr(var(66),'LBL'),var(61))),
	cf(1,eq(attr(var(66),'relation'),semform('stative_asp',15,[],[]))),
	cf(A1,eq(var(67),var(68))),
	cf(A2,eq(var(67),var(69))),
	cf(A2,eq(proj(var(70),'m::'),var(69))),
	cf(1,eq(proj(var(71),'m::'),var(67))),
	cf(A1,eq(proj(var(72),'m::'),var(68))),
	cf(1,eq(proj(var(73),'m::'),var(74))),
	cf(1,eq(proj(var(75),'m::'),var(74)))
	],
	% C-Structure:
	[
	cf(1,subtree(2659,'ROOT',2675,138)),
	cf(1,phi(2659,var(0))),
	cf(1,subtree(2675,'ROOT',-,2719)),
	cf(1,phi(2675,var(0))),
	cf(1,subtree(2719,'IP',2775,2725)),
	cf(1,phi(2719,var(0))),
	cf(1,subtree(2775,'IP',2771,95)),
	cf(1,phi(2775,var(0))),
	cf(1,subtree(2771,'IP',-,2770)),
	cf(1,phi(2771,var(0))),
	cf(1,subtree(2770,'CPnom',311,2628)),
	cf(1,phi(2770,var(7))),
	cf(1,subtree(311,'CPnom',-,16)),
	cf(1,phi(311,var(7))),
	cf(1,subtree(16,'Cnom',-,3)),
	cf(1,phi(16,var(7))),
	cf(1,terminal(3,'at',[3])),
	cf(1,phi(3,var(7))),
	cf(1,subtree(2628,'Ssub',2626,2012)),
	cf(1,phi(2628,var(7))),
	cf(1,subtree(2626,'Ssub',-,2873)),
	cf(1,phi(2626,var(7))),
	cf(1,subtree(2873,'NP',-,18)),
	cf(1,phi(2873,var(12))),
	cf(1,subtree(18,'N',1925,19)),
	cf(1,phi(18,var(12))),
	cf(1,cproj(18,var(71))),
	cf(1,subtree(1925,'N',1924,21)),
	cf(1,phi(1925,var(12))),
	cf(1,cproj(1925,var(71))),
	cf(A2,subtree(1924,'N',1923,36)),
	cf(A2,phi(1924,var(12))),
	cf(A2,cproj(1924,var(71))),
	cf(A2,subtree(1923,'N',1896,38)),
	cf(A2,phi(1923,var(12))),
	cf(A2,cproj(1923,var(71))),
	cf(A2,subtree(1896,'N',-,40)),
	cf(A2,phi(1896,var(12))),
	cf(A2,cproj(1896,var(71))),
	cf(A2,subtree(40,'N_BASE',-,41)),
	cf(A2,phi(40,var(12))),
	cf(A2,cproj(40,var(70))),
	cf(A2,terminal(41,'katt',[17])),
	cf(A2,phi(41,var(12))),
	cf(A2,cproj(41,var(70))),
	cf(A2,subtree(38,'N_SUFF_BASE',-,39)),
	cf(A2,phi(38,var(12))),
	cf(A2,terminal(39,'+Noun',[17])),
	cf(A2,phi(39,var(12))),
	cf(A2,subtree(36,'N_SUFF_BASE',-,37)),
	cf(A2,phi(36,var(12))),
	cf(A2,terminal(37,'+Masc',[17])),
	cf(A2,phi(37,var(12))),
	cf(A1,subtree(1924,'N',1926,23)),
	cf(A1,phi(1924,var(12))),
	cf(A1,cproj(1924,var(71))),
	cf(A1,subtree(1926,'N',1895,25)),
	cf(A1,phi(1926,var(12))),
	cf(A1,cproj(1926,var(71))),
	cf(A1,subtree(1895,'N',-,29)),
	cf(A1,phi(1895,var(12))),
	cf(A1,cproj(1895,var(71))),
	cf(A1,subtree(29,'N_BASE',-,28)),
	cf(A1,phi(29,var(12))),
	cf(A1,cproj(29,var(72))),
	cf(A1,terminal(28,'katte',[17])),
	cf(A1,phi(28,var(12))),
	cf(A1,cproj(28,var(72))),
	cf(A1,subtree(25,'N_SUFF_BASE',-,26)),
	cf(A1,phi(25,var(12))),
	cf(A1,terminal(26,'+Noun',[17])),
	cf(A1,phi(26,var(12))),
	cf(A1,subtree(23,'N_SUFF_BASE',-,24)),
	cf(A1,phi(23,var(12))),
	cf(A1,terminal(24,'+Masc',[17])),
	cf(A1,phi(24,var(12))),
	cf(1,subtree(21,'N_SUFF_BASE',-,22)),
	cf(1,phi(21,var(12))),
	cf(1,terminal(22,'+Def',[17])),
	cf(1,phi(22,var(76))),
	cf(1,subtree(19,'N_SUFF_BASE',-,20)),
	cf(1,phi(19,var(12))),
	cf(1,terminal(20,'+Sg',[17])),
	cf(1,phi(20,var(12))),
	cf(1,subtree(2012,'VPfin',2011,1829)),
	cf(1,phi(2012,var(7))),
	cf(1,subtree(2011,'VPfin',-,2007)),
	cf(1,phi(2011,var(7))),
	cf(1,subtree(2007,'Vfin',2006,66)),
	cf(1,phi(2007,var(7))),
	cf(1,cproj(2007,var(73))),
	cf(1,subtree(2006,'Vfin',2005,68)),
	cf(1,phi(2006,var(7))),
	cf(1,cproj(2006,var(73))),
	cf(1,subtree(2005,'Vfin',-,70)),
	cf(1,phi(2005,var(7))),
	cf(1,cproj(2005,var(73))),
	cf(1,subtree(70,'V_BASE',-,71)),
	cf(1,phi(70,var(7))),
	cf(1,cproj(70,var(75))),
	cf(1,terminal(71,'jage',[48])),
	cf(1,phi(71,var(7))),
	cf(1,cproj(71,var(75))),
	cf(1,subtree(68,'V_SUFF_BASE',-,69)),
	cf(1,phi(68,var(7))),
	cf(1,terminal(69,'+Verb',[48])),
	cf(1,phi(69,var(7))),
	cf(1,subtree(66,'V_SUFF_BASE',-,67)),
	cf(1,phi(66,var(7))),
	cf(1,terminal(67,'+Pres',[48])),
	cf(1,phi(67,var(7))),
	cf(1,subtree(1829,'PROPP',-,498)),
	cf(1,phi(1829,var(13))),
	cf(1,subtree(498,'PROP',497,80)),
	cf(1,phi(498,var(13))),
	cf(1,subtree(497,'PROP',494,82)),
	cf(1,phi(497,var(13))),
	cf(1,subtree(494,'PROP',-,84)),
	cf(1,phi(494,var(13))),
	cf(1,subtree(84,'PROP_BASE',-,85)),
	cf(1,phi(84,var(13))),
	cf(1,terminal(85,'Browne',[79])),
	cf(1,phi(85,var(13))),
	cf(1,subtree(82,'N_SUFF_BASE',-,83)),
	cf(1,phi(82,var(13))),
	cf(1,terminal(83,'+Prop',[79])),
	cf(1,phi(83,var(13))),
	cf(1,subtree(80,'N_SUFF_BASE',-,81)),
	cf(1,phi(80,var(13))),
	cf(1,terminal(81,'+Indef',[79])),
	cf(1,phi(81,var(13))),
	cf(1,subtree(95,'COMMA',-,87)),
	cf(1,phi(95,var(0))),
	cf(1,terminal(87,',',[87])),
	cf(1,phi(87,var(0))),
	cf(1,subtree(2725,'I\'cop',2724,2709)),
	cf(1,phi(2725,var(0))),
	cf(1,subtree(2724,'I\'cop',-,101)),
	cf(1,phi(2724,var(0))),
	cf(1,subtree(101,'Vcopfin',-,96)),
	cf(1,phi(101,var(0))),
	cf(1,terminal(96,'er',[96])),
	cf(1,phi(96,var(0))),
	cf(1,subtree(2709,'Scop',-,2641)),
	cf(1,phi(2709,var(0))),
	cf(1,subtree(2641,'VPmain2',-,1059)),
	cf(1,phi(2641,var(0))),
	cf(1,subtree(1059,'AP',-,1057)),
	cf(1,phi(1059,var(8))),
	cf(1,subtree(1057,'A',1056,112)),
	cf(1,phi(1057,var(8))),
	cf(1,subtree(1056,'A',1055,114)),
	cf(1,phi(1056,var(8))),
	cf(1,subtree(1055,'A',1054,116)),
	cf(1,phi(1055,var(8))),
	cf(1,subtree(1054,'A',1053,118)),
	cf(1,phi(1054,var(8))),
	cf(1,subtree(1053,'A',1046,120)),
	cf(1,phi(1053,var(8))),
	cf(1,subtree(1046,'A',-,122)),
	cf(1,phi(1046,var(8))),
	cf(1,subtree(122,'A_BASE',-,123)),
	cf(1,phi(122,var(8))),
	cf(1,terminal(123,'gammel',[111])),
	cf(1,phi(123,var(8))),
	cf(1,subtree(120,'A_SUFF_BASE',-,121)),
	cf(1,phi(120,var(8))),
	cf(1,terminal(121,'+Adj',[111])),
	cf(1,phi(121,var(8))),
	cf(1,subtree(118,'A_SUFF_BASE',-,119)),
	cf(1,phi(118,var(8))),
	cf(1,terminal(119,'+Pos',[111])),
	cf(1,phi(119,var(8))),
	cf(1,subtree(116,'N_SUFF_BASE',-,117)),
	cf(1,phi(116,var(8))),
	cf(1,terminal(117,'+Neut',[111])),
	cf(1,phi(117,var(8))),
	cf(1,subtree(114,'N_SUFF_BASE',-,115)),
	cf(1,phi(114,var(8))),
	cf(1,terminal(115,'+Indef',[111])),
	cf(1,phi(115,var(8))),
	cf(1,subtree(112,'N_SUFF_BASE',-,113)),
	cf(1,phi(112,var(8))),
	cf(1,terminal(113,'+Sg',[111])),
	cf(1,phi(113,var(8))),
	cf(1,subtree(138,'PERIOD',-,131)),
	cf(1,phi(138,var(0))),
	cf(1,terminal(131,'.',[131])),
	cf(1,phi(131,var(0))),
	cf(A1,semform_data(0,29,4,9)),
	cf(A1,semform_data(1,29,4,9)),
	cf(A2,semform_data(2,40,4,7)),
	cf(A2,semform_data(3,40,4,7)),
	cf(1,semform_data(5,21,10,10)),
	cf(1,semform_data(6,70,11,14)),
	cf(1,semform_data(7,70,11,14)),
	cf(1,semform_data(10,84,17,22)),
	cf(1,semform_data(11,84,17,22)),
	cf(1,semform_data(12,84,17,22)),
	cf(1,semform_data(13,101,25,27)),
	cf(1,semform_data(15,101,25,27)),
	cf(1,semform_data(16,122,28,33)),
	cf(1,semform_data(17,122,28,33)),
	cf(1,fspan(var(0),1,36)),
	cf(1,fspan(var(7),1,23)),
	cf(1,fspan(var(12),4,10)),
	cf(1,fspan(var(13),17,23)),
	cf(1,fspan(var(8),28,36)),
	cf(1,surfaceform(3,'at',1,3)),
	cf(1,surfaceform(17,'katten',4,10)),
	cf(1,surfaceform(48,'jager',11,16)),
	cf(1,surfaceform(79,'Browne',17,23)),
	cf(1,surfaceform(87,',',23,24)),
	cf(1,surfaceform(96,'er',25,27)),
	cf(1,surfaceform(111,'gammelt',28,36)),
	cf(1,surfaceform(131,'.',35,36))
	]).
