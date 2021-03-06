% -*- coding: iso-8859-1 -*-

fstructure('Kattene fant en måte å bjeffe på.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('4 solutions, 0.29 CPU seconds, 310 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('25'),
	'max_medial2_constituent_weight'('20')
	],
	% Choices:
	[
	choice([A1,A2,A3,A4], 1)
	],
	% Equivalences:
	[
	select(A4, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('finne',7,[var(11),var(12)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(11))),
	cf(1,eq(attr(var(0),'OBJ'),var(12))),
	cf(1,eq(attr(var(0),'TOPIC'),var(11))),
	cf(1,eq(attr(var(0),'CHECK'),var(13))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(14))),
	cf(1,eq(proj(var(0),'m::'),var(15))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(11),'PRED'),var(16))),
	cf(1,eq(attr(var(11),'CHECK'),var(17))),
	cf(1,eq(attr(var(11),'GEND'),var(18))),
	cf(1,eq(attr(var(11),'NTYPE'),var(19))),
	cf(1,eq(proj(var(11),'m::'),var(20))),
	cf(1,eq(attr(var(11),'CASE'),'nom')),
	cf(1,eq(attr(var(11),'DEF'),'+')),
	cf(1,eq(attr(var(11),'NUM'),'pl')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(or(A3,A1),eq(var(16),semform('katte',0,[],[]))),
	cf(or(A4,A2),eq(var(16),semform('katt',2,[],[]))),
	cf(1,eq(attr(var(17),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(17),'_NOUN'),'+')),
	cf(or(A4,A2),eq(attr(var(18),'FEM'),'-')),
	cf(or(A4,A2),eq(attr(var(18),'MASC'),'+')),
	cf(1,eq(attr(var(18),'NEUT'),'-')),
	cf(1,eq(attr(var(19),'NSEM'),var(21))),
	cf(1,eq(attr(var(19),'NSYN'),'common')),
	cf(1,eq(attr(var(21),'COMMON'),'count')),
	cf(1,eq(attr(var(20),'H-CONS'),var(22))),
	cf(1,eq(attr(var(20),'RELS'),var(23))),
	cf(1,eq(attr(var(20),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(20),'TOP'),var(24))),
	cf(1,eq(attr(var(20),'_QUANT'),var(25))),
	cf(1,in_set(var(1),var(22))),
	cf(or(A3,A1),in_set(var(2),var(22))),
	cf(or(A4,A2),in_set(var(3),var(22))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(26))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(27))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(or(A3,A1),eq(attr(var(2),'OUTSCPD'),var(28))),
	cf(or(A3,A1),eq(attr(var(2),'SC_ARG'),var(29))),
	cf(or(A3,A1),eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(or(A4,A2),eq(attr(var(3),'OUTSCPD'),var(28))),
	cf(or(A4,A2),eq(attr(var(3),'SC_ARG'),var(29))),
	cf(or(A4,A2),eq(attr(var(3),'relation'),'qeq')),
	cf(1,in_set(var(4),var(23))),
	cf(1,in_set(var(5),var(23))),
	cf(1,eq(attr(var(4),'ARG0'),var(30))),
	cf(1,eq(attr(var(4),'BODY'),var(31))),
	cf(1,eq(attr(var(4),'LBL'),var(24))),
	cf(1,eq(attr(var(4),'RSTR'),var(29))),
	cf(1,eq(attr(var(4),'relation'),var(32))),
	cf(1,eq(attr(var(30),'DIV'),'+')),
	cf(1,eq(attr(var(30),'GRIND'),'-')),
	cf(1,eq(attr(var(30),'NATGEND'),'gender')),
	cf(1,eq(attr(var(30),'NUM'),'pl')),
	cf(1,eq(attr(var(30),'PERS'),'3')),
	cf(1,eq(attr(var(30),'type'),'ref-ind')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(or(A4,A2),eq(var(32),semform('def_q',5,[],[]))),
	cf(or(A3,A1),eq(var(32),semform('def_q',6,[],[]))),
	cf(1,eq(attr(var(5),'ARG0'),var(30))),
	cf(1,eq(attr(var(5),'LBL'),var(28))),
	cf(1,eq(attr(var(5),'relation'),var(33))),
	cf(1,eq(attr(var(5),'_CAT'),'n')),
	cf(or(A3,A1),eq(var(33),semform('katte',1,[],[]))),
	cf(or(A4,A2),eq(var(33),semform('katt',3,[],[]))),
	cf(1,eq(attr(var(25),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(25),'TOP'),var(34))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(12),'PRED'),semform('måte',17,[],[]))),
	cf(1,eq(attr(var(12),'ADJUNCT'),var(35))),
	cf(1,eq(attr(var(12),'CHECK'),var(36))),
	cf(1,eq(attr(var(12),'GEND'),var(37))),
	cf(1,eq(attr(var(12),'NTYPE'),var(38))),
	cf(1,eq(attr(var(12),'SPEC'),var(39))),
	cf(1,eq(proj(var(12),'m::'),var(40))),
	cf(1,eq(attr(var(12),'CASE'),'obl')),
	cf(1,eq(attr(var(12),'DEF'),'-')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(12),'REF'),'+')),
	cf(1,in_set(var(41),var(35))),
	cf(1,eq(attr(var(41),'PRED'),semform('bjeffe',20,[var(42)],[]))),
	cf(1,eq(attr(var(41),'SUBJ'),var(42))),
	cf(1,eq(attr(var(41),'ADJUNCT'),var(43))),
	cf(1,eq(attr(var(41),'GEND'),var(44))),
	cf(1,eq(attr(var(41),'TOPIC-REL'),var(45))),
	cf(1,eq(proj(var(41),'m::'),var(46))),
	cf(1,eq(attr(var(41),'COMP-FORM'),'å')),
	cf(1,eq(attr(var(41),'NUM'),'sg')),
	cf(1,eq(attr(var(41),'VFORM'),'inf')),
	cf(1,eq(attr(var(41),'VTYPE'),'main')),
	cf(1,eq(attr(var(42),'PRED'),semform('pro',19,[],[]))),
	cf(1,eq(attr(var(42),'NUM'),var(47))),
	cf(1,eq(attr(var(42),'PERS'),var(48))),
	cf(1,eq(proj(var(42),'m::'),var(49))),
	cf(1,eq(attr(var(49),'H-CONS'),var(50))),
	cf(1,eq(attr(var(49),'RELS_EL'),var(51))),
	cf(1,eq(attr(var(51),'ARG0'),var(52))),
	cf(1,eq(attr(var(52),'type'),'non_expl-ind')),
	cf(1,in_set(var(6),var(43))),
	cf(1,eq(attr(var(6),'PRED'),semform('på',22,[var(45)],[]))),
	cf(1,eq(attr(var(6),'OBJ'),var(45))),
	cf(1,eq(attr(var(6),'CHECK'),var(53))),
	cf(1,eq(proj(var(6),'m::'),var(54))),
	cf(1,eq(attr(var(6),'PFORM'),'på')),
	cf(1,eq(attr(var(6),'PTYPE'),'sem')),
	cf(1,eq(attr(var(45),'PRED'),semform('måte',17,[],[]))),
	cf(1,eq(attr(var(45),'GEND'),var(37))),
	cf(1,eq(proj(var(45),'m::'),var(55))),
	cf(1,eq(attr(var(45),'CASE'),'obl')),
	cf(1,eq(attr(var(45),'NUM'),'sg')),
	cf(1,eq(attr(var(37),'FEM'),'-')),
	cf(1,eq(attr(var(37),'MASC'),'+')),
	cf(1,eq(attr(var(37),'NEUT'),'-')),
	cf(1,eq(attr(var(55),'RELS'),var(56))),
	cf(1,eq(attr(var(55),'RELS_EL'),var(57))),
	cf(1,in_set(var(58),var(56))),
	cf(1,eq(attr(var(58),'ARG0'),var(59))),
	cf(1,eq(attr(var(58),'ARG1'),var(60))),
	cf(1,eq(attr(var(58),'ARG2'),var(61))),
	cf(1,eq(attr(var(58),'LBL'),var(62))),
	cf(1,eq(attr(var(58),'_CAT'),'p')),
	cf(1,eq(attr(var(58),'relation'),semform('på',23,[],[]))),
	cf(1,eq(attr(var(59),'PERF'),'-')),
	cf(1,eq(attr(var(59),'TENSE'),'notense')),
	cf(1,eq(attr(var(59),'type'),'event')),
	cf(1,eq(attr(var(60),'PERF'),'-')),
	cf(1,eq(attr(var(60),'TENSE'),'notense')),
	cf(1,eq(attr(var(60),'type'),'event')),
	cf(1,eq(attr(var(61),'DIV'),'-')),
	cf(1,eq(attr(var(61),'GRIND'),'-')),
	cf(1,eq(attr(var(61),'NATGEND'),'gender')),
	cf(1,eq(attr(var(61),'NUM'),'sg')),
	cf(1,eq(attr(var(61),'PERS'),'3')),
	cf(1,eq(attr(var(61),'type'),'ref-ind')),
	cf(1,eq(attr(var(62),'type'),'handle')),
	cf(1,eq(attr(var(57),'ARG0'),var(61))),
	cf(1,eq(attr(var(53),'_ANTECED'),var(63))),
	cf(1,eq(attr(var(53),'_MOVED-OBJ'),'+')),
	cf(1,eq(attr(var(63),'NUM'),var(47))),
	cf(1,eq(attr(var(63),'PERS'),var(48))),
	cf(1,eq(proj(var(63),'m::'),var(64))),
	cf(1,eq(attr(var(64),'ARG0'),var(52))),
	cf(1,eq(attr(var(54),'RELS'),var(56))),
	cf(1,eq(attr(var(54),'RELS_EL'),var(58))),
	cf(1,eq(attr(var(54),'_TOPLBL'),var(62))),
	cf(1,eq(attr(var(44),'FEM'),'-')),
	cf(1,eq(attr(var(44),'MASC'),'-')),
	cf(1,eq(attr(var(44),'NEUT'),'+')),
	cf(1,eq(attr(var(46),'H-CONS'),var(50))),
	cf(1,eq(attr(var(46),'INDEX'),var(60))),
	cf(1,eq(attr(var(46),'RELS'),var(65))),
	cf(1,eq(attr(var(46),'RELS_EL'),var(66))),
	cf(1,in_set(var(66),var(65))),
	cf(1,eq(attr(var(66),'ARG0'),var(60))),
	cf(1,eq(attr(var(66),'ARG1'),var(52))),
	cf(1,eq(attr(var(66),'LBL'),var(62))),
	cf(1,eq(attr(var(66),'_CAT'),'v')),
	cf(1,eq(attr(var(66),'relation'),semform('bjeffe',21,[],[]))),
	cf(1,eq(attr(var(36),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(36),'_NOUN'),'+')),
	cf(1,eq(attr(var(36),'_PREDET'),'+')),
	cf(1,eq(attr(var(36),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(38),'NSEM'),var(67))),
	cf(1,eq(attr(var(38),'NSYN'),'common')),
	cf(1,eq(attr(var(67),'COMMON'),'count')),
	cf(or(A3,A4),eq(attr(var(39),'DET'),var(68))),
	cf(or(A1,A2),eq(attr(var(39),'NUMBER'),var(69))),
	cf(or(A3,A4),eq(attr(var(68),'PRED'),semform('en',9,[],[]))),
	cf(or(A3,A4),eq(attr(var(68),'DET-TYPE'),'article')),
	cf(or(A1,A2),eq(attr(var(69),'PRED'),semform('en',9,[],[]))),
	cf(or(A1,A2),eq(attr(var(69),'AGRGEND'),var(37))),
	cf(or(A1,A2),eq(proj(var(69),'m::'),var(70))),
	cf(or(A1,A2),eq(attr(var(69),'AGRNUM'),'sg')),
	cf(or(A1,A2),eq(attr(var(69),'DIGVALUE'),'1')),
	cf(or(A1,A2),eq(attr(var(69),'HEADNUM'),'sg')),
	cf(or(A1,A2),eq(attr(var(70),'RELS'),var(71))),
	cf(or(A1,A2),eq(attr(var(70),'_CARD'),var(72))),
	cf(or(A1,A2),eq(attr(var(70),'_TOPHNDL'),var(62))),
	cf(or(A1,A2),in_set(var(72),var(71))),
	cf(or(A1,A2),eq(attr(var(72),'ARG0'),var(73))),
	cf(or(A1,A2),eq(attr(var(72),'ARG1'),var(61))),
	cf(or(A1,A2),eq(attr(var(72),'LBL'),var(62))),
	cf(or(A1,A2),eq(attr(var(72),'CARG'),'1')),
	cf(or(A1,A2),eq(attr(var(72),'relation'),semform('card',10,[],[]))),
	cf(or(A1,A2),eq(attr(var(73),'type'),'event')),
	cf(1,eq(attr(var(40),'H-CONS'),var(74))),
	cf(1,eq(attr(var(40),'RELS'),var(75))),
	cf(1,eq(attr(var(40),'RELS_EL'),var(9))),
	cf(1,eq(attr(var(40),'TOP'),var(76))),
	cf(1,eq(attr(var(40),'_ANTECEDVAR'),var(30))),
	cf(1,eq(attr(var(40),'_QUANT'),var(77))),
	cf(or(A1,A2),in_set(var(7),var(74))),
	cf(or(A3,A4),in_set(var(8),var(74))),
	cf(or(A1,A2),eq(attr(var(7),'OUTSCPD'),var(62))),
	cf(or(A1,A2),eq(attr(var(7),'SC_ARG'),var(78))),
	cf(or(A1,A2),eq(attr(var(7),'relation'),'qeq')),
	cf(1,eq(attr(var(78),'type'),'handle')),
	cf(or(A3,A4),eq(attr(var(8),'OUTSCPD'),var(62))),
	cf(or(A3,A4),eq(attr(var(8),'SC_ARG'),var(78))),
	cf(or(A3,A4),eq(attr(var(8),'relation'),'qeq')),
	cf(1,in_set(var(9),var(75))),
	cf(1,in_set(var(10),var(75))),
	cf(1,eq(attr(var(9),'ARG0'),var(61))),
	cf(1,eq(attr(var(9),'LBL'),var(62))),
	cf(1,eq(attr(var(9),'_CAT'),'n')),
	cf(1,eq(attr(var(9),'relation'),semform('måte',18,[],[]))),
	cf(1,eq(attr(var(10),'ARG0'),var(61))),
	cf(1,eq(attr(var(10),'BODY'),var(79))),
	cf(1,eq(attr(var(10),'LBL'),var(76))),
	cf(1,eq(attr(var(10),'RSTR'),var(78))),
	cf(1,eq(attr(var(10),'relation'),var(80))),
	cf(1,eq(attr(var(79),'type'),'handle')),
	cf(1,eq(attr(var(76),'type'),'handle')),
	cf(or(A3,A4),eq(var(80),semform('en_q',12,[],[]))),
	cf(or(A1,A2),eq(var(80),semform('udef_q',14,[],[]))),
	cf(1,eq(attr(var(77),'RELS_EL'),var(10))),
	cf(1,eq(attr(var(77),'TOP'),var(81))),
	cf(1,eq(attr(var(81),'type'),'handle')),
	cf(1,eq(attr(var(13),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(14),'MOOD'),'indicative')),
	cf(1,eq(attr(var(14),'TENSE'),'past')),
	cf(1,eq(attr(var(15),'H-CONS'),var(22))),
	cf(1,eq(attr(var(15),'INDEX'),var(82))),
	cf(1,eq(attr(var(15),'RELS'),var(83))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(84))),
	cf(1,eq(attr(var(15),'TOP'),var(27))),
	cf(1,eq(attr(var(15),'_MSG'),var(85))),
	cf(1,eq(attr(var(15),'_MSGQEQ'),var(1))),
	cf(1,eq(attr(var(82),'PERF'),'-')),
	cf(1,eq(attr(var(82),'SF'),'prop')),
	cf(1,eq(attr(var(82),'TENSE'),'past')),
	cf(1,eq(attr(var(82),'type'),'event')),
	cf(1,in_set(var(84),var(83))),
	cf(1,eq(attr(var(84),'ARG0'),var(82))),
	cf(1,eq(attr(var(84),'ARG1'),var(30))),
	cf(1,eq(attr(var(84),'ARG2'),var(61))),
	cf(1,eq(attr(var(84),'LBL'),var(26))),
	cf(1,eq(attr(var(84),'_CAT'),'v')),
	cf(1,eq(attr(var(84),'relation'),semform('finne',8,[],[]))),
	cf(1,eq(attr(var(85),'ARG0'),var(82))),
	cf(1,eq(attr(var(85),'LBL'),var(27))),
	cf(1,eq(attr(var(86),'LEFT_SISTER'),var(87))),
	cf(1,eq(attr(var(87),'RIGHT_DAUGHTER'),var(88))),
	cf(1,eq(attr(var(87),'RIGHT_SISTER'),var(86))),
	cf(1,eq(attr(var(88),'LEFT_SISTER'),var(89))),
	cf(1,eq(attr(var(88),'RIGHT_DAUGHTER'),var(90))),
	cf(1,eq(attr(var(89),'RIGHT_SISTER'),var(88))),
	cf(1,eq(attr(var(90),'LEFT_SISTER'),var(91))),
	cf(1,eq(attr(var(90),'RIGHT_DAUGHTER'),var(92))),
	cf(1,eq(attr(var(91),'RIGHT_SISTER'),var(90))),
	cf(1,eq(proj(var(91),'m::'),var(93))),
	cf(1,eq(attr(var(92),'RIGHT_DAUGHTER'),var(94))),
	cf(1,eq(attr(var(94),'RIGHT_DAUGHTER'),var(95))),
	cf(1,eq(attr(var(95),'RIGHT_DAUGHTER'),var(96))),
	cf(1,eq(attr(var(96),'LEFT_SISTER'),var(97))),
	cf(1,eq(attr(var(96),'RIGHT_DAUGHTER'),var(98))),
	cf(1,eq(attr(var(97),'RIGHT_SISTER'),var(96))),
	cf(1,eq(proj(var(97),'m::'),var(99))),
	cf(1,eq(attr(var(98),'LEFT_SISTER'),var(100))),
	cf(1,eq(attr(var(98),'RIGHT_DAUGHTER'),var(101))),
	cf(1,eq(attr(var(100),'RIGHT_SISTER'),var(98))),
	cf(1,eq(attr(var(101),'LEFT_SISTER'),var(102))),
	cf(1,eq(proj(var(101),'o::'),var(103))),
	cf(1,eq(attr(var(102),'RIGHT_SISTER'),var(101))),
	cf(1,eq(proj(var(102),'m::'),var(104))),
	cf(1,in_set('Mark5',var(103))),
	cf(or(A3,A1),eq(var(105),var(106))),
	cf(or(A4,A2),eq(var(105),var(107))),
	cf(or(A4,A2),eq(proj(var(108),'m::'),var(107))),
	cf(1,eq(proj(var(109),'m::'),var(105))),
	cf(or(A3,A1),eq(proj(var(110),'m::'),var(106))),
	cf(1,eq(proj(var(111),'m::'),var(93))),
	cf(1,eq(proj(var(112),'m::'),var(99))),
	cf(1,eq(proj(var(113),'m::'),var(104)))
	],
	% C-Structure:
	[
	cf(1,subtree(1326,'ROOT',1451,210)),
	cf(1,phi(1326,var(0))),
	cf(1,subtree(1451,'ROOT',-,2006)),
	cf(1,phi(1451,var(0))),
	cf(1,subtree(2006,'IP',3929,4063)),
	cf(1,phi(2006,var(0))),
	cf(1,cproj(2006,var(87))),
	cf(1,subtree(3929,'IP',-,4131)),
	cf(1,phi(3929,var(0))),
	cf(1,subtree(4131,'NP',-,2)),
	cf(1,phi(4131,var(11))),
	cf(1,subtree(2,'N',406,4)),
	cf(1,phi(2,var(11))),
	cf(1,cproj(2,var(109))),
	cf(or(A4,A2),subtree(406,'N',402,21)),
	cf(or(A4,A2),phi(406,var(11))),
	cf(or(A4,A2),cproj(406,var(109))),
	cf(or(A4,A2),subtree(402,'N',398,23)),
	cf(or(A4,A2),phi(402,var(11))),
	cf(or(A4,A2),cproj(402,var(109))),
	cf(or(A4,A2),subtree(398,'N',395,25)),
	cf(or(A4,A2),phi(398,var(11))),
	cf(or(A4,A2),cproj(398,var(109))),
	cf(or(A4,A2),subtree(395,'N',-,27)),
	cf(or(A4,A2),phi(395,var(11))),
	cf(or(A4,A2),cproj(395,var(109))),
	cf(or(A4,A2),subtree(27,'N_BASE',-,28)),
	cf(or(A4,A2),phi(27,var(11))),
	cf(or(A4,A2),cproj(27,var(108))),
	cf(or(A4,A2),terminal(28,'katt',[3])),
	cf(or(A4,A2),phi(28,var(11))),
	cf(or(A4,A2),cproj(28,var(108))),
	cf(or(A4,A2),subtree(25,'N_SUFF_BASE',-,26)),
	cf(or(A4,A2),phi(25,var(11))),
	cf(or(A4,A2),terminal(26,'+Noun',[3])),
	cf(or(A4,A2),phi(26,var(11))),
	cf(or(A4,A2),subtree(23,'N_SUFF_BASE',-,24)),
	cf(or(A4,A2),phi(23,var(11))),
	cf(or(A4,A2),terminal(24,'+Masc',[3])),
	cf(or(A4,A2),phi(24,var(11))),
	cf(or(A4,A2),subtree(21,'N_SUFF_BASE',-,22)),
	cf(or(A4,A2),phi(21,var(11))),
	cf(or(A4,A2),terminal(22,'+Def',[3])),
	cf(or(A4,A2),phi(22,var(114))),
	cf(or(A3,A1),subtree(406,'N',432,6)),
	cf(or(A3,A1),phi(406,var(11))),
	cf(or(A3,A1),cproj(406,var(109))),
	cf(or(A3,A1),subtree(432,'N',428,8)),
	cf(or(A3,A1),phi(432,var(11))),
	cf(or(A3,A1),cproj(432,var(109))),
	cf(or(A3,A1),subtree(428,'N',421,10)),
	cf(or(A3,A1),phi(428,var(11))),
	cf(or(A3,A1),cproj(428,var(109))),
	cf(or(A3,A1),subtree(421,'N',-,14)),
	cf(or(A3,A1),phi(421,var(11))),
	cf(or(A3,A1),cproj(421,var(109))),
	cf(or(A3,A1),subtree(14,'N_BASE',-,13)),
	cf(or(A3,A1),phi(14,var(11))),
	cf(or(A3,A1),cproj(14,var(110))),
	cf(or(A3,A1),terminal(13,'katte',[3])),
	cf(or(A3,A1),phi(13,var(11))),
	cf(or(A3,A1),cproj(13,var(110))),
	cf(or(A3,A1),subtree(10,'N_SUFF_BASE',-,11)),
	cf(or(A3,A1),phi(10,var(11))),
	cf(or(A3,A1),terminal(11,'+Noun',[3])),
	cf(or(A3,A1),phi(11,var(11))),
	cf(or(A3,A1),subtree(8,'N_SUFF_BASE',-,9)),
	cf(or(A3,A1),phi(8,var(11))),
	cf(or(A3,A1),terminal(9,'+MF',[3])),
	cf(or(A3,A1),phi(9,var(11))),
	cf(or(A3,A1),subtree(6,'N_SUFF_BASE',-,7)),
	cf(or(A3,A1),phi(6,var(11))),
	cf(or(A3,A1),terminal(7,'+Def',[3])),
	cf(or(A3,A1),phi(7,var(115))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(11))),
	cf(1,terminal(5,'+Pl',[3])),
	cf(1,phi(5,var(11))),
	cf(1,subtree(4063,'I\'',3980,3991)),
	cf(1,phi(4063,var(0))),
	cf(1,cproj(4063,var(88))),
	cf(1,subtree(3980,'I\'',-,500)),
	cf(1,phi(3980,var(0))),
	cf(1,subtree(500,'Vfin',499,53)),
	cf(1,phi(500,var(0))),
	cf(1,cproj(500,var(91))),
	cf(1,subtree(499,'Vfin',498,55)),
	cf(1,phi(499,var(0))),
	cf(1,cproj(499,var(91))),
	cf(1,subtree(498,'Vfin',-,57)),
	cf(1,phi(498,var(0))),
	cf(1,cproj(498,var(91))),
	cf(1,subtree(57,'V_BASE',-,58)),
	cf(1,phi(57,var(0))),
	cf(1,cproj(57,var(111))),
	cf(1,terminal(58,'finne',[35])),
	cf(1,phi(58,var(0))),
	cf(1,cproj(58,var(111))),
	cf(1,subtree(55,'V_SUFF_BASE',-,56)),
	cf(1,phi(55,var(0))),
	cf(1,terminal(56,'+Verb',[35])),
	cf(1,phi(56,var(0))),
	cf(1,subtree(53,'V_SUFF_BASE',-,54)),
	cf(1,phi(53,var(0))),
	cf(1,terminal(54,'+Past',[35])),
	cf(1,phi(54,var(0))),
	cf(1,subtree(3991,'S',-,4004)),
	cf(1,phi(3991,var(0))),
	cf(1,cproj(3991,var(90))),
	cf(1,subtree(4004,'VPmain',-,3473)),
	cf(1,phi(4004,var(0))),
	cf(1,cproj(4004,var(92))),
	cf(1,subtree(3473,'QuantP',3463,2778)),
	cf(1,phi(3473,var(12))),
	cf(1,cproj(3473,var(94))),
	cf(or(A3,A4),subtree(3463,'QuantP',-,109)),
	cf(or(A3,A4),phi(3463,var(12))),
	cf(or(A3,A4),subtree(109,'ART',-,66)),
	cf(or(A3,A4),phi(109,var(12))),
	cf(or(A3,A4),terminal(66,'en',[66])),
	cf(or(A3,A4),phi(66,var(116))),
	cf(or(A1,A2),subtree(3463,'QuantP',-,2937)),
	cf(or(A1,A2),phi(3463,var(12))),
	cf(or(A1,A2),subtree(2937,'NUMP',-,2931)),
	cf(or(A1,A2),phi(2937,var(69))),
	cf(or(A1,A2),subtree(2931,'NUM1P',-,107)),
	cf(or(A1,A2),phi(2931,var(69))),
	cf(or(A1,A2),subtree(107,'NUM1',-,66)),
	cf(or(A1,A2),phi(107,var(69))),
	cf(or(A1,A2),terminal(66,'en',[66])),
	cf(or(A1,A2),phi(66,var(69))),
	cf(1,subtree(2778,'NP',2770,2049)),
	cf(1,phi(2778,var(12))),
	cf(1,cproj(2778,var(95))),
	cf(1,subtree(2770,'NP',-,132)),
	cf(1,phi(2770,var(12))),
	cf(1,subtree(132,'N',2522,111)),
	cf(1,phi(132,var(12))),
	cf(1,cproj(132,var(97))),
	cf(1,subtree(2522,'N',2521,113)),
	cf(1,phi(2522,var(12))),
	cf(1,cproj(2522,var(97))),
	cf(1,subtree(2521,'N',2520,115)),
	cf(1,phi(2521,var(12))),
	cf(1,cproj(2521,var(97))),
	cf(1,subtree(2520,'N',2491,117)),
	cf(1,phi(2520,var(12))),
	cf(1,cproj(2520,var(97))),
	cf(1,subtree(2491,'N',-,121)),
	cf(1,phi(2491,var(12))),
	cf(1,cproj(2491,var(97))),
	cf(1,subtree(121,'N_BASE',-,120)),
	cf(1,phi(121,var(12))),
	cf(1,cproj(121,var(112))),
	cf(1,terminal(120,'måte',[110])),
	cf(1,phi(120,var(12))),
	cf(1,cproj(120,var(112))),
	cf(1,subtree(117,'N_SUFF_BASE',-,118)),
	cf(1,phi(117,var(12))),
	cf(1,terminal(118,'+Noun',[110])),
	cf(1,phi(118,var(12))),
	cf(1,subtree(115,'N_SUFF_BASE',-,116)),
	cf(1,phi(115,var(12))),
	cf(1,terminal(116,'+Masc',[110])),
	cf(1,phi(116,var(12))),
	cf(1,subtree(113,'N_SUFF_BASE',-,114)),
	cf(1,phi(113,var(12))),
	cf(1,terminal(114,'+Indef',[110])),
	cf(1,phi(114,var(12))),
	cf(1,subtree(111,'N_SUFF_BASE',-,112)),
	cf(1,phi(111,var(12))),
	cf(1,terminal(112,'+Sg',[110])),
	cf(1,phi(112,var(12))),
	cf(1,subtree(2049,'VP\'',2046,1726)),
	cf(1,phi(2049,var(41))),
	cf(1,cproj(2049,var(96))),
	cf(1,subtree(2046,'VP\'',-,167)),
	cf(1,phi(2046,var(41))),
	cf(1,subtree(167,'PARTinf',-,133)),
	cf(1,phi(167,var(41))),
	cf(1,terminal(133,'å',[133])),
	cf(1,phi(133,var(41))),
	cf(1,subtree(1726,'VP',1713,974)),
	cf(1,phi(1726,var(41))),
	cf(1,cproj(1726,var(98))),
	cf(1,subtree(1713,'VP',-,1709)),
	cf(1,phi(1713,var(41))),
	cf(1,subtree(1709,'V',1708,169)),
	cf(1,phi(1709,var(41))),
	cf(1,cproj(1709,var(102))),
	cf(1,subtree(1708,'V',1707,171)),
	cf(1,phi(1708,var(41))),
	cf(1,cproj(1708,var(102))),
	cf(1,subtree(1707,'V',-,173)),
	cf(1,phi(1707,var(41))),
	cf(1,cproj(1707,var(102))),
	cf(1,subtree(173,'V_BASE',-,174)),
	cf(1,phi(173,var(41))),
	cf(1,cproj(173,var(113))),
	cf(1,terminal(174,'bjeffe',[168])),
	cf(1,phi(174,var(41))),
	cf(1,cproj(174,var(113))),
	cf(1,subtree(171,'V_SUFF_BASE',-,172)),
	cf(1,phi(171,var(41))),
	cf(1,terminal(172,'+Verb',[168])),
	cf(1,phi(172,var(41))),
	cf(1,subtree(169,'V_SUFF_BASE',-,170)),
	cf(1,phi(169,var(41))),
	cf(1,terminal(170,'+Infin',[168])),
	cf(1,phi(170,var(41))),
	cf(1,subtree(974,'PP',-,972)),
	cf(1,phi(974,var(6))),
	cf(1,cproj(974,var(101))),
	cf(1,subtree(972,'P',971,197)),
	cf(1,phi(972,var(6))),
	cf(1,subtree(971,'P',-,188)),
	cf(1,phi(971,var(6))),
	cf(1,subtree(188,'P_BASE',-,189)),
	cf(1,phi(188,var(6))),
	cf(1,terminal(189,'på',[185])),
	cf(1,phi(189,var(117))),
	cf(1,subtree(197,'P_SUFF_BASE',-,198)),
	cf(1,phi(197,var(6))),
	cf(1,terminal(198,'+Prep',[185])),
	cf(1,phi(198,var(6))),
	cf(1,subtree(210,'PERIOD',-,203)),
	cf(1,phi(210,var(0))),
	cf(1,terminal(203,'.',[203])),
	cf(1,phi(203,var(0))),
	cf(or(A3,A1),semform_data(0,14,1,7)),
	cf(or(A3,A1),semform_data(1,14,1,7)),
	cf(or(A4,A2),semform_data(2,27,1,4)),
	cf(or(A4,A2),semform_data(3,27,1,4)),
	cf(or(A4,A2),semform_data(5,21,7,8)),
	cf(or(A3,A1),semform_data(6,6,8,8)),
	cf(1,semform_data(7,57,9,13)),
	cf(1,semform_data(8,57,9,13)),
	cf(or(A1,A2),semform_data(9,107,14,16)),
	cf(or(A3,A4),semform_data(9,109,14,16)),
	cf(or(A1,A2),semform_data(10,107,14,16)),
	cf(or(A3,A4),semform_data(12,109,14,16)),
	cf(or(A1,A2),semform_data(14,3463,14,16)),
	cf(1,semform_data(17,121,17,20)),
	cf(1,semform_data(18,121,17,20)),
	cf(1,semform_data(19,2778,17,34)),
	cf(1,semform_data(20,173,24,29)),
	cf(1,semform_data(21,173,24,29)),
	cf(1,semform_data(22,188,31,32)),
	cf(1,semform_data(23,188,31,32)),
	cf(1,fspan(var(0),1,34)),
	cf(1,fspan(var(11),1,8)),
	cf(1,fspan(var(12),14,34)),
	cf(or(A1,A2),fspan(var(69),14,16)),
	cf(1,fspan(var(41),22,34)),
	cf(1,fspan(var(6),31,34)),
	cf(1,surfaceform(3,'kattene',1,8)),
	cf(1,surfaceform(35,'fant',9,13)),
	cf(1,surfaceform(66,'en',14,16)),
	cf(1,surfaceform(110,'måte',17,21)),
	cf(1,surfaceform(133,'å',22,23)),
	cf(1,surfaceform(168,'bjeffe',24,30)),
	cf(1,surfaceform(185,'på',31,34)),
	cf(1,surfaceform(203,'.',33,34))
	]).

