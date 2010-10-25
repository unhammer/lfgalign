% -*- coding: iso-8859-1 -*-

fstructure('Når bjeffet hunden?',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.08 CPU seconds, 211 subtrees unified'),
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
	select(A1, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(15))),
	cf(A2,eq(attr(var(0),'OBJ'),var(16))),
	cf(1,eq(attr(var(0),'SUBJ'),var(17))),
	cf(A1,eq(attr(var(0),'ADJUNCT'),var(18))),
	cf(1,eq(attr(var(0),'CHECK'),var(19))),
	cf(A1,eq(attr(var(0),'FOCUS-INT'),var(20))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(21))),
	cf(1,eq(proj(var(0),'m::'),var(22))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'int')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(A1,eq(var(15),semform('bjeffe',6,[var(23)],[]))),
	cf(A2,eq(var(15),semform('nå',4,[var(17),var(16)],['NULL']))),
	cf(A2,eq(attr(var(16),'PRED'),semform('hund',12,[],[]))),
	cf(A2,eq(attr(var(16),'CHECK'),var(24))),
	cf(A2,eq(attr(var(16),'GEND'),var(25))),
	cf(A2,eq(attr(var(16),'NTYPE'),var(26))),
	cf(A2,eq(proj(var(16),'m::'),var(27))),
	cf(A2,eq(attr(var(16),'CASE'),'obl')),
	cf(A2,eq(attr(var(16),'DEF'),'+')),
	cf(A2,eq(attr(var(16),'NUM'),'sg')),
	cf(A2,eq(attr(var(16),'PERS'),'3')),
	cf(A2,eq(attr(var(24),'_SEL'),var(28))),
	cf(A2,eq(attr(var(24),'_DEF-MORPH'),'+')),
	cf(A2,eq(attr(var(24),'_NOUN'),'+')),
	cf(A2,eq(attr(var(24),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(28),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(28),'_ANIM'),'+')),
	cf(1,eq(attr(var(28),'_HUMAN'),'-')),
	cf(1,eq(attr(var(25),'FEM'),'-')),
	cf(1,eq(attr(var(25),'MASC'),'+')),
	cf(1,eq(attr(var(25),'NEUT'),'-')),
	cf(A2,eq(attr(var(26),'NSEM'),var(29))),
	cf(A2,eq(attr(var(26),'NSYN'),'common')),
	cf(A2,eq(attr(var(29),'COMMON'),'count')),
	cf(A2,eq(attr(var(27),'H-CONS'),var(30))),
	cf(A2,eq(attr(var(27),'RELS'),var(31))),
	cf(A2,eq(attr(var(27),'RELS_EL'),var(3))),
	cf(A2,eq(attr(var(27),'TOP'),var(32))),
	cf(A2,eq(attr(var(27),'_ANTECEDVAR'),var(33))),
	cf(A2,eq(attr(var(27),'_QUANT'),var(34))),
	cf(A1,eq(var(30),var(35))),
	cf(1,in_set(var(12),var(30))),
	cf(1,eq(attr(var(12),'OUTSCPD'),var(36))),
	cf(1,eq(attr(var(12),'SC_ARG'),var(37))),
	cf(1,eq(attr(var(12),'relation'),'qeq')),
	cf(1,eq(attr(var(36),'type'),'handle')),
	cf(1,eq(attr(var(37),'type'),'handle')),
	cf(1,in_set(var(1),var(31))),
	cf(A1,in_set(var(2),var(31))),
	cf(A2,in_set(var(3),var(31))),
	cf(1,eq(attr(var(1),'ARG0'),var(38))),
	cf(1,eq(attr(var(1),'BODY'),var(39))),
	cf(1,eq(attr(var(1),'LBL'),var(32))),
	cf(1,eq(attr(var(1),'RSTR'),var(37))),
	cf(1,eq(attr(var(1),'relation'),semform('def_q',17,[],[]))),
	cf(A1,eq(var(38),var(40))),
	cf(A2,eq(var(38),var(41))),
	cf(1,eq(attr(var(38),'DIV'),'-')),
	cf(1,eq(attr(var(38),'GRIND'),'-')),
	cf(1,eq(attr(var(38),'NATGEND'),'gender')),
	cf(1,eq(attr(var(38),'NUM'),'sg')),
	cf(1,eq(attr(var(38),'PERS'),'3')),
	cf(1,eq(attr(var(38),'type'),'ref-ind')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(A1,eq(attr(var(2),'ARG0'),var(40))),
	cf(A1,eq(attr(var(2),'LBL'),var(36))),
	cf(A1,eq(attr(var(2),'_CAT'),'n')),
	cf(A1,eq(attr(var(2),'relation'),semform('hund',13,[],[]))),
	cf(A1,eq(attr(var(40),'DIV'),'-')),
	cf(A1,eq(attr(var(40),'GRIND'),'-')),
	cf(A1,eq(attr(var(40),'NATGEND'),'gender')),
	cf(A1,eq(attr(var(40),'NUM'),'sg')),
	cf(A1,eq(attr(var(40),'PERS'),'3')),
	cf(A1,eq(attr(var(40),'type'),'ref-ind')),
	cf(A2,eq(attr(var(3),'ARG0'),var(41))),
	cf(A2,eq(attr(var(3),'LBL'),var(36))),
	cf(A2,eq(attr(var(3),'_CAT'),'n')),
	cf(A2,eq(attr(var(3),'relation'),semform('hund',13,[],[]))),
	cf(A2,eq(attr(var(41),'DIV'),'-')),
	cf(A2,eq(attr(var(41),'GRIND'),'-')),
	cf(A2,eq(attr(var(41),'NATGEND'),'gender')),
	cf(A2,eq(attr(var(41),'NUM'),'sg')),
	cf(A2,eq(attr(var(41),'PERS'),'3')),
	cf(A2,eq(attr(var(41),'type'),'ref-ind')),
	cf(A2,eq(attr(var(33),'DIV'),'-')),
	cf(A2,eq(attr(var(33),'GRIND'),'-')),
	cf(A2,eq(attr(var(33),'NATGEND'),'gender')),
	cf(A2,eq(attr(var(33),'NUM'),'sg')),
	cf(A2,eq(attr(var(33),'PERS'),'3')),
	cf(A2,eq(attr(var(33),'type'),'ref-ind')),
	cf(1,eq(attr(var(34),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(34),'TOP'),var(42))),
	cf(1,eq(attr(var(42),'type'),'handle')),
	cf(A1,eq(var(43),var(44))),
	cf(A1,eq(var(17),var(23))),
	cf(A2,eq(attr(var(17),'PRED'),semform('bjeff',8,[],[]))),
	cf(A2,eq(attr(var(17),'CHECK'),var(45))),
	cf(A2,eq(attr(var(17),'GEND'),var(46))),
	cf(A2,eq(attr(var(17),'NTYPE'),var(47))),
	cf(A2,eq(proj(var(17),'m::'),var(48))),
	cf(A2,eq(attr(var(17),'CASE'),'nom')),
	cf(A2,eq(attr(var(17),'DEF'),'+')),
	cf(A2,eq(attr(var(17),'NUM'),'sg')),
	cf(A2,eq(attr(var(17),'PERS'),'3')),
	cf(A2,eq(attr(var(45),'_DEF-MORPH'),'+')),
	cf(A2,eq(attr(var(45),'_NOUN'),'+')),
	cf(A2,eq(attr(var(46),'FEM'),'-')),
	cf(A2,eq(attr(var(46),'MASC'),'-')),
	cf(A2,eq(attr(var(46),'NEUT'),'+')),
	cf(A2,eq(attr(var(47),'NSEM'),var(49))),
	cf(A2,eq(attr(var(47),'NSYN'),'common')),
	cf(A2,eq(attr(var(49),'COMMON'),'count')),
	cf(A2,eq(attr(var(48),'H-CONS'),var(50))),
	cf(A2,eq(attr(var(48),'RELS'),var(51))),
	cf(A2,eq(attr(var(48),'RELS_EL'),var(11))),
	cf(A2,eq(attr(var(48),'TOP'),var(52))),
	cf(A2,eq(attr(var(48),'_QUANT'),var(53))),
	cf(A2,in_set(var(4),var(50))),
	cf(A2,in_set(var(5),var(50))),
	cf(A2,eq(attr(var(4),'OUTSCPD'),var(54))),
	cf(A2,eq(attr(var(4),'SC_ARG'),var(55))),
	cf(A2,eq(attr(var(4),'relation'),'qeq')),
	cf(1,eq(attr(var(54),'type'),'handle')),
	cf(A2,eq(attr(var(55),'type'),'handle')),
	cf(A2,eq(attr(var(5),'OUTSCPD'),var(56))),
	cf(A2,eq(attr(var(5),'SC_ARG'),var(57))),
	cf(A2,eq(attr(var(5),'relation'),'qeq')),
	cf(A2,eq(attr(var(56),'type'),'handle')),
	cf(A2,eq(attr(var(57),'type'),'handle')),
	cf(A1,in_set(var(6),var(51))),
	cf(A1,in_set(var(7),var(51))),
	cf(A1,in_set(var(8),var(51))),
	cf(A1,in_set(var(9),var(51))),
	cf(A2,in_set(var(10),var(51))),
	cf(A2,in_set(var(11),var(51))),
	cf(A2,eq(var(6),var(11))),
	cf(1,eq(attr(var(6),'ARG0'),var(58))),
	cf(A1,eq(attr(var(6),'ARG1'),var(40))),
	cf(1,eq(attr(var(6),'LBL'),var(54))),
	cf(1,eq(attr(var(6),'_CAT'),var(59))),
	cf(1,eq(attr(var(6),'relation'),var(60))),
	cf(A1,eq(var(58),var(61))),
	cf(A2,eq(var(58),var(33))),
	cf(1,eq(attr(var(58),'type'),var(62))),
	cf(A2,eq(attr(var(58),'DIV'),'-')),
	cf(A2,eq(attr(var(58),'GRIND'),'-')),
	cf(A2,eq(attr(var(58),'NATGEND'),'gender')),
	cf(A2,eq(attr(var(58),'NUM'),'sg')),
	cf(A1,eq(attr(var(58),'PERF'),'-')),
	cf(A2,eq(attr(var(58),'PERS'),'3')),
	cf(A1,eq(var(62),'event')),
	cf(A2,eq(var(62),'ref-ind')),
	cf(A1,eq(var(59),'v')),
	cf(A2,eq(var(59),'n')),
	cf(A1,eq(var(60),semform('bjeffe',7,[],[]))),
	cf(A2,eq(var(60),semform('bjeff',9,[],[]))),
	cf(A1,eq(attr(var(7),'ARG0'),var(63))),
	cf(A1,eq(attr(var(7),'ARG1'),var(61))),
	cf(A1,eq(attr(var(7),'ARG2'),var(64))),
	cf(A1,eq(attr(var(7),'LBL'),var(54))),
	cf(A1,eq(attr(var(7),'relation'),semform('unspec_loc',1,[],[]))),
	cf(A1,eq(attr(var(63),'PERF'),'-')),
	cf(A1,eq(attr(var(63),'TENSE'),'notense')),
	cf(A1,eq(attr(var(63),'type'),'event')),
	cf(A1,eq(attr(var(61),'PERF'),'-')),
	cf(A1,eq(attr(var(61),'SF'),'ques')),
	cf(A1,eq(attr(var(61),'TENSE'),'past')),
	cf(A1,eq(attr(var(61),'type'),'event')),
	cf(A1,eq(attr(var(64),'NUM'),'sg')),
	cf(A1,eq(attr(var(64),'PERS'),'3')),
	cf(A1,eq(attr(var(64),'type'),'ref-ind')),
	cf(A1,eq(attr(var(8),'ARG0'),var(64))),
	cf(A1,eq(attr(var(8),'LBL'),var(65))),
	cf(A1,eq(attr(var(8),'relation'),semform('tid',3,[],[]))),
	cf(A1,eq(attr(var(65),'type'),'handle')),
	cf(A1,eq(attr(var(9),'ARG0'),var(64))),
	cf(A1,eq(attr(var(9),'BODY'),var(66))),
	cf(A1,eq(attr(var(9),'LBL'),var(67))),
	cf(A1,eq(attr(var(9),'RSTR'),var(68))),
	cf(A1,eq(attr(var(9),'relation'),semform('hvilken_q',2,[],[]))),
	cf(A1,eq(attr(var(66),'type'),'handle')),
	cf(A1,eq(attr(var(67),'type'),'handle')),
	cf(A1,eq(attr(var(68),'type'),'handle')),
	cf(A2,eq(attr(var(10),'ARG0'),var(33))),
	cf(A2,eq(attr(var(10),'BODY'),var(69))),
	cf(A2,eq(attr(var(10),'LBL'),var(52))),
	cf(A2,eq(attr(var(10),'RSTR'),var(55))),
	cf(A2,eq(attr(var(10),'relation'),semform('def_q',11,[],[]))),
	cf(A2,eq(attr(var(69),'type'),'handle')),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(A2,eq(attr(var(11),'ARG0'),var(33))),
	cf(A2,eq(attr(var(11),'LBL'),var(54))),
	cf(A2,eq(attr(var(11),'_CAT'),'n')),
	cf(A2,eq(attr(var(11),'relation'),semform('bjeff',9,[],[]))),
	cf(A2,eq(attr(var(53),'RELS_EL'),var(10))),
	cf(A2,eq(attr(var(53),'TOP'),var(70))),
	cf(A2,eq(attr(var(70),'type'),'handle')),
	cf(A1,in_set(var(20),var(18))),
	cf(A1,eq(attr(var(20),'PRED'),semform('når',0,[],[]))),
	cf(A1,eq(proj(var(20),'m::'),var(71))),
	cf(A1,eq(attr(var(20),'ADV-TYPE'),'temp')),
	cf(A1,eq(attr(var(71),'H-CONS'),var(35))),
	cf(A1,eq(attr(var(71),'RELS'),var(51))),
	cf(A1,eq(attr(var(71),'RELS_EL'),var(7))),
	cf(A1,eq(attr(var(71),'_QUANT'),var(72))),
	cf(A1,eq(attr(var(71),'_TOPLBL'),var(54))),
	cf(A2,eq(var(35),var(50))),
	cf(A1,in_set(var(12),var(35))),
	cf(A1,in_set(var(13),var(35))),
	cf(A1,in_set(var(14),var(35))),
	cf(A1,eq(attr(var(13),'OUTSCPD'),var(65))),
	cf(A1,eq(attr(var(13),'SC_ARG'),var(68))),
	cf(A1,eq(attr(var(13),'relation'),'qeq')),
	cf(A1,eq(attr(var(14),'OUTSCPD'),var(54))),
	cf(A1,eq(attr(var(14),'SC_ARG'),var(52))),
	cf(A1,eq(attr(var(14),'relation'),'qeq')),
	cf(A1,eq(attr(var(72),'RELS'),var(51))),
	cf(A1,eq(attr(var(72),'RELS_EL'),var(9))),
	cf(A1,eq(attr(var(72),'TOP'),var(73))),
	cf(A1,eq(attr(var(73),'type'),'handle')),
	cf(A1,eq(var(19),var(74))),
	cf(A2,eq(attr(var(19),'_MAIN-CL'),'+')),
	cf(A1,eq(var(75),var(76))),
	cf(A1,eq(var(21),var(77))),
	cf(A2,eq(attr(var(21),'MOOD'),'indicative')),
	cf(A2,eq(attr(var(21),'TENSE'),'pres')),
	cf(A1,eq(var(22),var(78))),
	cf(A2,eq(attr(var(22),'H-CONS'),var(50))),
	cf(A2,eq(attr(var(22),'INDEX'),var(79))),
	cf(A2,eq(attr(var(22),'RELS'),var(80))),
	cf(A2,eq(attr(var(22),'RELS_EL'),var(81))),
	cf(A2,eq(attr(var(22),'TOP'),var(57))),
	cf(A2,eq(attr(var(22),'_MSG'),var(82))),
	cf(A2,eq(attr(var(22),'_MSGQEQ'),var(5))),
	cf(A2,eq(attr(var(79),'PERF'),'-')),
	cf(A2,eq(attr(var(79),'SF'),'ques')),
	cf(A2,eq(attr(var(79),'TENSE'),'pres')),
	cf(A2,eq(attr(var(79),'type'),'event')),
	cf(A2,in_set(var(81),var(80))),
	cf(A2,eq(attr(var(81),'ARG0'),var(79))),
	cf(A2,eq(attr(var(81),'ARG1'),var(33))),
	cf(A2,eq(attr(var(81),'ARG2'),var(41))),
	cf(A2,eq(attr(var(81),'LBL'),var(56))),
	cf(A2,eq(attr(var(81),'_CAT'),'v')),
	cf(A2,eq(attr(var(81),'relation'),semform('nå',5,[],[]))),
	cf(A2,eq(attr(var(82),'ARG0'),var(79))),
	cf(A2,eq(attr(var(82),'LBL'),var(57))),
	cf(A2,eq(proj(var(83),'m::'),var(84))),
	cf(A2,eq(proj(var(85),'m::'),var(84))),
	cf(A2,eq(proj(var(86),'m::'),var(87))),
	cf(A2,eq(proj(var(88),'m::'),var(87))),
	cf(A1,eq(proj(var(89),'m::'),var(90))),
	cf(A1,eq(proj(var(91),'m::'),var(90))),
	cf(A1,eq(attr(var(23),'PRED'),semform('hund',12,[],[]))),
	cf(A1,eq(attr(var(23),'CHECK'),var(92))),
	cf(A1,eq(attr(var(23),'GEND'),var(25))),
	cf(A1,eq(attr(var(23),'NTYPE'),var(93))),
	cf(A1,eq(proj(var(23),'m::'),var(94))),
	cf(A1,eq(attr(var(23),'CASE'),'nom')),
	cf(A1,eq(attr(var(23),'DEF'),'+')),
	cf(A1,eq(attr(var(23),'NUM'),'sg')),
	cf(A1,eq(attr(var(23),'PERS'),'3')),
	cf(A1,eq(attr(var(92),'_SEL'),var(28))),
	cf(A1,eq(attr(var(92),'_DEF-MORPH'),'+')),
	cf(A1,eq(attr(var(92),'_NOUN'),'+')),
	cf(A1,eq(attr(var(93),'NSEM'),var(95))),
	cf(A1,eq(attr(var(93),'NSYN'),'common')),
	cf(A1,eq(attr(var(95),'COMMON'),'count')),
	cf(A1,eq(attr(var(94),'H-CONS'),var(35))),
	cf(A1,eq(attr(var(94),'RELS'),var(31))),
	cf(A1,eq(attr(var(94),'RELS_EL'),var(2))),
	cf(A1,eq(attr(var(94),'TOP'),var(32))),
	cf(A1,eq(attr(var(94),'_QUANT'),var(34))),
	cf(A2,eq(var(74),var(45))),
	cf(A2,eq(attr(var(74),'_DEF-MORPH'),'+')),
	cf(A1,eq(attr(var(74),'_MAIN-CL'),'+')),
	cf(A2,eq(attr(var(74),'_NOUN'),'+')),
	cf(A1,eq(attr(var(77),'MOOD'),'indicative')),
	cf(A1,eq(attr(var(77),'TENSE'),'past')),
	cf(A2,eq(var(78),var(48))),
	cf(1,eq(attr(var(78),'H-CONS'),var(35))),
	cf(A1,eq(attr(var(78),'INDEX'),var(61))),
	cf(1,eq(attr(var(78),'RELS'),var(51))),
	cf(1,eq(attr(var(78),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(78),'TOP'),var(52))),
	cf(A1,eq(attr(var(78),'_MSG'),var(96))),
	cf(A1,eq(attr(var(78),'_MSGQEQ'),var(14))),
	cf(A2,eq(attr(var(78),'_QUANT'),var(53))),
	cf(A1,eq(attr(var(96),'ARG0'),var(61))),
	cf(A1,eq(attr(var(96),'LBL'),var(52))),
	cf(1,eq(proj(var(97),'m::'),var(98))),
	cf(1,eq(proj(var(99),'m::'),var(98))),
	cf(A1,eq(var(100),var(23))),
	cf(A2,eq(var(100),var(16))),
	cf(1,eq(attr(var(100),'PRED'),semform('hund',12,[],[]))),
	cf(1,eq(attr(var(100),'CHECK'),var(101))),
	cf(1,eq(attr(var(100),'GEND'),var(25))),
	cf(1,eq(attr(var(100),'NTYPE'),var(102))),
	cf(1,eq(proj(var(100),'m::'),var(103))),
	cf(1,eq(attr(var(100),'DEF'),'+')),
	cf(1,eq(attr(var(100),'NUM'),'sg')),
	cf(1,eq(attr(var(100),'PERS'),'3')),
	cf(A1,eq(var(104),'nom')),
	cf(A2,eq(var(104),'obl')),
	cf(A1,eq(var(101),var(92))),
	cf(A2,eq(var(101),var(24))),
	cf(1,eq(attr(var(101),'_SEL'),var(28))),
	cf(1,eq(attr(var(101),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(101),'_NOUN'),'+')),
	cf(A1,eq(var(105),var(106))),
	cf(A2,eq(var(105),var(107))),
	cf(A1,eq(var(108),var(109))),
	cf(A2,eq(var(108),var(110))),
	cf(A1,eq(var(111),var(112))),
	cf(A2,eq(var(111),var(113))),
	cf(A1,eq(var(102),var(93))),
	cf(A2,eq(var(102),var(26))),
	cf(1,eq(attr(var(102),'NSEM'),var(114))),
	cf(1,eq(attr(var(102),'NSYN'),'common')),
	cf(A1,eq(var(114),var(95))),
	cf(A2,eq(var(114),var(29))),
	cf(1,eq(attr(var(114),'COMMON'),'count')),
	cf(A1,eq(var(115),var(116))),
	cf(A2,eq(var(115),var(117))),
	cf(A1,eq(var(118),var(119))),
	cf(A2,eq(var(118),var(120))),
	cf(A1,eq(var(103),var(94))),
	cf(A2,eq(var(103),var(27))),
	cf(1,eq(attr(var(103),'H-CONS'),var(30))),
	cf(1,eq(attr(var(103),'RELS'),var(31))),
	cf(1,eq(attr(var(103),'RELS_EL'),var(121))),
	cf(1,eq(attr(var(103),'TOP'),var(32))),
	cf(A2,eq(attr(var(103),'_ANTECEDVAR'),var(33))),
	cf(1,eq(attr(var(103),'_QUANT'),var(34))),
	cf(A1,eq(var(121),var(2))),
	cf(A2,eq(var(121),var(3))),
	cf(1,eq(attr(var(121),'ARG0'),var(38))),
	cf(1,eq(attr(var(121),'LBL'),var(36))),
	cf(1,eq(attr(var(121),'_CAT'),'n')),
	cf(1,eq(attr(var(121),'relation'),semform('hund',13,[],[])))
	],
	% C-Structure:
	[
	cf(1,subtree(721,'ROOT',999,1006)),
	cf(1,phi(721,var(0))),
	cf(1,subtree(999,'ROOT',-,1607)),
	cf(1,phi(999,var(0))),
	cf(A2,subtree(1607,'IP',-,1675)),
	cf(A2,phi(1607,var(0))),
	cf(A2,subtree(1675,'I\'',267,1626)),
	cf(A2,phi(1675,var(0))),
	cf(A2,subtree(267,'I\'',-,263)),
	cf(A2,phi(267,var(0))),
	cf(A2,subtree(263,'Vfin',259,11)),
	cf(A2,phi(263,var(0))),
	cf(A2,cproj(263,var(83))),
	cf(A2,subtree(259,'Vfin',255,13)),
	cf(A2,phi(259,var(0))),
	cf(A2,cproj(259,var(83))),
	cf(A2,subtree(255,'Vfin',-,15)),
	cf(A2,phi(255,var(0))),
	cf(A2,cproj(255,var(83))),
	cf(A2,subtree(15,'V_BASE',-,16)),
	cf(A2,phi(15,var(0))),
	cf(A2,cproj(15,var(85))),
	cf(A2,terminal(16,'nå',[3])),
	cf(A2,phi(16,var(0))),
	cf(A2,cproj(16,var(85))),
	cf(A2,subtree(13,'V_SUFF_BASE',-,14)),
	cf(A2,phi(13,var(0))),
	cf(A2,terminal(14,'+Verb',[3])),
	cf(A2,phi(14,var(0))),
	cf(A2,subtree(11,'V_SUFF_BASE',-,12)),
	cf(A2,phi(11,var(0))),
	cf(A2,terminal(12,'+Pres',[3])),
	cf(A2,phi(12,var(0))),
	cf(A2,subtree(1626,'S',1625,1582)),
	cf(A2,phi(1626,var(0))),
	cf(A2,subtree(1625,'S',-,1448)),
	cf(A2,phi(1625,var(0))),
	cf(A2,subtree(1448,'NP',-,18)),
	cf(A2,phi(1448,var(17))),
	cf(A2,subtree(18,'N',1213,32)),
	cf(A2,phi(18,var(17))),
	cf(A2,cproj(18,var(86))),
	cf(A2,subtree(1213,'N',1212,42)),
	cf(A2,phi(1213,var(17))),
	cf(A2,cproj(1213,var(86))),
	cf(A2,subtree(1212,'N',1211,44)),
	cf(A2,phi(1212,var(17))),
	cf(A2,cproj(1212,var(86))),
	cf(A2,subtree(1211,'N',1176,46)),
	cf(A2,phi(1211,var(17))),
	cf(A2,cproj(1211,var(86))),
	cf(A2,subtree(1176,'N',-,48)),
	cf(A2,phi(1176,var(17))),
	cf(A2,cproj(1176,var(86))),
	cf(A2,subtree(48,'N_BASE',-,49)),
	cf(A2,phi(48,var(17))),
	cf(A2,cproj(48,var(88))),
	cf(A2,terminal(49,'bjeff',[17])),
	cf(A2,phi(49,var(17))),
	cf(A2,cproj(49,var(88))),
	cf(A2,subtree(46,'N_SUFF_BASE',-,47)),
	cf(A2,phi(46,var(17))),
	cf(A2,terminal(47,'+Noun',[17])),
	cf(A2,phi(47,var(17))),
	cf(A2,subtree(44,'N_SUFF_BASE',-,45)),
	cf(A2,phi(44,var(17))),
	cf(A2,terminal(45,'+Neut',[17])),
	cf(A2,phi(45,var(17))),
	cf(A2,subtree(42,'N_SUFF_BASE',-,43)),
	cf(A2,phi(42,var(17))),
	cf(A2,terminal(43,'+Def',[17])),
	cf(A2,phi(43,var(122))),
	cf(A2,subtree(32,'N_SUFF_BASE',-,33)),
	cf(A2,phi(32,var(17))),
	cf(A2,terminal(33,'+Sg',[17])),
	cf(A2,phi(33,var(17))),
	cf(A2,subtree(1582,'VPmain',-,1049)),
	cf(A2,phi(1582,var(0))),
	cf(A1,subtree(1607,'IP',1752,1605)),
	cf(A1,phi(1607,var(0))),
	cf(A1,subtree(1752,'IP',-,1751)),
	cf(A1,phi(1752,var(0))),
	cf(A1,subtree(1751,'ADVPint',-,10)),
	cf(A1,phi(1751,var(20))),
	cf(A1,subtree(10,'ADVint',-,3)),
	cf(A1,phi(10,var(20))),
	cf(A1,terminal(3,'når',[3])),
	cf(A1,phi(3,var(123))),
	cf(A1,subtree(1605,'I\'',1572,1575)),
	cf(A1,phi(1605,var(0))),
	cf(A1,subtree(1572,'I\'',-,346)),
	cf(A1,phi(1572,var(0))),
	cf(A1,subtree(346,'Vfin',345,19)),
	cf(A1,phi(346,var(0))),
	cf(A1,cproj(346,var(89))),
	cf(A1,subtree(345,'Vfin',344,21)),
	cf(A1,phi(345,var(0))),
	cf(A1,cproj(345,var(89))),
	cf(A1,subtree(344,'Vfin',-,23)),
	cf(A1,phi(344,var(0))),
	cf(A1,cproj(344,var(89))),
	cf(A1,subtree(23,'V_BASE',-,24)),
	cf(A1,phi(23,var(0))),
	cf(A1,cproj(23,var(91))),
	cf(A1,terminal(24,'bjeffe',[17])),
	cf(A1,phi(24,var(0))),
	cf(A1,cproj(24,var(91))),
	cf(A1,subtree(21,'V_SUFF_BASE',-,22)),
	cf(A1,phi(21,var(0))),
	cf(A1,terminal(22,'+Verb',[17])),
	cf(A1,phi(22,var(0))),
	cf(A1,subtree(19,'V_SUFF_BASE',-,20)),
	cf(A1,phi(19,var(0))),
	cf(A1,terminal(20,'+Past',[17])),
	cf(A1,phi(20,var(0))),
	cf(A1,subtree(1575,'S',-,1049)),
	cf(A1,phi(1575,var(0))),
	cf(1,subtree(1049,'NP',-,57)),
	cf(1,phi(1049,var(100))),
	cf(1,subtree(57,'N',484,58)),
	cf(1,phi(57,var(100))),
	cf(1,cproj(57,var(97))),
	cf(1,subtree(484,'N',483,60)),
	cf(1,phi(484,var(100))),
	cf(1,cproj(484,var(97))),
	cf(1,subtree(483,'N',482,62)),
	cf(1,phi(483,var(100))),
	cf(1,cproj(483,var(97))),
	cf(1,subtree(482,'N',454,64)),
	cf(1,phi(482,var(100))),
	cf(1,cproj(482,var(97))),
	cf(1,subtree(454,'N',-,66)),
	cf(1,phi(454,var(100))),
	cf(1,cproj(454,var(97))),
	cf(1,subtree(66,'N_BASE',-,67)),
	cf(1,phi(66,var(100))),
	cf(1,cproj(66,var(99))),
	cf(1,terminal(67,'hund',[56])),
	cf(1,phi(67,var(100))),
	cf(1,cproj(67,var(99))),
	cf(1,subtree(64,'N_SUFF_BASE',-,65)),
	cf(1,phi(64,var(100))),
	cf(1,terminal(65,'+Noun',[56])),
	cf(1,phi(65,var(100))),
	cf(1,subtree(62,'N_SUFF_BASE',-,63)),
	cf(1,phi(62,var(100))),
	cf(1,terminal(63,'+Masc',[56])),
	cf(1,phi(63,var(100))),
	cf(1,subtree(60,'N_SUFF_BASE',-,61)),
	cf(1,phi(60,var(100))),
	cf(1,terminal(61,'+Def',[56])),
	cf(1,phi(61,var(124))),
	cf(1,subtree(58,'N_SUFF_BASE',-,59)),
	cf(1,phi(58,var(100))),
	cf(1,terminal(59,'+Sg',[56])),
	cf(1,phi(59,var(100))),
	cf(1,subtree(1006,'INT-MARK',1005,75)),
	cf(1,phi(1006,var(0))),
	cf(1,subtree(1005,'INT-MARK',1004,77)),
	cf(1,phi(1005,var(0))),
	cf(1,subtree(1004,'INT-MARK',-,79)),
	cf(1,phi(1004,var(0))),
	cf(1,subtree(79,'INT-MARK_BASE',-,80)),
	cf(1,phi(79,var(0))),
	cf(1,terminal(80,'?',[74])),
	cf(1,phi(80,var(0))),
	cf(1,subtree(77,'Punct_SUFF_BASE',-,78)),
	cf(1,phi(77,var(0))),
	cf(1,terminal(78,'+Punct',[74])),
	cf(1,phi(78,var(0))),
	cf(1,subtree(75,'SENT_SUFF_BASE',-,76)),
	cf(1,phi(75,var(0))),
	cf(1,terminal(76,'+Sent',[74])),
	cf(1,phi(76,var(0))),
	cf(A1,semform_data(0,10,1,4)),
	cf(A1,semform_data(1,10,1,4)),
	cf(A1,semform_data(2,10,1,4)),
	cf(A1,semform_data(3,10,1,4)),
	cf(A2,semform_data(4,15,1,2)),
	cf(A2,semform_data(5,15,1,2)),
	cf(A1,semform_data(6,23,5,12)),
	cf(A1,semform_data(7,23,5,12)),
	cf(A2,semform_data(8,48,5,9)),
	cf(A2,semform_data(9,48,5,9)),
	cf(A2,semform_data(11,42,12,12)),
	cf(1,semform_data(12,66,13,16)),
	cf(1,semform_data(13,66,13,16)),
	cf(1,semform_data(17,60,19,19)),
	cf(1,fspan(var(0),1,20)),
	cf(A2,fspan(var(125),5,12)),
	cf(A1,fspan(var(20),1,4)),
	cf(1,fspan(var(100),13,19)),
	cf(1,fspan(var(0),20,20)),
	cf(1,surfaceform(3,'når',1,4)),
	cf(1,surfaceform(17,'bjeffet',5,12)),
	cf(1,surfaceform(56,'hunden',13,19)),
	cf(1,surfaceform(74,'?',19,20))
	]).
