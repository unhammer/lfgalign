% -*- coding: iso-8859-1 -*-

fstructure('Den bjeffende hunden jaget Browne.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('4 solutions, 0.12 CPU seconds, 217 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('25'),
	'max_medial2_constituent_weight'('20')
	],
	% Choices:
	[
	choice([A1,A2], 1),
	choice([B1,B2], A1),
	choice([C1,C2], A2)
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(18))),
	cf(A1,eq(attr(var(0),'OBJ'),var(19))),
	cf(A1,eq(attr(var(0),'SUBJ'),var(20))),
	cf(A2,eq(attr(var(0),'ADJUNCT'),var(21))),
	cf(A1,eq(attr(var(0),'TOPIC'),var(20))),
	cf(A2,eq(attr(var(0),'APP'),var(22))),
	cf(A2,eq(attr(var(0),'CASE'),var(23))),
	cf(1,eq(attr(var(0),'CHECK'),var(24))),
	cf(A2,eq(attr(var(0),'GEND'),var(25))),
	cf(A2,eq(attr(var(0),'REF'),var(26))),
	cf(A2,eq(attr(var(0),'SPEC'),var(27))),
	cf(A1,eq(attr(var(0),'STMT-TYPE'),var(28))),
	cf(A1,eq(attr(var(0),'TNS-ASP'),var(29))),
	cf(A1,eq(attr(var(0),'VFORM'),var(30))),
	cf(1,eq(proj(var(0),'m::'),var(31))),
	cf(A2,eq(attr(var(0),'DEF'),'+')),
	cf(A2,eq(attr(var(0),'NUM'),'sg')),
	cf(A2,eq(attr(var(0),'PERS'),'3')),
	cf(A1,eq(attr(var(0),'VTYPE'),'main')),
	cf(A2,eq(var(18),semform('pro',13,[],[]))),
	cf(A1,eq(var(18),semform('jage',20,[var(20),var(19)],['NULL']))),
	cf(A2,eq(var(19),var(32))),
	cf(1,eq(attr(var(19),'PRED'),var(33))),
	cf(A1,eq(attr(var(19),'CHECK'),var(34))),
	cf(A2,eq(attr(var(19),'GEND'),var(25))),
	cf(A1,eq(attr(var(19),'NTYPE'),var(35))),
	cf(1,eq(proj(var(19),'m::'),var(36))),
	cf(A1,eq(attr(var(19),'CASE'),'obl')),
	cf(A1,eq(attr(var(19),'DEF'),'+')),
	cf(1,eq(attr(var(19),'NUM'),'sg')),
	cf(1,eq(attr(var(19),'PERS'),'3')),
	cf(A1,eq(attr(var(19),'REF'),'+')),
	cf(A2,eq(var(33),semform('pro',15,[],[]))),
	cf(A1,eq(var(33),semform('Browne',22,[],[]))),
	cf(A1,eq(attr(var(34),'_DEF-MORPH'),'-')),
	cf(A1,eq(attr(var(34),'_PREPEXISTS'),'-')),
	cf(A2,eq(attr(var(25),'NEUT'),'-')),
	cf(1,eq(attr(var(35),'NSEM'),var(37))),
	cf(1,eq(attr(var(35),'NSYN'),'proper')),
	cf(1,eq(attr(var(37),'PROPER'),var(38))),
	cf(1,eq(attr(var(38),'PROPER-TYPE'),'name')),
	cf(A2,eq(var(39),var(40))),
	cf(A2,eq(var(36),var(41))),
	cf(A1,eq(attr(var(36),'H-CONS'),var(42))),
	cf(A1,eq(attr(var(36),'RELS'),var(43))),
	cf(1,eq(attr(var(36),'RELS_EL'),var(2))),
	cf(A1,eq(attr(var(36),'TOP'),var(44))),
	cf(A1,eq(attr(var(36),'_ANTECEDVAR'),var(45))),
	cf(A1,eq(attr(var(36),'_QUANT'),var(46))),
	cf(1,in_set(var(47),var(42))),
	cf(1,eq(attr(var(47),'OUTSCPD'),var(48))),
	cf(1,eq(attr(var(47),'SC_ARG'),var(49))),
	cf(1,eq(attr(var(47),'relation'),'qeq')),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(49),'type'),'handle')),
	cf(1,in_set(var(1),var(43))),
	cf(A1,in_set(var(2),var(43))),
	cf(A2,in_set(var(3),var(43))),
	cf(1,eq(attr(var(1),'ARG0'),var(50))),
	cf(1,eq(attr(var(1),'BODY'),var(51))),
	cf(1,eq(attr(var(1),'LBL'),var(44))),
	cf(1,eq(attr(var(1),'RSTR'),var(49))),
	cf(1,eq(attr(var(1),'relation'),semform('proper_q',24,[],[]))),
	cf(A2,eq(var(50),var(52))),
	cf(A1,eq(var(50),var(53))),
	cf(1,eq(attr(var(50),'NUM'),'sg')),
	cf(1,eq(attr(var(50),'PERS'),'3')),
	cf(1,eq(attr(var(50),'type'),'ref-ind')),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(44),'type'),'handle')),
	cf(A2,eq(var(2),var(54))),
	cf(1,eq(attr(var(2),'ARG0'),var(53))),
	cf(A1,eq(attr(var(2),'LBL'),var(48))),
	cf(A1,eq(attr(var(2),'CARG'),'Browne')),
	cf(A1,eq(attr(var(2),'relation'),semform('named',23,[],[]))),
	cf(A2,eq(var(53),var(55))),
	cf(A1,eq(attr(var(53),'NUM'),'sg')),
	cf(A1,eq(attr(var(53),'PERS'),'3')),
	cf(A1,eq(attr(var(53),'type'),'ref-ind')),
	cf(A2,eq(attr(var(3),'ARG0'),var(52))),
	cf(A2,eq(attr(var(3),'LBL'),var(48))),
	cf(A2,eq(attr(var(3),'CARG'),'Browne')),
	cf(A2,eq(attr(var(3),'relation'),semform('named',23,[],[]))),
	cf(A2,eq(attr(var(52),'NUM'),'sg')),
	cf(A2,eq(attr(var(52),'PERS'),'3')),
	cf(A2,eq(attr(var(52),'type'),'ref-ind')),
	cf(1,eq(attr(var(45),'DIV'),'-')),
	cf(1,eq(attr(var(45),'GRIND'),'-')),
	cf(1,eq(attr(var(45),'NATGEND'),'gender')),
	cf(1,eq(attr(var(45),'NUM'),'sg')),
	cf(1,eq(attr(var(45),'PERS'),'3')),
	cf(1,eq(attr(var(45),'type'),'ref-ind')),
	cf(1,eq(attr(var(46),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(46),'TOP'),var(56))),
	cf(1,eq(attr(var(56),'type'),'handle')),
	cf(A1,eq(attr(var(20),'PRED'),semform('hund',16,[],[]))),
	cf(A1,eq(attr(var(20),'ADJUNCT'),var(57))),
	cf(A1,eq(attr(var(20),'CHECK'),var(58))),
	cf(A1,eq(attr(var(20),'GEND'),var(59))),
	cf(A1,eq(attr(var(20),'NTYPE'),var(60))),
	cf(A1,eq(attr(var(20),'REF'),var(61))),
	cf(A1,eq(attr(var(20),'SPEC'),var(62))),
	cf(A1,eq(proj(var(20),'m::'),var(63))),
	cf(A1,eq(attr(var(20),'CASE'),'nom')),
	cf(A1,eq(attr(var(20),'DEF'),'+')),
	cf(A1,eq(attr(var(20),'NUM'),'sg')),
	cf(A1,eq(attr(var(20),'PERS'),'3')),
	cf(A1,in_set(var(14),var(57))),
	cf(1,eq(attr(var(14),'PRED'),semform('bjeffe',10,[var(64)],[]))),
	cf(1,eq(attr(var(14),'SUBJ'),var(64))),
	cf(1,eq(attr(var(14),'CHECK'),var(65))),
	cf(1,eq(attr(var(14),'GEND'),var(66))),
	cf(1,eq(attr(var(14),'REF'),var(67))),
	cf(1,eq(proj(var(14),'m::'),var(68))),
	cf(1,eq(attr(var(14),'ATYPE'),'attributive')),
	cf(1,eq(attr(var(14),'DEF'),'+')),
	cf(1,eq(attr(var(14),'NUM'),'sg')),
	cf(1,eq(attr(var(14),'VFORM'),'prespart')),
	cf(1,eq(attr(var(14),'VTYPE'),'main')),
	cf(A1,eq(var(64),var(20))),
	cf(A2,eq(var(64),var(69))),
	cf(1,eq(proj(var(64),'m::'),var(70))),
	cf(A1,eq(var(71),var(72))),
	cf(A2,eq(var(71),var(30))),
	cf(A2,eq(var(70),var(73))),
	cf(A1,eq(var(70),var(63))),
	cf(1,eq(attr(var(70),'H-CONS'),var(74))),
	cf(1,eq(attr(var(70),'RELS_EL'),var(75))),
	cf(A2,eq(var(74),var(76))),
	cf(A1,eq(var(74),var(77))),
	cf(A2,eq(var(75),var(16))),
	cf(A1,eq(var(75),var(11))),
	cf(1,eq(attr(var(75),'ARG0'),var(78))),
	cf(A2,eq(var(78),var(55))),
	cf(A1,eq(var(78),var(45))),
	cf(1,eq(attr(var(65),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(65),'_DEG-MORPH'),'positive')),
	cf(A1,eq(var(66),var(59))),
	cf(A2,eq(var(66),var(25))),
	cf(A1,eq(var(67),var(61))),
	cf(A2,eq(var(67),var(26))),
	cf(1,eq(attr(var(68),'H-CONS'),var(74))),
	cf(1,eq(attr(var(68),'INDEX'),var(79))),
	cf(1,eq(attr(var(68),'RELS'),var(80))),
	cf(1,eq(attr(var(68),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(68),'TOP'),var(81))),
	cf(1,eq(attr(var(68),'_MSG'),var(82))),
	cf(1,eq(attr(var(79),'PERF'),'-')),
	cf(1,eq(attr(var(79),'PROG'),'+')),
	cf(1,eq(attr(var(79),'TENSE'),'notense')),
	cf(1,eq(attr(var(79),'type'),'event')),
	cf(A1,eq(var(80),var(83))),
	cf(A2,eq(var(80),var(84))),
	cf(1,in_set(var(8),var(80))),
	cf(1,eq(attr(var(8),'ARG0'),var(79))),
	cf(1,eq(attr(var(8),'ARG1'),var(78))),
	cf(1,eq(attr(var(8),'LBL'),var(85))),
	cf(1,eq(attr(var(8),'_CAT'),'v')),
	cf(1,eq(attr(var(8),'relation'),semform('bjeffe',11,[],[]))),
	cf(A1,eq(var(85),var(86))),
	cf(A2,eq(var(85),var(87))),
	cf(1,eq(attr(var(85),'type'),'handle')),
	cf(A1,eq(var(81),var(86))),
	cf(A2,eq(var(81),var(87))),
	cf(1,eq(attr(var(81),'type'),'handle')),
	cf(1,eq(attr(var(82),'ARG0'),var(79))),
	cf(1,eq(attr(var(82),'LBL'),var(81))),
	cf(A1,eq(attr(var(58),'_SEL'),var(88))),
	cf(A1,eq(attr(var(58),'_ATTR'),'+')),
	cf(A1,eq(attr(var(58),'_DEF-MORPH'),'+')),
	cf(A1,eq(attr(var(58),'_NOUN'),'+')),
	cf(A1,eq(attr(var(58),'_PREDEF'),'+')),
	cf(A1,eq(attr(var(58),'_PREDET'),'+')),
	cf(1,eq(attr(var(88),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(88),'_ANIM'),'+')),
	cf(1,eq(attr(var(88),'_HUMAN'),'-')),
	cf(1,eq(attr(var(59),'FEM'),'-')),
	cf(1,eq(attr(var(59),'MASC'),'+')),
	cf(1,eq(attr(var(59),'NEUT'),'-')),
	cf(A1,eq(attr(var(60),'NSEM'),var(89))),
	cf(A1,eq(attr(var(60),'NSYN'),'common')),
	cf(A1,eq(attr(var(89),'COMMON'),'count')),
	cf(A2,eq(var(62),var(27))),
	cf(1,eq(attr(var(62),'DET'),var(90))),
	cf(1,eq(attr(var(90),'PRED'),semform('den',0,[],[]))),
	cf(1,eq(attr(var(90),'DET-TYPE'),var(91))),
	cf(or(C1,B1),eq(attr(var(90),'DEIXIS'),'distal')),
	cf(or(C1,B1),eq(var(91),'demon')),
	cf(or(C2,B2),eq(var(91),'article')),
	cf(A1,eq(attr(var(63),'H-CONS'),var(77))),
	cf(A1,eq(attr(var(63),'RELS'),var(83))),
	cf(A1,eq(attr(var(63),'RELS_EL'),var(11))),
	cf(A1,eq(attr(var(63),'TOP'),var(92))),
	cf(B2,eq(attr(var(63),'_QUANT'),var(93))),
	cf(B1,in_set(var(4),var(77))),
	cf(B2,in_set(var(5),var(77))),
	cf(A2,in_set(var(6),var(77))),
	cf(A1,in_set(var(7),var(77))),
	cf(or(C1,B1),eq(attr(var(4),'OUTSCPD'),var(87))),
	cf(or(C1,B1),eq(attr(var(4),'SC_ARG'),var(94))),
	cf(or(C1,B1),eq(attr(var(4),'relation'),'qeq')),
	cf(A1,eq(var(87),var(86))),
	cf(A2,eq(attr(var(87),'type'),'handle')),
	cf(or(C1,B1),eq(attr(var(94),'type'),'handle')),
	cf(or(C2,B2),eq(attr(var(5),'OUTSCPD'),var(87))),
	cf(or(C2,B2),eq(attr(var(5),'SC_ARG'),var(95))),
	cf(or(C2,B2),eq(attr(var(5),'relation'),'qeq')),
	cf(or(C2,B2),eq(attr(var(95),'type'),'handle')),
	cf(A2,eq(attr(var(6),'OUTSCPD'),var(86))),
	cf(A2,eq(attr(var(6),'SC_ARG'),var(96))),
	cf(A2,eq(attr(var(6),'relation'),'qeq')),
	cf(1,eq(attr(var(86),'type'),'handle')),
	cf(B2,eq(var(96),var(95))),
	cf(A2,eq(attr(var(96),'type'),'handle')),
	cf(A1,eq(attr(var(7),'OUTSCPD'),var(97))),
	cf(A1,eq(attr(var(7),'SC_ARG'),var(98))),
	cf(A1,eq(attr(var(7),'relation'),'qeq')),
	cf(A2,eq(var(97),var(87))),
	cf(1,eq(attr(var(97),'type'),'handle')),
	cf(A2,eq(var(98),var(87))),
	cf(1,eq(attr(var(98),'type'),'handle')),
	cf(A1,in_set(var(8),var(83))),
	cf(B1,in_set(var(9),var(83))),
	cf(or(B2,A2),in_set(var(10),var(83))),
	cf(A1,in_set(var(11),var(83))),
	cf(A2,in_set(var(12),var(83))),
	cf(or(C1,B1),eq(attr(var(9),'ARG0'),var(99))),
	cf(or(C1,B1),eq(attr(var(9),'BODY'),var(100))),
	cf(or(C1,B1),eq(attr(var(9),'LBL'),var(101))),
	cf(or(C1,B1),eq(attr(var(9),'RSTR'),var(94))),
	cf(or(C1,B1),eq(attr(var(9),'relation'),semform('den_dem',1,[],[]))),
	cf(A2,eq(var(99),var(55))),
	cf(A1,eq(var(99),var(45))),
	cf(1,eq(attr(var(99),'NUM'),'sg')),
	cf(1,eq(attr(var(99),'PERS'),'3')),
	cf(or(C1,B1,A2),eq(attr(var(99),'type'),'ref-ind')),
	cf(or(C1,B1),eq(attr(var(100),'type'),'handle')),
	cf(A1,eq(var(101),var(92))),
	cf(1,eq(attr(var(101),'type'),'handle')),
	cf(or(B2,A2),eq(attr(var(10),'ARG0'),var(102))),
	cf(or(B2,A2),eq(attr(var(10),'BODY'),var(103))),
	cf(or(B2,A2),eq(attr(var(10),'LBL'),var(104))),
	cf(or(B2,A2),eq(attr(var(10),'RSTR'),var(96))),
	cf(or(B2,A2),eq(attr(var(10),'relation'),var(105))),
	cf(B2,eq(var(102),var(99))),
	cf(A2,eq(var(102),var(45))),
	cf(A2,eq(attr(var(102),'NUM'),'sg')),
	cf(A2,eq(attr(var(102),'PERS'),'3')),
	cf(B2,eq(var(103),var(106))),
	cf(A2,eq(attr(var(103),'type'),'handle')),
	cf(A2,eq(var(104),var(92))),
	cf(B2,eq(var(104),var(101))),
	cf(A2,eq(attr(var(104),'type'),'handle')),
	cf(A2,eq(var(105),semform('def_q',19,[],[]))),
	cf(B2,eq(var(105),semform('def_q',2,[],[]))),
	cf(A1,eq(attr(var(11),'ARG0'),var(45))),
	cf(A1,eq(attr(var(11),'LBL'),var(86))),
	cf(A1,eq(attr(var(11),'_CAT'),'n')),
	cf(A1,eq(attr(var(11),'relation'),semform('hund',17,[],[]))),
	cf(A1,eq(var(12),var(11))),
	cf(1,eq(attr(var(12),'ARG0'),var(45))),
	cf(A2,eq(attr(var(12),'LBL'),var(86))),
	cf(A2,eq(attr(var(12),'_CAT'),'n')),
	cf(A2,eq(attr(var(12),'relation'),semform('hund',17,[],[]))),
	cf(1,eq(attr(var(92),'type'),'handle')),
	cf(or(B2,A2),eq(attr(var(93),'RELS_EL'),var(10))),
	cf(or(B2,A2),eq(attr(var(93),'TOP'),var(107))),
	cf(A2,eq(var(107),var(108))),
	cf(B2,eq(var(107),var(109))),
	cf(A2,in_set(var(13),var(21))),
	cf(A2,in_set(var(14),var(21))),
	cf(A1,eq(var(13),var(0))),
	cf(1,eq(attr(var(13),'PRED'),semform('jage',20,[var(110),var(19)],['NULL']))),
	cf(1,eq(attr(var(13),'OBJ'),var(19))),
	cf(1,eq(attr(var(13),'SUBJ'),var(110))),
	cf(A1,eq(attr(var(13),'TOPIC'),var(20))),
	cf(A1,eq(attr(var(13),'CHECK'),var(111))),
	cf(1,eq(attr(var(13),'TNS-ASP'),var(29))),
	cf(A2,eq(attr(var(13),'TOPIC-REL'),var(32))),
	cf(1,eq(proj(var(13),'m::'),var(112))),
	cf(A2,eq(attr(var(13),'CLAUSE-TYPE'),'rel')),
	cf(A2,eq(attr(var(13),'RESTR'),'+')),
	cf(A1,eq(attr(var(13),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(13),'VFORM'),'fin')),
	cf(1,eq(attr(var(13),'VTYPE'),'main')),
	cf(A1,eq(var(110),var(20))),
	cf(A2,eq(attr(var(110),'PRED'),semform('hund',16,[],[]))),
	cf(A2,eq(attr(var(110),'CHECK'),var(113))),
	cf(A2,eq(attr(var(110),'GEND'),var(59))),
	cf(A2,eq(attr(var(110),'NTYPE'),var(114))),
	cf(1,eq(proj(var(110),'m::'),var(115))),
	cf(1,eq(attr(var(110),'CASE'),'nom')),
	cf(A2,eq(attr(var(110),'DEF'),'+')),
	cf(A2,eq(attr(var(110),'NUM'),'sg')),
	cf(A2,eq(attr(var(110),'PERS'),'3')),
	cf(A2,eq(attr(var(113),'_SEL'),var(88))),
	cf(A2,eq(attr(var(113),'_DEF-MORPH'),'+')),
	cf(A2,eq(attr(var(113),'_NOUN'),'+')),
	cf(A1,eq(var(116),'+')),
	cf(A1,eq(var(117),var(118))),
	cf(A2,eq(attr(var(114),'NSEM'),var(119))),
	cf(A2,eq(attr(var(114),'NSYN'),'common')),
	cf(A2,eq(attr(var(119),'COMMON'),'count')),
	cf(A1,eq(var(120),var(72))),
	cf(A1,eq(var(115),var(63))),
	cf(1,eq(attr(var(115),'H-CONS'),var(77))),
	cf(A2,eq(attr(var(115),'RELS'),var(83))),
	cf(1,eq(attr(var(115),'RELS_EL'),var(12))),
	cf(A2,eq(attr(var(115),'TOP'),var(92))),
	cf(A2,eq(attr(var(115),'_QUANT'),var(93))),
	cf(A1,eq(attr(var(111),'_MAIN-CL'),var(121))),
	cf(A1,eq(var(121),'+')),
	cf(1,eq(attr(var(29),'MOOD'),'indicative')),
	cf(1,eq(attr(var(29),'TENSE'),'past')),
	cf(A2,eq(attr(var(32),'PRED'),semform('pro',15,[],[]))),
	cf(A2,eq(attr(var(32),'GEND'),var(25))),
	cf(A2,eq(proj(var(32),'m::'),var(41))),
	cf(A2,eq(attr(var(32),'NUM'),'sg')),
	cf(A2,eq(attr(var(32),'PERS'),'3')),
	cf(A2,eq(attr(var(41),'RELS_EL'),var(54))),
	cf(A2,eq(attr(var(54),'ARG0'),var(55))),
	cf(A2,eq(attr(var(55),'NUM'),'sg')),
	cf(A2,eq(attr(var(55),'PERS'),'3')),
	cf(A2,eq(attr(var(55),'type'),'ref-ind')),
	cf(1,eq(attr(var(112),'H-CONS'),var(77))),
	cf(1,eq(attr(var(112),'INDEX'),var(122))),
	cf(1,eq(attr(var(112),'RELS'),var(123))),
	cf(1,eq(attr(var(112),'RELS_EL'),var(124))),
	cf(1,eq(attr(var(112),'TOP'),var(98))),
	cf(1,eq(attr(var(112),'_MSG'),var(125))),
	cf(A1,eq(attr(var(112),'_MSGQEQ'),var(7))),
	cf(1,eq(attr(var(122),'PERF'),'-')),
	cf(1,eq(attr(var(122),'SF'),'prop')),
	cf(1,eq(attr(var(122),'TENSE'),'past')),
	cf(1,eq(attr(var(122),'type'),'event')),
	cf(1,in_set(var(124),var(123))),
	cf(1,eq(attr(var(124),'ARG0'),var(122))),
	cf(1,eq(attr(var(124),'ARG1'),var(45))),
	cf(1,eq(attr(var(124),'ARG2'),var(53))),
	cf(1,eq(attr(var(124),'LBL'),var(97))),
	cf(1,eq(attr(var(124),'_CAT'),'v')),
	cf(1,eq(attr(var(124),'relation'),semform('jage',21,[],[]))),
	cf(1,eq(attr(var(125),'ARG0'),var(122))),
	cf(1,eq(attr(var(125),'LBL'),var(98))),
	cf(A2,eq(attr(var(22),'PRED'),semform('Browne',22,[],[]))),
	cf(A2,eq(attr(var(22),'CASE'),var(23))),
	cf(A2,eq(attr(var(22),'CHECK'),var(126))),
	cf(A2,eq(attr(var(22),'NTYPE'),var(35))),
	cf(A2,eq(proj(var(22),'m::'),var(127))),
	cf(A2,eq(attr(var(22),'DEF'),'+')),
	cf(A2,eq(attr(var(22),'NUM'),'sg')),
	cf(A2,eq(attr(var(22),'PERS'),'3')),
	cf(A2,eq(attr(var(22),'REF'),'+')),
	cf(A1,eq(var(126),var(34))),
	cf(1,eq(attr(var(126),'_DEF-MORPH'),'-')),
	cf(A1,eq(var(128),var(129))),
	cf(A2,eq(attr(var(127),'H-CONS'),var(42))),
	cf(A2,eq(attr(var(127),'RELS'),var(43))),
	cf(A2,eq(attr(var(127),'RELS_EL'),var(3))),
	cf(A2,eq(attr(var(127),'TOP'),var(44))),
	cf(A2,eq(attr(var(127),'_QUANT'),var(46))),
	cf(A1,eq(var(24),var(111))),
	cf(A2,eq(attr(var(24),'_ATTR'),'+')),
	cf(A2,eq(attr(var(24),'_PREDEF'),'+')),
	cf(A2,eq(attr(var(24),'_PREDET'),'+')),
	cf(B1,eq(var(130),var(117))),
	cf(A1,eq(var(131),var(132))),
	cf(A2,eq(attr(var(27),'DET'),var(90))),
	cf(A1,eq(var(28),'decl')),
	cf(A1,eq(var(30),'fin')),
	cf(A1,eq(var(31),var(112))),
	cf(A2,eq(attr(var(31),'H-CONS'),var(133))),
	cf(A2,eq(attr(var(31),'INDEX'),var(134))),
	cf(A2,eq(attr(var(31),'RELS'),var(135))),
	cf(A2,eq(attr(var(31),'RELS_EL'),var(136))),
	cf(A2,eq(attr(var(31),'TOP'),var(137))),
	cf(A2,eq(attr(var(31),'_MSG'),var(138))),
	cf(A2,eq(attr(var(31),'_MSGQEQ'),var(139))),
	cf(A2,in_set(var(139),var(133))),
	cf(A2,eq(attr(var(139),'OUTSCPD'),var(140))),
	cf(A2,eq(attr(var(139),'SC_ARG'),var(137))),
	cf(A2,eq(attr(var(139),'relation'),'qeq')),
	cf(A2,eq(attr(var(140),'type'),'handle')),
	cf(A2,eq(attr(var(137),'type'),'handle')),
	cf(A2,eq(attr(var(134),'TENSE'),'notense')),
	cf(A2,eq(attr(var(134),'type'),'event')),
	cf(A2,in_set(var(136),var(135))),
	cf(A2,eq(attr(var(136),'ARG'),var(55))),
	cf(A2,eq(attr(var(136),'ARG0'),var(134))),
	cf(A2,eq(attr(var(136),'LBL'),var(140))),
	cf(A2,eq(attr(var(136),'relation'),semform('unknown',9,[],[]))),
	cf(A2,eq(attr(var(138),'ARG0'),var(134))),
	cf(A2,eq(attr(var(138),'LBL'),var(137))),
	cf(A2,eq(attr(var(15),'ARG1'),var(55))),
	cf(A2,eq(attr(var(15),'ARG2'),var(52))),
	cf(A2,eq(attr(var(15),'LBL'),var(141))),
	cf(A2,eq(attr(var(15),'relation'),semform('ref-identity',6,[],[]))),
	cf(A2,eq(attr(var(141),'type'),'handle')),
	cf(A1,eq(var(16),var(11))),
	cf(1,eq(attr(var(16),'ARG0'),var(99))),
	cf(1,eq(attr(var(16),'LBL'),var(87))),
	cf(A2,eq(attr(var(16),'relation'),semform('generic_nonpro',14,[],[]))),
	cf(B2,eq(var(17),var(10))),
	cf(or(C2,B2),eq(attr(var(17),'ARG0'),var(99))),
	cf(or(C2,B2),eq(attr(var(17),'BODY'),var(106))),
	cf(or(C2,B2),eq(attr(var(17),'LBL'),var(101))),
	cf(or(C2,B2),eq(attr(var(17),'RSTR'),var(95))),
	cf(or(C2,B2),eq(attr(var(17),'relation'),semform('def_q',2,[],[]))),
	cf(or(C2,B2),eq(attr(var(106),'type'),'handle')),
	cf(or(C2,B2),eq(attr(var(109),'type'),'handle')),
	cf(1,eq(proj(var(142),'m::'),var(143))),
	cf(1,eq(proj(var(144),'m::'),var(143))),
	cf(A2,eq(attr(var(108),'type'),'handle')),
	cf(1,eq(proj(var(145),'m::'),var(146))),
	cf(1,eq(proj(var(147),'m::'),var(146))),
	cf(A2,eq(var(148),var(69))),
	cf(A1,eq(var(148),var(20))),
	cf(A2,eq(attr(var(148),'PRED'),semform('pro',13,[],[]))),
	cf(A2,eq(attr(var(148),'ADJUNCT'),var(149))),
	cf(A2,eq(attr(var(148),'APP'),var(22))),
	cf(A2,eq(attr(var(148),'CASE'),var(23))),
	cf(1,eq(attr(var(148),'CHECK'),var(150))),
	cf(1,eq(attr(var(148),'GEND'),var(151))),
	cf(A2,eq(attr(var(148),'REF'),var(26))),
	cf(1,eq(attr(var(148),'SPEC'),var(62))),
	cf(1,eq(proj(var(148),'m::'),var(73))),
	cf(1,eq(attr(var(148),'DEF'),'+')),
	cf(1,eq(attr(var(148),'NUM'),'sg')),
	cf(1,eq(attr(var(148),'PERS'),'3')),
	cf(B2,eq(var(149),var(57))),
	cf(A2,eq(var(149),var(21))),
	cf(A2,in_set(var(13),var(149))),
	cf(A2,in_set(var(14),var(149))),
	cf(A2,eq(var(150),var(24))),
	cf(A1,eq(var(150),var(58))),
	cf(A2,eq(attr(var(150),'_ATTR'),'+')),
	cf(A2,eq(attr(var(150),'_PREDEF'),'+')),
	cf(1,eq(attr(var(150),'_PREDET'),'+')),
	cf(A1,eq(var(151),var(59))),
	cf(A2,eq(var(151),var(25))),
	cf(1,eq(attr(var(151),'NEUT'),'-')),
	cf(A1,eq(var(73),var(63))),
	cf(1,eq(attr(var(73),'H-CONS'),var(76))),
	cf(1,eq(attr(var(73),'RELS'),var(84))),
	cf(1,eq(attr(var(73),'RELS_EL'),var(16))),
	cf(1,eq(attr(var(73),'TOP'),var(101))),
	cf(or(C2,B2),eq(attr(var(73),'_QUANT'),var(152))),
	cf(A1,eq(var(76),var(77))),
	cf(C1,in_set(var(4),var(76))),
	cf(C2,in_set(var(5),var(76))),
	cf(A1,eq(var(84),var(83))),
	cf(A2,in_set(var(8),var(84))),
	cf(C1,in_set(var(9),var(84))),
	cf(A2,in_set(var(15),var(84))),
	cf(A2,in_set(var(16),var(84))),
	cf(C2,in_set(var(17),var(84))),
	cf(B2,eq(var(152),var(93))),
	cf(or(C2,B2),eq(attr(var(152),'RELS_EL'),var(17))),
	cf(or(C2,B2),eq(attr(var(152),'TOP'),var(109))),
	cf(A1,eq(var(153),var(20))),
	cf(A2,eq(var(153),var(110))),
	cf(1,eq(attr(var(153),'PRED'),semform('hund',16,[],[]))),
	cf(A1,eq(attr(var(153),'ADJUNCT'),var(57))),
	cf(1,eq(attr(var(153),'CHECK'),var(154))),
	cf(1,eq(attr(var(153),'GEND'),var(59))),
	cf(1,eq(attr(var(153),'NTYPE'),var(155))),
	cf(A1,eq(attr(var(153),'REF'),var(156))),
	cf(A1,eq(attr(var(153),'SPEC'),var(62))),
	cf(1,eq(proj(var(153),'m::'),var(157))),
	cf(1,eq(attr(var(153),'DEF'),'+')),
	cf(1,eq(attr(var(153),'NUM'),'sg')),
	cf(1,eq(attr(var(153),'PERS'),'3')),
	cf(A1,eq(var(154),var(58))),
	cf(A2,eq(var(154),var(113))),
	cf(A1,eq(attr(var(154),'_PREDEF'),var(116))),
	cf(A1,eq(attr(var(154),'_PREDET'),var(158))),
	cf(1,eq(attr(var(154),'_SEL'),var(88))),
	cf(A1,eq(attr(var(154),'_ATTR'),'+')),
	cf(1,eq(attr(var(154),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(154),'_NOUN'),'+')),
	cf(A1,eq(var(158),'+')),
	cf(A2,eq(var(158),var(159))),
	cf(A1,eq(var(160),var(161))),
	cf(A2,eq(var(160),var(162))),
	cf(A1,eq(var(163),var(164))),
	cf(A2,eq(var(163),var(165))),
	cf(A1,eq(var(155),var(60))),
	cf(A2,eq(var(155),var(114))),
	cf(1,eq(attr(var(155),'NSEM'),var(166))),
	cf(1,eq(attr(var(155),'NSYN'),'common')),
	cf(A1,eq(var(166),var(89))),
	cf(A2,eq(var(166),var(119))),
	cf(1,eq(attr(var(166),'COMMON'),'count')),
	cf(A1,eq(var(167),var(168))),
	cf(A2,eq(var(167),var(169))),
	cf(A1,eq(var(156),var(61))),
	cf(A2,eq(var(156),var(170))),
	cf(A1,eq(var(157),var(63))),
	cf(A2,eq(var(157),var(115))),
	cf(1,eq(attr(var(157),'H-CONS'),var(77))),
	cf(1,eq(attr(var(157),'RELS'),var(83))),
	cf(1,eq(attr(var(157),'RELS_EL'),var(171))),
	cf(1,eq(attr(var(157),'TOP'),var(92))),
	cf(or(B2,A2),eq(attr(var(157),'_QUANT'),var(93))),
	cf(A1,eq(var(171),var(11))),
	cf(A2,eq(var(171),var(12))),
	cf(1,eq(attr(var(171),'ARG0'),var(45))),
	cf(1,eq(attr(var(171),'LBL'),var(86))),
	cf(1,eq(attr(var(171),'_CAT'),'n')),
	cf(1,eq(attr(var(171),'relation'),semform('hund',17,[],[]))),
	cf(A1,eq(var(172),var(19))),
	cf(A2,eq(var(172),var(22))),
	cf(1,eq(attr(var(172),'PRED'),semform('Browne',22,[],[]))),
	cf(A2,eq(attr(var(172),'CASE'),var(173))),
	cf(1,eq(attr(var(172),'CHECK'),var(126))),
	cf(1,eq(attr(var(172),'NTYPE'),var(35))),
	cf(1,eq(proj(var(172),'m::'),var(174))),
	cf(1,eq(attr(var(172),'DEF'),'+')),
	cf(1,eq(attr(var(172),'NUM'),'sg')),
	cf(1,eq(attr(var(172),'PERS'),'3')),
	cf(1,eq(attr(var(172),'REF'),'+')),
	cf(A1,eq(var(173),'obl')),
	cf(A2,eq(var(173),var(23))),
	cf(A1,eq(var(174),var(36))),
	cf(A2,eq(var(174),var(127))),
	cf(1,eq(attr(var(174),'H-CONS'),var(42))),
	cf(1,eq(attr(var(174),'RELS'),var(43))),
	cf(1,eq(attr(var(174),'RELS_EL'),var(175))),
	cf(1,eq(attr(var(174),'TOP'),var(44))),
	cf(A1,eq(attr(var(174),'_ANTECEDVAR'),var(45))),
	cf(1,eq(attr(var(174),'_QUANT'),var(46))),
	cf(A1,eq(var(175),var(2))),
	cf(A2,eq(var(175),var(3))),
	cf(1,eq(attr(var(175),'ARG0'),var(50))),
	cf(1,eq(attr(var(175),'LBL'),var(48))),
	cf(1,eq(attr(var(175),'CARG'),'Browne')),
	cf(1,eq(attr(var(175),'relation'),semform('named',23,[],[]))),
	cf(A2,eq(attr(var(69),'PRED'),semform('pro',13,[],[]))),
	cf(A2,eq(attr(var(69),'ADJUNCT'),var(21))),
	cf(A2,eq(attr(var(69),'APP'),var(22))),
	cf(A2,eq(attr(var(69),'CASE'),var(23))),
	cf(A2,eq(attr(var(69),'CHECK'),var(24))),
	cf(A2,eq(attr(var(69),'GEND'),var(25))),
	cf(A2,eq(attr(var(69),'REF'),var(26))),
	cf(A2,eq(attr(var(69),'SPEC'),var(27))),
	cf(A2,eq(proj(var(69),'m::'),var(73))),
	cf(A2,eq(attr(var(69),'DEF'),'+')),
	cf(A2,eq(attr(var(69),'NUM'),'sg')),
	cf(A2,eq(attr(var(69),'PERS'),'3'))
	],
	% C-Structure:
	[
	cf(1,subtree(1923,'ROOT',1920,109)),
	cf(1,phi(1923,var(0))),
	cf(A2,subtree(1920,'ROOT',-,1973)),
	cf(A2,phi(1920,var(0))),
	cf(A2,subtree(1973,'AppP',1963,857)),
	cf(A2,phi(1973,var(69))),
	cf(A2,subtree(1963,'AppP',-,2149)),
	cf(A2,phi(1963,var(69))),
	cf(A2,subtree(2149,'DP',285,1417)),
	cf(A2,phi(2149,var(69))),
	cf(A2,subtree(1417,'NP',2007,1838)),
	cf(A2,phi(1417,var(69))),
	cf(A2,subtree(2007,'NP',-,1877)),
	cf(A2,phi(2007,var(69))),
	cf(A2,subtree(1838,'CPnullc',-,1833)),
	cf(A2,phi(1838,var(13))),
	cf(A2,subtree(1833,'Ssub2',1828,1175)),
	cf(A2,phi(1833,var(13))),
	cf(A2,subtree(1828,'Ssub2',-,1708)),
	cf(A2,phi(1828,var(13))),
	cf(A2,subtree(1708,'NP',-,38)),
	cf(A2,phi(1708,var(110))),
	cf(A2,subtree(1175,'VPfin',-,1171)),
	cf(A2,phi(1175,var(13))),
	cf(A1,subtree(1920,'ROOT',-,2127)),
	cf(A1,phi(1920,var(0))),
	cf(A1,subtree(2127,'IP',2022,2125)),
	cf(A1,phi(2127,var(0))),
	cf(A1,subtree(2022,'IP',-,2147)),
	cf(A1,phi(2022,var(0))),
	cf(A1,subtree(2147,'DP',285,1889)),
	cf(A1,phi(2147,var(20))),
	cf(1,subtree(285,'DP',-,19)),
	cf(1,phi(285,var(148))),
	cf(1,subtree(19,'D',-,3)),
	cf(1,phi(19,var(148))),
	cf(1,terminal(3,'den',[3])),
	cf(1,phi(3,var(176))),
	cf(A1,subtree(1889,'NP',2009,38)),
	cf(A1,phi(1889,var(20))),
	cf(A1,subtree(2009,'NP',-,1877)),
	cf(A1,phi(2009,var(20))),
	cf(1,subtree(1877,'AP',-,1874)),
	cf(1,phi(1877,var(14))),
	cf(1,subtree(1874,'A',1873,22)),
	cf(1,phi(1874,var(14))),
	cf(1,subtree(1873,'A',1872,24)),
	cf(1,phi(1873,var(14))),
	cf(1,subtree(1872,'A',1871,26)),
	cf(1,phi(1872,var(14))),
	cf(1,subtree(1871,'A',-,28)),
	cf(1,phi(1871,var(14))),
	cf(1,subtree(28,'V_BASE',-,29)),
	cf(1,phi(28,var(14))),
	cf(1,terminal(29,'bjeffe',[20])),
	cf(1,phi(29,var(14))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(14))),
	cf(1,terminal(27,'+Verb',[20])),
	cf(1,phi(27,var(14))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(14))),
	cf(1,terminal(25,'+PresPart',[20])),
	cf(1,phi(25,var(14))),
	cf(1,subtree(22,'N_SUFF_BASE',-,23)),
	cf(1,phi(22,var(14))),
	cf(1,terminal(23,'+MFN',[20])),
	cf(1,phi(23,var(14))),
	cf(1,subtree(38,'N',913,39)),
	cf(1,phi(38,var(153))),
	cf(1,cproj(38,var(142))),
	cf(1,subtree(913,'N',912,41)),
	cf(1,phi(913,var(153))),
	cf(1,cproj(913,var(142))),
	cf(1,subtree(912,'N',911,43)),
	cf(1,phi(912,var(153))),
	cf(1,cproj(912,var(142))),
	cf(1,subtree(911,'N',883,45)),
	cf(1,phi(911,var(153))),
	cf(1,cproj(911,var(142))),
	cf(1,subtree(883,'N',-,47)),
	cf(1,phi(883,var(153))),
	cf(1,cproj(883,var(142))),
	cf(1,subtree(47,'N_BASE',-,48)),
	cf(1,phi(47,var(153))),
	cf(1,cproj(47,var(144))),
	cf(1,terminal(48,'hund',[37])),
	cf(1,phi(48,var(153))),
	cf(1,cproj(48,var(144))),
	cf(1,subtree(45,'N_SUFF_BASE',-,46)),
	cf(1,phi(45,var(153))),
	cf(1,terminal(46,'+Noun',[37])),
	cf(1,phi(46,var(153))),
	cf(1,subtree(43,'N_SUFF_BASE',-,44)),
	cf(1,phi(43,var(153))),
	cf(1,terminal(44,'+Masc',[37])),
	cf(1,phi(44,var(153))),
	cf(1,subtree(41,'N_SUFF_BASE',-,42)),
	cf(1,phi(41,var(153))),
	cf(1,terminal(42,'+Def',[37])),
	cf(1,phi(42,var(177))),
	cf(1,subtree(39,'N_SUFF_BASE',-,40)),
	cf(1,phi(39,var(153))),
	cf(1,terminal(40,'+Sg',[37])),
	cf(1,phi(40,var(153))),
	cf(A1,subtree(2125,'I\'',2092,2095)),
	cf(A1,phi(2125,var(0))),
	cf(A1,subtree(2092,'I\'',-,1171)),
	cf(A1,phi(2092,var(0))),
	cf(1,subtree(1171,'Vfin',1170,57)),
	cf(1,phi(1171,var(13))),
	cf(1,cproj(1171,var(145))),
	cf(1,subtree(1170,'Vfin',1169,59)),
	cf(1,phi(1170,var(13))),
	cf(1,cproj(1170,var(145))),
	cf(1,subtree(1169,'Vfin',-,61)),
	cf(1,phi(1169,var(13))),
	cf(1,cproj(1169,var(145))),
	cf(1,subtree(61,'V_BASE',-,62)),
	cf(1,phi(61,var(13))),
	cf(1,cproj(61,var(147))),
	cf(1,terminal(62,'jage',[55])),
	cf(1,phi(62,var(178))),
	cf(1,cproj(62,var(147))),
	cf(1,subtree(59,'V_SUFF_BASE',-,60)),
	cf(1,phi(59,var(13))),
	cf(1,terminal(60,'+Verb',[55])),
	cf(1,phi(60,var(13))),
	cf(1,subtree(57,'V_SUFF_BASE',-,58)),
	cf(1,phi(57,var(13))),
	cf(1,terminal(58,'+Past',[55])),
	cf(1,phi(58,var(13))),
	cf(A1,subtree(2095,'S',-,2102)),
	cf(A1,phi(2095,var(0))),
	cf(A1,subtree(2102,'VPmain',-,857)),
	cf(A1,phi(2102,var(0))),
	cf(1,subtree(857,'PROPP',-,617)),
	cf(1,phi(857,var(172))),
	cf(1,subtree(617,'PROP',616,95)),
	cf(1,phi(617,var(172))),
	cf(1,subtree(616,'PROP',613,97)),
	cf(1,phi(616,var(172))),
	cf(1,subtree(613,'PROP',-,99)),
	cf(1,phi(613,var(172))),
	cf(1,subtree(99,'PROP_BASE',-,100)),
	cf(1,phi(99,var(172))),
	cf(1,terminal(100,'Browne',[94])),
	cf(1,phi(100,var(172))),
	cf(1,subtree(97,'N_SUFF_BASE',-,98)),
	cf(1,phi(97,var(172))),
	cf(1,terminal(98,'+Prop',[94])),
	cf(1,phi(98,var(172))),
	cf(1,subtree(95,'N_SUFF_BASE',-,96)),
	cf(1,phi(95,var(172))),
	cf(1,terminal(96,'+Indef',[94])),
	cf(1,phi(96,var(172))),
	cf(1,subtree(109,'PERIOD',-,102)),
	cf(1,phi(109,var(0))),
	cf(1,terminal(102,'.',[102])),
	cf(1,phi(102,var(0))),
	cf(1,semform_data(0,19,1,4)),
	cf(or(C1,B1),semform_data(1,19,1,4)),
	cf(or(C2,B2),semform_data(2,19,1,4)),
	cf(A2,semform_data(6,1973,1,34)),
	cf(A2,semform_data(9,1920,1,34)),
	cf(1,semform_data(10,28,5,10)),
	cf(1,semform_data(11,28,5,10)),
	cf(A2,semform_data(13,2007,5,14)),
	cf(A2,semform_data(14,2007,5,14)),
	cf(A2,semform_data(15,1417,5,27)),
	cf(1,semform_data(16,47,15,18)),
	cf(1,semform_data(17,47,15,18)),
	cf(A2,semform_data(19,41,21,21)),
	cf(1,semform_data(20,61,22,27)),
	cf(1,semform_data(21,61,22,27)),
	cf(1,semform_data(22,99,28,33)),
	cf(1,semform_data(23,99,28,33)),
	cf(1,semform_data(24,99,28,33)),
	cf(1,fspan(var(0),1,29)),
	cf(A2,fspan(var(148),1,34)),
	cf(A2,fspan(var(13),15,27)),
	cf(A2,fspan(var(153),15,21)),
	cf(A1,fspan(var(153),1,21)),
	cf(1,fspan(var(14),5,14)),
	cf(1,fspan(var(172),28,34)),
	cf(1,fspan(var(0),28,29)),
	cf(1,surfaceform(3,'den',1,4)),
	cf(1,surfaceform(20,'bjeffende',5,14)),
	cf(1,surfaceform(37,'hunden',15,21)),
	cf(1,surfaceform(55,'jaget',22,27)),
	cf(1,surfaceform(94,'Browne',28,34)),
	cf(1,surfaceform(102,'.',28,29))
	]).

