% -*- coding: iso-8859-1 -*-

fstructure('Katten jaget en.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('8 solutions, 0.11 CPU seconds, 172 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('25'),
	'max_medial2_constituent_weight'('20')
	],
	% Choices:
	[
	choice([A1,A2], 1),
	choice([B1,B2,B3,B4,B5,B6], A1),
	choice([C1,C2], A2)
	],
	% Equivalences:
	[
	select(B6, 1),
	select(A1, 1),
	define(CV_001, or(or(B5,B6),or(B1,B2),A1)),
	define(CV_002, or(B4,or(B3,or(B4,B5,B6)),A1)),
	define(CV_003, or(B4,B3,or(B1,B2,B3))),
	define(CV_004, or(B4,or(B1,B2),or(B1,B2,B3))),
	define(CV_005, or(B3,or(B4,B5,B6))),
	define(CV_006, or(C2,or(B4,B6),B2)),
	define(CV_007, or(C1,B5,B1,B3)),
	define(CV_008, or(B4,or(B1,B2,B3)))
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('jage',6,[var(16),var(17)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(16))),
	cf(1,eq(attr(var(0),'OBJ'),var(17))),
	cf(1,eq(attr(var(0),'TOPIC'),var(18))),
	cf(1,eq(attr(var(0),'CHECK'),var(19))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(20))),
	cf(1,eq(proj(var(0),'m::'),var(21))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(A1,eq(var(16),var(18))),
	cf(A2,eq(attr(var(16),'PRED'),semform('en',8,[],[]))),
	cf(A2,eq(attr(var(16),'CHECK'),var(22))),
	cf(A2,eq(attr(var(16),'GEND'),var(23))),
	cf(A2,eq(attr(var(16),'NTYPE'),var(24))),
	cf(1,eq(proj(var(16),'m::'),var(25))),
	cf(1,eq(attr(var(16),'CASE'),'nom')),
	cf(A2,eq(attr(var(16),'DEF'),'+')),
	cf(A2,eq(attr(var(16),'PERS'),'3')),
	cf(A2,eq(attr(var(16),'PRON-FORM'),'en')),
	cf(A2,eq(attr(var(16),'PRON-TYPE'),'pers')),
	cf(A2,eq(attr(var(16),'REF'),'+')),
	cf(A2,eq(attr(var(22),'_SEL'),var(26))),
	cf(A2,eq(attr(var(26),'_ABSTRACT'),'-')),
	cf(A2,eq(attr(var(26),'_ANIM'),'+')),
	cf(A2,eq(attr(var(26),'_HUMAN'),'+')),
	cf(A2,eq(attr(var(23),'FEM'),'-')),
	cf(A2,eq(attr(var(23),'MASC'),'+')),
	cf(A2,eq(attr(var(23),'NEUT'),'-')),
	cf(A2,eq(attr(var(24),'NSYN'),'pronoun')),
	cf(A1,eq(var(25),var(27))),
	cf(1,eq(attr(var(25),'H-CONS'),var(28))),
	cf(A2,eq(attr(var(25),'RELS'),var(29))),
	cf(1,eq(attr(var(25),'RELS_EL'),var(4))),
	cf(A2,eq(attr(var(25),'TOP'),var(30))),
	cf(A2,eq(attr(var(25),'_QUANT'),var(31))),
	cf(A1,in_set(var(1),var(28))),
	cf(1,in_set(var(2),var(28))),
	cf(A2,in_set(var(3),var(28))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(32))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(33))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(34))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(35))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(35),'type'),'handle')),
	cf(A2,eq(attr(var(3),'OUTSCPD'),var(36))),
	cf(A2,eq(attr(var(3),'SC_ARG'),var(37))),
	cf(A2,eq(attr(var(3),'relation'),'qeq')),
	cf(A2,eq(attr(var(36),'type'),'handle')),
	cf(A2,eq(attr(var(37),'type'),'handle')),
	cf(A2,in_set(var(4),var(29))),
	cf(A2,in_set(var(5),var(29))),
	cf(A1,eq(var(4),var(12))),
	cf(1,eq(attr(var(4),'ARG0'),var(38))),
	cf(A2,eq(attr(var(4),'LBL'),var(36))),
	cf(A2,eq(attr(var(4),'relation'),semform('generic_pron',24,[],[]))),
	cf(1,eq(attr(var(38),'NATGEND'),var(39))),
	cf(A1,eq(attr(var(38),'DIV'),'-')),
	cf(1,eq(attr(var(38),'GRIND'),'-')),
	cf(A1,eq(attr(var(38),'NUM'),'sg')),
	cf(1,eq(attr(var(38),'PERS'),'3')),
	cf(1,eq(attr(var(38),'type'),'ref-ind')),
	cf(A2,eq(var(39),'animate')),
	cf(A1,eq(var(39),'gender')),
	cf(A2,eq(attr(var(5),'ARG0'),var(38))),
	cf(A2,eq(attr(var(5),'BODY'),var(40))),
	cf(A2,eq(attr(var(5),'LBL'),var(30))),
	cf(A2,eq(attr(var(5),'RSTR'),var(37))),
	cf(A2,eq(attr(var(5),'relation'),semform('pronoun_q',25,[],[]))),
	cf(A2,eq(attr(var(40),'type'),'handle')),
	cf(A2,eq(attr(var(30),'type'),'handle')),
	cf(A2,eq(attr(var(31),'RELS_EL'),var(5))),
	cf(A2,eq(var(17),var(18))),
	cf(A1,eq(attr(var(17),'PRED'),var(41))),
	cf(or(B1,B2),eq(attr(var(17),'AGRGEND'),var(42))),
	cf(or(B1,B2),eq(attr(var(17),'AGRNUM'),var(43))),
	cf(or(CV_005,A1),eq(attr(var(17),'CHECK'),var(44))),
	cf(CV_005,eq(attr(var(17),'DEF'),var(45))),
	cf(or(B1,B2),eq(attr(var(17),'DIGVALUE'),var(46))),
	cf(CV_005,eq(attr(var(17),'GEND'),var(47))),
	cf(or(B1,B2),eq(attr(var(17),'HEADNUM'),var(48))),
	cf(CV_005,eq(attr(var(17),'NUM'),var(49))),
	cf(or(B5,B6),eq(attr(var(17),'PERS'),var(50))),
	cf(or(CV_005,A1),eq(attr(var(17),'REF'),var(51))),
	cf(CV_005,eq(attr(var(17),'SPEC'),var(52))),
	cf(1,eq(proj(var(17),'m::'),var(53))),
	cf(1,eq(attr(var(17),'CASE'),'obl')),
	cf(or(B1,B2),eq(var(41),var(54))),
	cf(CV_005,eq(var(41),var(55))),
	cf(or(B4,B3),eq(var(42),var(47))),
	cf(CV_003,eq(attr(var(42),'NEUT'),var(56))),
	cf(CV_008,eq(var(56),'-')),
	cf(CV_003,eq(var(43),'sg')),
	cf(or(B4,B3),eq(var(43),var(49))),
	cf(or(CV_005,A1),eq(attr(var(44),'_PREPEXISTS'),var(57))),
	cf(A1,eq(var(57),'-')),
	cf(or(B1,B2),eq(var(58),var(59))),
	cf(CV_005,eq(var(58),var(60))),
	cf(CV_005,eq(var(45),'-')),
	cf(or(B1,B2),eq(var(61),var(62))),
	cf(CV_008,eq(var(46),'1')),
	cf(or(B1,B2),eq(var(63),var(64))),
	cf(or(B4,B3,or(B4,B5,B6)),eq(attr(var(47),'NEUT'),var(65))),
	cf(or(B5,B6),eq(attr(var(47),'FEM'),'-')),
	cf(or(B5,B6),eq(attr(var(47),'MASC'),'+')),
	cf(or(B5,B6),eq(var(65),'-')),
	cf(or(B4,B3),eq(var(65),var(56))),
	cf(CV_003,eq(var(48),'sg')),
	cf(CV_005,eq(var(49),'sg')),
	cf(or(B1,B2),eq(var(50),var(66))),
	cf(or(B5,B6),eq(var(50),'3')),
	cf(or(B1,B2),eq(var(67),var(68))),
	cf(A1,eq(var(51),'+')),
	cf(or(B5,B6),eq(attr(var(52),'DET'),var(69))),
	cf(or(B4,B3),eq(attr(var(52),'NUMBER'),var(70))),
	cf(or(B5,B6),eq(attr(var(69),'PRED'),semform('en',8,[],[]))),
	cf(or(B5,B6),eq(attr(var(69),'DET-TYPE'),'article')),
	cf(or(B4,B3),eq(attr(var(70),'PRED'),semform('en',8,[],[]))),
	cf(or(B4,B3),eq(attr(var(70),'AGRGEND'),var(47))),
	cf(or(B4,B3),eq(attr(var(70),'AGRNUM'),var(49))),
	cf(or(B4,B3),eq(proj(var(70),'m::'),var(71))),
	cf(or(B4,B3),eq(attr(var(70),'DIGVALUE'),'1')),
	cf(or(B4,B3),eq(attr(var(70),'HEADNUM'),'sg')),
	cf(or(B4,B3),eq(attr(var(71),'RELS'),var(72))),
	cf(or(B4,B3),eq(attr(var(71),'_CARD'),var(6))),
	cf(or(B4,B3),eq(attr(var(71),'_TOPHNDL'),var(73))),
	cf(or(B1,B2),eq(var(72),var(74))),
	cf(or(B4,B3),in_set(var(6),var(72))),
	cf(or(B1,B2),in_set(var(7),var(72))),
	cf(or(B4,B3),eq(attr(var(6),'ARG0'),var(75))),
	cf(or(B4,B3),eq(attr(var(6),'ARG1'),var(76))),
	cf(or(B4,B3),eq(attr(var(6),'LBL'),var(77))),
	cf(or(B4,B3),eq(attr(var(6),'relation'),var(78))),
	cf(or(B4,B3),eq(attr(var(6),'CARG'),'1')),
	cf(or(B1,B2),eq(var(75),var(76))),
	cf(CV_004,eq(attr(var(75),'type'),var(79))),
	cf(or(B1,B2),eq(attr(var(75),'NUM'),'sg')),
	cf(or(B1,B2),eq(attr(var(75),'PERS'),'3')),
	cf(or(B4,B3),eq(var(79),'event')),
	cf(or(B1,B2),eq(var(79),'ref-ind')),
	cf(A2,eq(attr(var(76),'DIV'),'-')),
	cf(A2,eq(attr(var(76),'GRIND'),'-')),
	cf(A2,eq(attr(var(76),'NATGEND'),'gender')),
	cf(1,eq(attr(var(76),'NUM'),'sg')),
	cf(1,eq(attr(var(76),'PERS'),'3')),
	cf(1,eq(attr(var(76),'type'),'ref-ind')),
	cf(CV_004,eq(var(77),var(80))),
	cf(CV_004,eq(attr(var(77),'type'),var(81))),
	cf(CV_008,eq(var(81),'handle')),
	cf(CV_004,eq(var(78),semform('card',9,[],[]))),
	cf(A2,eq(var(7),var(12))),
	cf(1,eq(attr(var(7),'ARG0'),var(76))),
	cf(or(B1,B2),eq(attr(var(7),'CARG'),var(82))),
	cf(or(B4,A1),eq(attr(var(7),'LBL'),var(73))),
	cf(or(B4,A1),eq(attr(var(7),'relation'),var(83))),
	cf(CV_008,eq(var(82),'1')),
	cf(or(B1,B2),eq(var(73),var(84))),
	cf(or(B4,B3,or(B4,B5,B6)),eq(attr(var(73),'type'),'handle')),
	cf(or(B1,B2),eq(var(83),semform('card',9,[],[]))),
	cf(or(B4,B3),eq(var(83),semform('generic-nom',16,[],[]))),
	cf(or(B5,B6),eq(var(83),semform('generic-nom',18,[],[]))),
	cf(A2,eq(var(53),var(27))),
	cf(or(CV_005,A1),eq(attr(var(53),'H-CONS'),var(85))),
	cf(or(CV_005,A1),eq(attr(var(53),'RELS'),var(74))),
	cf(1,eq(attr(var(53),'RELS_EL'),var(7))),
	cf(CV_002,eq(attr(var(53),'TOP'),var(86))),
	cf(or(CV_005,A1),eq(attr(var(53),'_ANTECEDVAR'),var(38))),
	cf(or(B1,B2),eq(attr(var(53),'_CARD'),var(7))),
	cf(or(CV_005,A1),eq(attr(var(53),'_QUANT'),var(87))),
	cf(or(B1,B2),eq(attr(var(53),'_TOPHNDL'),var(84))),
	cf(or(B1,B2),in_set(var(8),var(85))),
	cf(or(B4,B3),in_set(var(9),var(85))),
	cf(or(B5,B6),in_set(var(10),var(85))),
	cf(or(B1,B2),eq(attr(var(8),'OUTSCPD'),var(84))),
	cf(or(B1,B2),eq(attr(var(8),'SC_ARG'),var(88))),
	cf(or(B1,B2),eq(attr(var(8),'relation'),'qeq')),
	cf(or(B1,B2),eq(attr(var(84),'type'),'handle')),
	cf(or(B4,A1),eq(attr(var(88),'type'),var(89))),
	cf(CV_002,eq(var(89),'handle')),
	cf(or(B4,B3),eq(attr(var(9),'OUTSCPD'),var(73))),
	cf(or(B4,B3),eq(attr(var(9),'SC_ARG'),var(88))),
	cf(or(B4,B3),eq(attr(var(9),'relation'),'qeq')),
	cf(or(B5,B6),eq(attr(var(10),'OUTSCPD'),var(73))),
	cf(or(B5,B6),eq(attr(var(10),'SC_ARG'),var(88))),
	cf(or(B5,B6),eq(attr(var(10),'relation'),'qeq')),
	cf(A1,in_set(var(7),var(74))),
	cf(A1,in_set(var(11),var(74))),
	cf(or(B4,A1),eq(attr(var(11),'ARG0'),var(90))),
	cf(or(B4,A1),eq(attr(var(11),'BODY'),var(91))),
	cf(or(B4,A1),eq(attr(var(11),'LBL'),var(92))),
	cf(or(B4,A1),eq(attr(var(11),'RSTR'),var(88))),
	cf(or(B4,A1),eq(attr(var(11),'relation'),var(93))),
	cf(CV_002,eq(var(90),var(76))),
	cf(A1,eq(attr(var(90),'type'),var(94))),
	cf(or(or(B5,B6),or(B1,B2)),eq(attr(var(90),'NUM'),'sg')),
	cf(or(or(B5,B6),or(B1,B2)),eq(attr(var(90),'PERS'),'3')),
	cf(CV_001,eq(var(94),'ref-ind')),
	cf(or(B4,A1),eq(attr(var(91),'type'),var(95))),
	cf(CV_002,eq(var(95),'handle')),
	cf(CV_002,eq(var(92),var(86))),
	cf(or(or(B1,B2),A1),eq(attr(var(92),'type'),var(96))),
	cf(A1,eq(var(96),var(97))),
	cf(CV_005,eq(var(96),'handle')),
	cf(or(B1,B2),eq(var(93),semform('number_q',10,[],[]))),
	cf(or(B4,B3),eq(var(93),semform('udef_q',15,[],[]))),
	cf(or(B5,B6),eq(var(93),semform('en_q',12,[],[]))),
	cf(or(B4,A1),eq(attr(var(86),'type'),var(97))),
	cf(CV_002,eq(var(97),'handle')),
	cf(or(B4,A1),eq(attr(var(87),'RELS_EL'),var(11))),
	cf(or(B4,A1),eq(attr(var(87),'TOP'),var(98))),
	cf(or(B4,A1),eq(attr(var(98),'type'),var(99))),
	cf(CV_002,eq(var(99),'handle')),
	cf(1,eq(attr(var(18),'PRED'),var(100))),
	cf(1,eq(attr(var(18),'CASE'),var(101))),
	cf(1,eq(attr(var(18),'CHECK'),var(102))),
	cf(1,eq(attr(var(18),'GEND'),var(103))),
	cf(1,eq(attr(var(18),'NTYPE'),var(104))),
	cf(1,eq(proj(var(18),'m::'),var(27))),
	cf(1,eq(attr(var(18),'DEF'),'+')),
	cf(1,eq(attr(var(18),'NUM'),'sg')),
	cf(1,eq(attr(var(18),'PERS'),'3')),
	cf(CV_007,eq(var(100),semform('katte',0,[],[]))),
	cf(CV_006,eq(var(100),semform('katt',2,[],[]))),
	cf(A2,eq(var(101),'obl')),
	cf(A1,eq(var(101),'nom')),
	cf(1,eq(attr(var(102),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(102),'_NOUN'),'+')),
	cf(1,eq(attr(var(103),'FEM'),'-')),
	cf(1,eq(attr(var(103),'MASC'),'+')),
	cf(1,eq(attr(var(103),'NEUT'),'-')),
	cf(1,eq(attr(var(104),'NSEM'),var(105))),
	cf(1,eq(attr(var(104),'NSYN'),'common')),
	cf(1,eq(attr(var(105),'COMMON'),'count')),
	cf(A2,eq(var(106),var(107))),
	cf(A1,eq(var(106),var(108))),
	cf(1,eq(attr(var(27),'H-CONS'),var(109))),
	cf(1,eq(attr(var(27),'RELS'),var(110))),
	cf(1,eq(attr(var(27),'RELS_EL'),var(12))),
	cf(1,eq(attr(var(27),'TOP'),var(111))),
	cf(1,eq(attr(var(27),'_QUANT'),var(112))),
	cf(A1,eq(var(109),var(28))),
	cf(1,in_set(var(1),var(109))),
	cf(1,in_set(var(12),var(110))),
	cf(1,in_set(var(13),var(110))),
	cf(1,eq(attr(var(12),'ARG0'),var(113))),
	cf(1,eq(attr(var(12),'LBL'),var(32))),
	cf(1,eq(attr(var(12),'relation'),var(114))),
	cf(1,eq(attr(var(12),'_CAT'),'n')),
	cf(A2,eq(var(113),var(76))),
	cf(A1,eq(var(113),var(38))),
	cf(1,eq(attr(var(113),'DIV'),'-')),
	cf(1,eq(attr(var(113),'GRIND'),'-')),
	cf(1,eq(attr(var(113),'NATGEND'),'gender')),
	cf(1,eq(attr(var(113),'NUM'),'sg')),
	cf(1,eq(attr(var(113),'PERS'),'3')),
	cf(1,eq(attr(var(113),'type'),'ref-ind')),
	cf(CV_007,eq(var(114),semform('katte',1,[],[]))),
	cf(CV_006,eq(var(114),semform('katt',3,[],[]))),
	cf(1,eq(attr(var(13),'ARG0'),var(113))),
	cf(1,eq(attr(var(13),'BODY'),var(115))),
	cf(1,eq(attr(var(13),'LBL'),var(111))),
	cf(1,eq(attr(var(13),'RSTR'),var(33))),
	cf(1,eq(attr(var(13),'relation'),semform('def_q',5,[],[]))),
	cf(1,eq(attr(var(115),'type'),'handle')),
	cf(1,eq(attr(var(111),'type'),'handle')),
	cf(1,eq(attr(var(112),'RELS_EL'),var(13))),
	cf(1,eq(attr(var(112),'TOP'),var(116))),
	cf(1,eq(attr(var(116),'type'),'handle')),
	cf(1,eq(attr(var(19),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(20),'MOOD'),'indicative')),
	cf(1,eq(attr(var(20),'TENSE'),'past')),
	cf(1,eq(attr(var(21),'H-CONS'),var(28))),
	cf(1,eq(attr(var(21),'INDEX'),var(117))),
	cf(1,eq(attr(var(21),'RELS'),var(118))),
	cf(1,eq(attr(var(21),'RELS_EL'),var(14))),
	cf(1,eq(attr(var(21),'TOP'),var(35))),
	cf(1,eq(attr(var(21),'_MSG'),var(119))),
	cf(1,eq(attr(var(21),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(117),'PERF'),'-')),
	cf(1,eq(attr(var(117),'SF'),'prop')),
	cf(1,eq(attr(var(117),'TENSE'),'past')),
	cf(1,eq(attr(var(117),'type'),'event')),
	cf(1,in_set(var(14),var(118))),
	cf(A2,in_set(var(15),var(118))),
	cf(1,eq(attr(var(14),'ARG0'),var(117))),
	cf(1,eq(attr(var(14),'ARG1'),var(38))),
	cf(1,eq(attr(var(14),'ARG2'),var(76))),
	cf(1,eq(attr(var(14),'LBL'),var(34))),
	cf(1,eq(attr(var(14),'_CAT'),'v')),
	cf(1,eq(attr(var(14),'relation'),semform('jage',7,[],[]))),
	cf(A2,eq(attr(var(15),'ARG1'),var(117))),
	cf(A2,eq(attr(var(15),'ARG2'),var(76))),
	cf(A2,eq(attr(var(15),'LBL'),var(34))),
	cf(A2,eq(attr(var(15),'relation'),'topic_d')),
	cf(1,eq(attr(var(119),'ARG0'),var(117))),
	cf(1,eq(attr(var(119),'LBL'),var(35))),
	cf(CV_007,eq(var(120),var(121))),
	cf(CV_006,eq(var(120),var(122))),
	cf(CV_006,eq(proj(var(123),'m::'),var(122))),
	cf(1,eq(proj(var(124),'m::'),var(120))),
	cf(CV_007,eq(proj(var(125),'m::'),var(121))),
	cf(1,eq(proj(var(126),'m::'),var(127))),
	cf(1,eq(proj(var(128),'m::'),var(127))),
	cf(CV_005,eq(var(129),var(17))),
	cf(CV_005,eq(attr(var(129),'PRED'),var(55))),
	cf(CV_005,eq(attr(var(129),'GEND'),var(47))),
	cf(or(B5,B6),eq(attr(var(129),'PERS'),var(50))),
	cf(CV_005,eq(attr(var(129),'REF'),var(51))),
	cf(CV_005,eq(attr(var(129),'SPEC'),var(52))),
	cf(CV_005,eq(proj(var(129),'m::'),var(53))),
	cf(CV_005,eq(attr(var(129),'DEF'),'-')),
	cf(CV_005,eq(attr(var(129),'NUM'),'sg')),
	cf(or(B4,B3),eq(var(55),semform('pro',13,[],[]))),
	cf(or(B5,B6),eq(var(55),semform('pro',17,[],[]))),
	cf(or(B1,B2),eq(var(130),var(17))),
	cf(or(B4,B3),eq(var(130),var(70))),
	cf(CV_003,eq(attr(var(130),'PRED'),var(54))),
	cf(CV_003,eq(attr(var(130),'AGRGEND'),var(42))),
	cf(CV_004,eq(attr(var(130),'AGRNUM'),var(43))),
	cf(CV_003,eq(attr(var(130),'DIGVALUE'),var(46))),
	cf(CV_004,eq(attr(var(130),'HEADNUM'),var(48))),
	cf(or(B1,B2),eq(attr(var(130),'REF'),var(51))),
	cf(CV_003,eq(proj(var(130),'m::'),var(131))),
	cf(CV_008,eq(var(54),semform('en',8,[],[]))),
	cf(or(B1,B2),eq(var(131),var(53))),
	cf(or(B4,B3),eq(var(131),var(71))),
	cf(or(B1,B2),eq(attr(var(131),'H-CONS'),var(85))),
	cf(CV_003,eq(attr(var(131),'RELS'),var(72))),
	cf(or(B1,B2),eq(attr(var(131),'RELS_EL'),var(7))),
	cf(or(B1,B2),eq(attr(var(131),'TOP'),var(86))),
	cf(or(B1,B2),eq(attr(var(131),'_ANTECEDVAR'),var(38))),
	cf(CV_003,eq(attr(var(131),'_CARD'),var(132))),
	cf(or(B1,B2),eq(attr(var(131),'_QUANT'),var(87))),
	cf(CV_003,eq(attr(var(131),'_TOPHNDL'),var(80))),
	cf(or(B4,B3),eq(var(132),var(6))),
	cf(or(B1,B2),eq(var(132),var(7))),
	cf(CV_003,eq(attr(var(132),'ARG0'),var(75))),
	cf(CV_003,eq(attr(var(132),'CARG'),var(82))),
	cf(CV_003,eq(attr(var(132),'LBL'),var(77))),
	cf(CV_003,eq(attr(var(132),'relation'),var(78))),
	cf(or(B1,B2),eq(var(80),var(84))),
	cf(or(B4,B3),eq(var(80),var(73))),
	cf(CV_003,eq(attr(var(80),'type'),'handle'))
	],
	% C-Structure:
	[
	cf(1,subtree(895,'ROOT',1051,124)),
	cf(1,phi(895,var(0))),
	cf(1,subtree(1051,'ROOT',-,1445)),
	cf(1,phi(1051,var(0))),
	cf(1,subtree(1445,'IP',1522,1639)),
	cf(1,phi(1445,var(0))),
	cf(1,subtree(1522,'IP',-,1682)),
	cf(1,phi(1522,var(0))),
	cf(1,subtree(1682,'NP',-,2)),
	cf(1,phi(1682,var(18))),
	cf(1,subtree(2,'N',342,4)),
	cf(1,phi(2,var(18))),
	cf(1,cproj(2,var(124))),
	cf(1,subtree(342,'N',316,6)),
	cf(1,phi(342,var(18))),
	cf(1,cproj(342,var(124))),
	cf(CV_006,subtree(316,'N',312,21)),
	cf(CV_006,phi(316,var(18))),
	cf(CV_006,cproj(316,var(124))),
	cf(CV_006,subtree(312,'N',309,23)),
	cf(CV_006,phi(312,var(18))),
	cf(CV_006,cproj(312,var(124))),
	cf(CV_006,subtree(309,'N',-,25)),
	cf(CV_006,phi(309,var(18))),
	cf(CV_006,cproj(309,var(124))),
	cf(CV_006,subtree(25,'N_BASE',-,26)),
	cf(CV_006,phi(25,var(18))),
	cf(CV_006,cproj(25,var(123))),
	cf(CV_006,terminal(26,'katt',[3])),
	cf(CV_006,phi(26,var(18))),
	cf(CV_006,cproj(26,var(123))),
	cf(CV_006,subtree(23,'N_SUFF_BASE',-,24)),
	cf(CV_006,phi(23,var(18))),
	cf(CV_006,terminal(24,'+Noun',[3])),
	cf(CV_006,phi(24,var(18))),
	cf(CV_006,subtree(21,'N_SUFF_BASE',-,22)),
	cf(CV_006,phi(21,var(18))),
	cf(CV_006,terminal(22,'+Masc',[3])),
	cf(CV_006,phi(22,var(18))),
	cf(CV_007,subtree(316,'N',338,8)),
	cf(CV_007,phi(316,var(18))),
	cf(CV_007,cproj(316,var(124))),
	cf(CV_007,subtree(338,'N',331,10)),
	cf(CV_007,phi(338,var(18))),
	cf(CV_007,cproj(338,var(124))),
	cf(CV_007,subtree(331,'N',-,14)),
	cf(CV_007,phi(331,var(18))),
	cf(CV_007,cproj(331,var(124))),
	cf(CV_007,subtree(14,'N_BASE',-,13)),
	cf(CV_007,phi(14,var(18))),
	cf(CV_007,cproj(14,var(125))),
	cf(CV_007,terminal(13,'katte',[3])),
	cf(CV_007,phi(13,var(18))),
	cf(CV_007,cproj(13,var(125))),
	cf(CV_007,subtree(10,'N_SUFF_BASE',-,11)),
	cf(CV_007,phi(10,var(18))),
	cf(CV_007,terminal(11,'+Noun',[3])),
	cf(CV_007,phi(11,var(18))),
	cf(CV_007,subtree(8,'N_SUFF_BASE',-,9)),
	cf(CV_007,phi(8,var(18))),
	cf(CV_007,terminal(9,'+Masc',[3])),
	cf(CV_007,phi(9,var(18))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(18))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(133))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(18))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(18))),
	cf(1,subtree(1639,'I\'',1611,1616)),
	cf(1,phi(1639,var(0))),
	cf(1,subtree(1611,'I\'',-,410)),
	cf(1,phi(1611,var(0))),
	cf(1,subtree(410,'Vfin',409,35)),
	cf(1,phi(410,var(0))),
	cf(1,cproj(410,var(126))),
	cf(1,subtree(409,'Vfin',408,37)),
	cf(1,phi(409,var(0))),
	cf(1,cproj(409,var(126))),
	cf(1,subtree(408,'Vfin',-,39)),
	cf(1,phi(408,var(0))),
	cf(1,cproj(408,var(126))),
	cf(1,subtree(39,'V_BASE',-,40)),
	cf(1,phi(39,var(0))),
	cf(1,cproj(39,var(128))),
	cf(1,terminal(40,'jage',[33])),
	cf(1,phi(40,var(0))),
	cf(1,cproj(40,var(128))),
	cf(1,subtree(37,'V_SUFF_BASE',-,38)),
	cf(1,phi(37,var(0))),
	cf(1,terminal(38,'+Verb',[33])),
	cf(1,phi(38,var(0))),
	cf(1,subtree(35,'V_SUFF_BASE',-,36)),
	cf(1,phi(35,var(0))),
	cf(1,terminal(36,'+Past',[33])),
	cf(1,phi(36,var(0))),
	cf(A2,subtree(1616,'S',-,115)),
	cf(A2,phi(1616,var(0))),
	cf(A2,subtree(115,'PRON',-,74)),
	cf(A2,phi(115,var(16))),
	cf(A2,terminal(74,'en',[74])),
	cf(A2,phi(74,var(134))),
	cf(A1,subtree(1616,'S',-,1623)),
	cf(A1,phi(1616,var(0))),
	cf(CV_005,subtree(1623,'VPmain',-,1192)),
	cf(CV_005,phi(1623,var(0))),
	cf(or(B5,B6),subtree(1192,'QuantP',-,116)),
	cf(or(B5,B6),phi(1192,var(129))),
	cf(or(B5,B6),subtree(116,'ART',-,74)),
	cf(or(B5,B6),phi(116,var(129))),
	cf(or(B5,B6),terminal(74,'en',[74])),
	cf(or(B5,B6),phi(74,var(135))),
	cf(or(B4,B3),subtree(1192,'QuantP',-,991)),
	cf(or(B4,B3),phi(1192,var(129))),
	cf(or(B4,B3),subtree(991,'NUMP',-,985)),
	cf(or(B4,B3),phi(991,var(70))),
	cf(or(B1,B2),subtree(1623,'VPmain',-,1070)),
	cf(or(B1,B2),phi(1623,var(0))),
	cf(or(B1,B2),subtree(1070,'NUMBP',-,985)),
	cf(or(B1,B2),phi(1070,var(17))),
	cf(CV_008,subtree(985,'NUM1P',-,114)),
	cf(CV_008,phi(985,var(130))),
	cf(CV_008,subtree(114,'NUM1',-,74)),
	cf(CV_008,phi(114,var(130))),
	cf(CV_008,terminal(74,'en',[74])),
	cf(CV_008,phi(74,var(130))),
	cf(1,subtree(124,'PERIOD',-,117)),
	cf(1,phi(124,var(0))),
	cf(1,terminal(117,'.',[117])),
	cf(1,phi(117,var(0))),
	cf(CV_007,semform_data(0,14,1,6)),
	cf(CV_007,semform_data(1,14,1,6)),
	cf(CV_006,semform_data(2,25,1,4)),
	cf(CV_006,semform_data(3,25,1,4)),
	cf(1,semform_data(5,6,7,7)),
	cf(1,semform_data(6,39,8,13)),
	cf(1,semform_data(7,39,8,13)),
	cf(CV_008,semform_data(8,114,14,17)),
	cf(or(B5,B6),semform_data(8,116,14,17)),
	cf(A2,semform_data(8,115,14,17)),
	cf(CV_008,semform_data(9,114,14,17)),
	cf(or(B1,B2),semform_data(10,1070,14,17)),
	cf(or(B5,B6),semform_data(12,116,14,17)),
	cf(or(B4,B3),semform_data(13,1192,14,17)),
	cf(or(B4,B3),semform_data(15,1192,14,17)),
	cf(or(B4,B3),semform_data(16,1192,14,17)),
	cf(or(B5,B6),semform_data(17,1192,14,17)),
	cf(or(B5,B6),semform_data(18,1192,14,17)),
	cf(A2,semform_data(24,115,14,17)),
	cf(A2,semform_data(25,115,14,17)),
	cf(1,fspan(var(0),1,17)),
	cf(1,fspan(var(18),1,7)),
	cf(A2,fspan(var(16),14,17)),
	cf(CV_005,fspan(var(129),14,17)),
	cf(CV_008,fspan(var(130),14,17)),
	cf(1,surfaceform(3,'katten',1,7)),
	cf(1,surfaceform(33,'jaget',8,13)),
	cf(1,surfaceform(74,'en',14,17)),
	cf(1,surfaceform(117,'.',16,17))
	]).

