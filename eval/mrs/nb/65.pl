% -*- coding: iso-8859-1 -*-

fstructure('Abrams drev på og bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('4 solutions, 0.25 CPU seconds, 346 subtrees unified'),
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
	select(A2, 1),
	
	],
	% Constraints:
	[
	cf(A1,in_set(var(1),var(0))),
	cf(A1,in_set(var(2),var(0))),
	cf(A2,eq(attr(var(0),'PRED'),semform('bjeffe',15,[var(27)],[]))),
	cf(A2,eq(attr(var(0),'SUBJ'),var(28))),
	cf(A2,eq(attr(var(0),'TOPIC'),var(29))),
	cf(1,eq(attr(var(0),'CHECK'),var(30))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(31))),
	cf(1,eq(proj(var(0),'m::'),var(32))),
	cf(A1,eq(attr(var(0),'COORD-FORM'),'og')),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(A2,eq(attr(var(0),'VFORM'),'fin')),
	cf(A2,eq(attr(var(0),'VTYPE'),'main')),
	cf(A2,eq(var(28),var(27))),
	cf(A1,eq(attr(var(28),'PRED'),var(33))),
	cf(A1,eq(attr(var(28),'CHECK'),var(34))),
	cf(B1,eq(attr(var(28),'NTYPE'),var(35))),
	cf(A1,eq(attr(var(28),'NUM'),var(17))),
	cf(B1,eq(attr(var(28),'REF'),var(36))),
	cf(B2,eq(attr(var(28),'SPEC'),var(37))),
	cf(A1,eq(proj(var(28),'m::'),var(38))),
	cf(1,eq(attr(var(28),'CASE'),'nom')),
	cf(A1,eq(attr(var(28),'DEF'),'+')),
	cf(A1,eq(attr(var(28),'PERS'),'3')),
	cf(B1,eq(var(33),semform('Abrams',0,[],[]))),
	cf(B2,eq(var(33),semform('pro',6,[],[]))),
	cf(B2,eq(attr(var(34),'_PREDET'),var(39))),
	cf(B1,eq(attr(var(34),'_SEL'),var(40))),
	cf(B2,eq(var(39),'+')),
	cf(B1,eq(attr(var(40),'_ABSTRACT'),'-')),
	cf(B1,eq(attr(var(40),'_ANIM'),'+')),
	cf(B1,eq(attr(var(40),'_HUMAN'),'+')),
	cf(B1,eq(attr(var(35),'NSEM'),var(41))),
	cf(B1,eq(attr(var(35),'NSYN'),'proper')),
	cf(B1,eq(attr(var(41),'PROPER'),var(42))),
	cf(B1,eq(attr(var(42),'PROPER-TYPE'),'name')),
	cf(B1,eq(var(17),'sg')),
	cf(B1,eq(var(36),'+')),
	cf(B2,eq(attr(var(37),'POSS'),var(43))),
	cf(B2,eq(attr(var(43),'PRED'),semform('Abram',3,[],[]))),
	cf(B2,eq(attr(var(43),'CHECK'),var(44))),
	cf(B2,eq(attr(var(43),'NTYPE'),var(45))),
	cf(B2,eq(proj(var(43),'m::'),var(46))),
	cf(B2,eq(attr(var(43),'CASE'),'gen')),
	cf(B2,eq(attr(var(43),'DEF'),'+')),
	cf(B2,eq(attr(var(43),'NUM'),'sg')),
	cf(B2,eq(attr(var(43),'PERS'),'3')),
	cf(B2,eq(attr(var(43),'REF'),'+')),
	cf(B2,eq(attr(var(44),'_DEF-MORPH'),'-')),
	cf(B2,eq(attr(var(45),'NSEM'),var(47))),
	cf(B2,eq(attr(var(45),'NSYN'),'proper')),
	cf(B2,eq(attr(var(47),'PROPER'),var(48))),
	cf(B2,eq(attr(var(48),'PROPER-TYPE'),'name')),
	cf(B2,eq(attr(var(46),'H-CONS'),var(14))),
	cf(B2,eq(attr(var(46),'RELS'),var(24))),
	cf(B2,eq(attr(var(46),'RELS_EL'),var(7))),
	cf(B2,eq(attr(var(46),'TOP'),var(49))),
	cf(B2,eq(attr(var(46),'_QUANT'),var(50))),
	cf(B2,in_set(var(3),var(14))),
	cf(B2,in_set(var(4),var(14))),
	cf(B1,in_set(var(5),var(14))),
	cf(B2,eq(attr(var(3),'OUTSCPD'),var(51))),
	cf(B2,eq(attr(var(3),'SC_ARG'),var(52))),
	cf(B2,eq(attr(var(3),'relation'),'qeq')),
	cf(B2,eq(attr(var(51),'type'),'handle')),
	cf(B2,eq(attr(var(52),'type'),'handle')),
	cf(B2,eq(attr(var(4),'OUTSCPD'),var(53))),
	cf(B2,eq(attr(var(4),'SC_ARG'),var(54))),
	cf(B2,eq(attr(var(4),'relation'),'qeq')),
	cf(A1,eq(attr(var(53),'type'),'handle')),
	cf(A1,eq(attr(var(54),'type'),'handle')),
	cf(B1,eq(attr(var(5),'OUTSCPD'),var(53))),
	cf(B1,eq(attr(var(5),'SC_ARG'),var(54))),
	cf(B1,eq(attr(var(5),'relation'),'qeq')),
	cf(B2,in_set(var(6),var(24))),
	cf(B2,in_set(var(7),var(24))),
	cf(A1,in_set(var(8),var(24))),
	cf(A1,in_set(var(9),var(24))),
	cf(B2,in_set(var(10),var(24))),
	cf(B1,eq(var(6),var(8))),
	cf(B1,eq(attr(var(6),'ARG0'),var(55))),
	cf(B2,eq(attr(var(6),'ARG1'),var(56))),
	cf(B2,eq(attr(var(6),'ARG2'),var(55))),
	cf(B1,eq(attr(var(6),'BODY'),var(57))),
	cf(A1,eq(attr(var(6),'LBL'),var(58))),
	cf(B1,eq(attr(var(6),'RSTR'),var(54))),
	cf(A1,eq(attr(var(6),'relation'),var(59))),
	cf(A1,eq(attr(var(55),'NUM'),var(16))),
	cf(B1,eq(attr(var(55),'NATGEND'),'animate')),
	cf(A1,eq(attr(var(55),'PERS'),'3')),
	cf(A1,eq(attr(var(55),'type'),'ref-ind')),
	cf(B1,eq(var(16),'sg')),
	cf(B2,eq(var(16),var(17))),
	cf(B2,eq(attr(var(56),'NUM'),'sg')),
	cf(B2,eq(attr(var(56),'PERS'),'3')),
	cf(B2,eq(attr(var(56),'type'),'ref-ind')),
	cf(A1,eq(attr(var(57),'type'),'handle')),
	cf(B1,eq(var(58),var(60))),
	cf(B2,eq(var(58),var(53))),
	cf(A1,eq(attr(var(58),'type'),'handle')),
	cf(B1,eq(var(59),semform('proper_q',2,[],[]))),
	cf(B2,eq(var(59),semform('poss',8,[],[]))),
	cf(B2,eq(attr(var(7),'ARG0'),var(56))),
	cf(B2,eq(attr(var(7),'LBL'),var(51))),
	cf(B2,eq(attr(var(7),'CARG'),'Abram')),
	cf(B2,eq(attr(var(7),'relation'),semform('named',4,[],[]))),
	cf(A1,eq(attr(var(8),'ARG0'),var(55))),
	cf(A1,eq(attr(var(8),'BODY'),var(57))),
	cf(A1,eq(attr(var(8),'LBL'),var(61))),
	cf(A1,eq(attr(var(8),'RSTR'),var(54))),
	cf(A1,eq(attr(var(8),'relation'),var(62))),
	cf(A1,eq(attr(var(61),'type'),'handle')),
	cf(B2,eq(var(62),semform('def_explicit_q',9,[],[]))),
	cf(B1,eq(var(62),semform('proper_q',2,[],[]))),
	cf(A1,eq(attr(var(9),'ARG0'),var(55))),
	cf(A1,eq(attr(var(9),'LBL'),var(53))),
	cf(A1,eq(attr(var(9),'relation'),var(63))),
	cf(B1,eq(attr(var(9),'CARG'),'Abrams')),
	cf(B2,eq(var(63),semform('generic-nom',7,[],[]))),
	cf(B1,eq(var(63),semform('named',1,[],[]))),
	cf(B2,eq(attr(var(10),'ARG0'),var(56))),
	cf(B2,eq(attr(var(10),'BODY'),var(64))),
	cf(B2,eq(attr(var(10),'LBL'),var(49))),
	cf(B2,eq(attr(var(10),'RSTR'),var(52))),
	cf(B2,eq(attr(var(10),'relation'),semform('proper_q',5,[],[]))),
	cf(B2,eq(attr(var(64),'type'),'handle')),
	cf(B2,eq(attr(var(49),'type'),'handle')),
	cf(B2,eq(attr(var(50),'RELS_EL'),var(10))),
	cf(B2,eq(attr(var(50),'TOP'),var(65))),
	cf(B2,eq(attr(var(65),'type'),'handle')),
	cf(A1,eq(attr(var(38),'H-CONS'),var(14))),
	cf(A1,eq(attr(var(38),'RELS'),var(24))),
	cf(A1,eq(attr(var(38),'RELS_EL'),var(9))),
	cf(A1,eq(attr(var(38),'TOP'),var(61))),
	cf(A1,eq(attr(var(38),'_QUANT'),var(66))),
	cf(A1,eq(attr(var(66),'RELS_EL'),var(8))),
	cf(A1,eq(attr(var(66),'TOP'),var(67))),
	cf(A1,eq(attr(var(67),'type'),'handle')),
	cf(A2,eq(var(29),var(27))),
	cf(A1,subsume(var(29),var(28))),
	cf(1,eq(attr(var(29),'PRED'),var(68))),
	cf(1,eq(attr(var(29),'CHECK'),var(69))),
	cf(or(C1,B1),eq(attr(var(29),'NTYPE'),var(70))),
	cf(1,eq(attr(var(29),'NUM'),var(71))),
	cf(or(C1,B1),eq(attr(var(29),'REF'),var(72))),
	cf(or(C2,B2),eq(attr(var(29),'SPEC'),var(73))),
	cf(1,eq(proj(var(29),'m::'),var(74))),
	cf(1,eq(attr(var(29),'DEF'),'+')),
	cf(1,eq(attr(var(29),'PERS'),'3')),
	cf(or(C1,B1),eq(var(68),semform('Abrams',0,[],[]))),
	cf(or(C2,B2),eq(var(68),semform('pro',6,[],[]))),
	cf(A1,subsume(var(68),var(33))),
	cf(A2,eq(var(75),'nom')),
	cf(A1,subsume(var(75),'nom')),
	cf(A1,subsume(var(69),var(34))),
	cf(or(C2,B2),eq(attr(var(69),'_PREDET'),var(76))),
	cf(or(C1,B1),eq(attr(var(69),'_SEL'),var(77))),
	cf(A1,subsume(var(78),var(79))),
	cf(B1,subsume(var(80),var(81))),
	cf(A1,subsume(var(82),var(83))),
	cf(A1,subsume(var(84),var(85))),
	cf(A1,subsume(var(86),var(87))),
	cf(or(C2,B2),eq(var(76),'+')),
	cf(A1,subsume(var(76),var(39))),
	cf(A1,subsume(var(88),var(89))),
	cf(B1,subsume(var(77),var(40))),
	cf(or(C1,B1),eq(attr(var(77),'_ABSTRACT'),'-')),
	cf(or(C1,B1),eq(attr(var(77),'_ANIM'),'+')),
	cf(or(C1,B1),eq(attr(var(77),'_HUMAN'),'+')),
	cf(A1,subsume(var(90),var(91))),
	cf(A1,subsume(var(92),var(93))),
	cf(A1,subsume(var(94),var(95))),
	cf(A1,subsume(var(70),var(35))),
	cf(or(C1,B1),eq(attr(var(70),'NSEM'),var(96))),
	cf(or(C1,B1),eq(attr(var(70),'NSYN'),'proper')),
	cf(A1,subsume(var(96),var(41))),
	cf(or(C1,B1),eq(attr(var(96),'PROPER'),var(97))),
	cf(B2,subsume(var(98),var(99))),
	cf(B1,subsume(var(97),var(42))),
	cf(or(C1,B1),eq(attr(var(97),'PROPER-TYPE'),'name')),
	cf(or(C1,B1),eq(var(71),'sg')),
	cf(A1,subsume(var(71),var(17))),
	cf(A2,eq(var(100),var(101))),
	cf(A1,subsume(var(100),var(102))),
	cf(or(C1,B1),eq(var(72),'+')),
	cf(A1,subsume(var(72),var(36))),
	cf(B2,subsume(var(73),var(37))),
	cf(or(C2,B2),eq(attr(var(73),'POSS'),var(103))),
	cf(B2,subsume(var(104),var(105))),
	cf(B2,subsume(var(103),var(43))),
	cf(or(C2,B2),eq(attr(var(103),'PRED'),semform('Abram',3,[],[]))),
	cf(or(C2,B2),eq(attr(var(103),'CHECK'),var(106))),
	cf(or(C2,B2),eq(attr(var(103),'NTYPE'),var(107))),
	cf(or(C2,B2),eq(proj(var(103),'m::'),var(108))),
	cf(or(C2,B2),eq(attr(var(103),'CASE'),'gen')),
	cf(or(C2,B2),eq(attr(var(103),'DEF'),'+')),
	cf(or(C2,B2),eq(attr(var(103),'NUM'),'sg')),
	cf(or(C2,B2),eq(attr(var(103),'PERS'),'3')),
	cf(or(C2,B2),eq(attr(var(103),'REF'),'+')),
	cf(B2,subsume(var(106),var(44))),
	cf(or(C2,B2),eq(attr(var(106),'_DEF-MORPH'),'-')),
	cf(B2,subsume(var(109),var(110))),
	cf(B2,subsume(var(111),var(112))),
	cf(B2,subsume(var(113),var(114))),
	cf(B2,subsume(var(115),var(116))),
	cf(B2,subsume(var(107),var(45))),
	cf(or(C2,B2),eq(attr(var(107),'NSEM'),var(117))),
	cf(or(C2,B2),eq(attr(var(107),'NSYN'),'proper')),
	cf(B2,subsume(var(117),var(47))),
	cf(or(C2,B2),eq(attr(var(117),'PROPER'),var(118))),
	cf(B2,subsume(var(118),var(48))),
	cf(or(C2,B2),eq(attr(var(118),'PROPER-TYPE'),'name')),
	cf(B2,subsume(var(108),var(46))),
	cf(or(C2,B2),eq(attr(var(108),'H-CONS'),var(119))),
	cf(or(C2,B2),eq(attr(var(108),'RELS'),var(120))),
	cf(or(C2,B2),eq(attr(var(108),'RELS_EL'),var(20))),
	cf(or(C2,B2),eq(attr(var(108),'TOP'),var(121))),
	cf(or(C2,B2),eq(attr(var(108),'_QUANT'),var(122))),
	cf(A2,eq(var(119),var(123))),
	cf(B1,in_set(var(11),var(119))),
	cf(B2,in_set(var(12),var(119))),
	cf(or(C2,B2),in_set(var(13),var(119))),
	cf(A1,subsume(var(119),var(14))),
	cf(B2,subsume(var(119),var(15))),
	cf(or(C2,B2),eq(var(11),var(124))),
	cf(B1,subsume(var(11),var(5))),
	cf(or(C1,B1),eq(attr(var(11),'OUTSCPD'),var(125))),
	cf(or(C2,B2),eq(attr(var(11),'RELS_EL'),var(22))),
	cf(or(C1,B1),eq(attr(var(11),'SC_ARG'),var(126))),
	cf(or(C2,B2),eq(attr(var(11),'TOP'),var(127))),
	cf(or(C1,B1),eq(attr(var(11),'relation'),'qeq')),
	cf(A1,subsume(var(125),var(53))),
	cf(1,eq(attr(var(125),'type'),'handle')),
	cf(A1,subsume(var(22),var(8))),
	cf(1,eq(attr(var(22),'ARG0'),var(128))),
	cf(1,eq(attr(var(22),'BODY'),var(129))),
	cf(1,eq(attr(var(22),'LBL'),var(130))),
	cf(1,eq(attr(var(22),'RSTR'),var(126))),
	cf(1,eq(attr(var(22),'relation'),var(131))),
	cf(A2,eq(var(128),var(132))),
	cf(A1,subsume(var(128),var(55))),
	cf(1,eq(attr(var(128),'NUM'),var(133))),
	cf(or(C1,B1),eq(attr(var(128),'NATGEND'),'animate')),
	cf(1,eq(attr(var(128),'PERS'),'3')),
	cf(1,eq(attr(var(128),'type'),'ref-ind')),
	cf(or(C1,B1),eq(var(133),'sg')),
	cf(or(C2,B2),eq(var(133),var(71))),
	cf(A1,subsume(var(133),var(16))),
	cf(B2,subsume(var(133),var(17))),
	cf(B1,subsume(var(133),'sg')),
	cf(A1,subsume(var(129),var(57))),
	cf(1,eq(attr(var(129),'type'),'handle')),
	cf(A1,subsume(var(130),var(61))),
	cf(1,eq(attr(var(130),'type'),'handle')),
	cf(A1,subsume(var(126),var(54))),
	cf(1,eq(attr(var(126),'type'),'handle')),
	cf(or(C1,B1),eq(var(131),semform('proper_q',2,[],[]))),
	cf(or(C2,B2),eq(var(131),semform('def_explicit_q',9,[],[]))),
	cf(B2,subsume(var(131),semform('def_explicit_q',9,[],[]))),
	cf(A1,subsume(var(131),var(62))),
	cf(A1,subsume(var(127),var(67))),
	cf(1,eq(attr(var(127),'type'),'handle')),
	cf(B2,subsume(var(12),var(4))),
	cf(or(C2,B2),eq(attr(var(12),'OUTSCPD'),var(125))),
	cf(or(C2,B2),eq(attr(var(12),'SC_ARG'),var(126))),
	cf(or(C2,B2),eq(attr(var(12),'relation'),'qeq')),
	cf(B2,subsume(var(13),var(3))),
	cf(or(C2,B2),eq(attr(var(13),'OUTSCPD'),var(134))),
	cf(or(C2,B2),eq(attr(var(13),'SC_ARG'),var(135))),
	cf(or(C2,B2),eq(attr(var(13),'relation'),'qeq')),
	cf(B2,subsume(var(134),var(51))),
	cf(or(C2,B2),eq(attr(var(134),'type'),'handle')),
	cf(B2,subsume(var(135),var(52))),
	cf(or(C2,B2),eq(attr(var(135),'type'),'handle')),
	cf(A1,in_set(var(18),var(120))),
	cf(or(C2,B2),in_set(var(19),var(120))),
	cf(or(C2,B2),in_set(var(20),var(120))),
	cf(or(C2,B2),in_set(var(21),var(120))),
	cf(1,in_set(var(22),var(120))),
	cf(A2,in_set(var(23),var(120))),
	cf(A1,subsume(var(120),var(24))),
	cf(B2,subsume(var(120),var(25))),
	cf(A2,eq(var(18),var(23))),
	cf(A1,subsume(var(18),var(9))),
	cf(1,eq(attr(var(18),'ARG0'),var(128))),
	cf(1,eq(attr(var(18),'LBL'),var(125))),
	cf(1,eq(attr(var(18),'relation'),var(136))),
	cf(or(C1,B1),eq(attr(var(18),'CARG'),'Abrams')),
	cf(or(C1,B1),eq(var(136),semform('named',1,[],[]))),
	cf(or(C2,B2),eq(var(136),semform('generic-nom',7,[],[]))),
	cf(B2,subsume(var(136),semform('generic-nom',7,[],[]))),
	cf(A1,subsume(var(136),var(63))),
	cf(or(C1,B1),eq(var(19),var(22))),
	cf(A1,subsume(var(19),var(6))),
	cf(or(C1,B1),eq(attr(var(19),'ARG0'),var(128))),
	cf(or(C2,B2),eq(attr(var(19),'ARG1'),var(137))),
	cf(or(C2,B2),eq(attr(var(19),'ARG2'),var(128))),
	cf(or(C1,B1),eq(attr(var(19),'BODY'),var(129))),
	cf(1,eq(attr(var(19),'LBL'),var(138))),
	cf(or(C1,B1),eq(attr(var(19),'RSTR'),var(126))),
	cf(1,eq(attr(var(19),'relation'),var(139))),
	cf(B2,subsume(var(137),var(56))),
	cf(or(C2,B2),eq(attr(var(137),'NUM'),'sg')),
	cf(or(C2,B2),eq(attr(var(137),'PERS'),'3')),
	cf(or(C2,B2),eq(attr(var(137),'type'),'ref-ind')),
	cf(or(C1,B1),eq(var(138),var(130))),
	cf(or(C2,B2),eq(var(138),var(125))),
	cf(A1,subsume(var(138),var(58))),
	cf(1,eq(attr(var(138),'type'),'handle')),
	cf(or(C1,B1),eq(var(139),semform('proper_q',2,[],[]))),
	cf(or(C2,B2),eq(var(139),semform('poss',8,[],[]))),
	cf(A1,subsume(var(139),var(59))),
	cf(B2,subsume(var(20),var(7))),
	cf(or(C2,B2),eq(attr(var(20),'ARG0'),var(137))),
	cf(or(C2,B2),eq(attr(var(20),'LBL'),var(134))),
	cf(or(C2,B2),eq(attr(var(20),'CARG'),'Abram')),
	cf(or(C2,B2),eq(attr(var(20),'relation'),semform('named',4,[],[]))),
	cf(B2,subsume(var(21),var(10))),
	cf(or(C2,B2),eq(attr(var(21),'ARG0'),var(137))),
	cf(or(C2,B2),eq(attr(var(21),'BODY'),var(140))),
	cf(or(C2,B2),eq(attr(var(21),'LBL'),var(121))),
	cf(or(C2,B2),eq(attr(var(21),'RSTR'),var(135))),
	cf(or(C2,B2),eq(attr(var(21),'relation'),semform('proper_q',5,[],[]))),
	cf(B2,subsume(var(140),var(64))),
	cf(or(C2,B2),eq(attr(var(140),'type'),'handle')),
	cf(B2,subsume(var(121),var(49))),
	cf(or(C2,B2),eq(attr(var(121),'type'),'handle')),
	cf(A1,eq(var(23),var(9))),
	cf(1,eq(attr(var(23),'ARG0'),var(141))),
	cf(1,eq(attr(var(23),'LBL'),var(142))),
	cf(1,eq(attr(var(23),'relation'),var(143))),
	cf(or(C1,B1),eq(attr(var(23),'CARG'),'Abrams')),
	cf(A2,eq(var(141),var(132))),
	cf(A1,eq(var(141),var(55))),
	cf(1,eq(attr(var(141),'NUM'),var(144))),
	cf(or(C1,B1),eq(attr(var(141),'NATGEND'),'animate')),
	cf(1,eq(attr(var(141),'PERS'),'3')),
	cf(1,eq(attr(var(141),'type'),'ref-ind')),
	cf(B1,eq(var(144),'sg')),
	cf(A1,eq(var(144),var(16))),
	cf(A2,eq(var(144),var(133))),
	cf(A1,eq(var(142),var(53))),
	cf(A2,eq(var(142),var(125))),
	cf(A1,eq(attr(var(142),'type'),'handle')),
	cf(A1,eq(var(143),var(63))),
	cf(B2,eq(var(143),semform('generic-nom',7,[],[]))),
	cf(B1,eq(var(143),semform('named',1,[],[]))),
	cf(A2,eq(var(143),var(136))),
	cf(B2,subsume(var(122),var(50))),
	cf(or(C2,B2),eq(attr(var(122),'RELS_EL'),var(21))),
	cf(or(C2,B2),eq(attr(var(122),'TOP'),var(145))),
	cf(B2,subsume(var(145),var(65))),
	cf(or(C2,B2),eq(attr(var(145),'type'),'handle')),
	cf(A2,eq(var(74),var(146))),
	cf(A1,subsume(var(74),var(38))),
	cf(1,eq(attr(var(74),'H-CONS'),var(119))),
	cf(1,eq(attr(var(74),'RELS'),var(120))),
	cf(1,eq(attr(var(74),'RELS_EL'),var(18))),
	cf(1,eq(attr(var(74),'TOP'),var(130))),
	cf(1,eq(attr(var(74),'_QUANT'),var(124))),
	cf(A1,subsume(var(124),var(66))),
	cf(1,eq(attr(var(124),'RELS_EL'),var(22))),
	cf(1,eq(attr(var(124),'TOP'),var(127))),
	cf(A2,eq(var(30),var(147))),
	cf(A1,eq(attr(var(30),'_MAIN-CL'),'+')),
	cf(A2,eq(var(31),var(148))),
	cf(A1,eq(attr(var(31),'MOOD'),'indicative')),
	cf(A1,eq(attr(var(31),'TENSE'),'past')),
	cf(A2,eq(var(32),var(149))),
	cf(A1,eq(attr(var(32),'H-CONS'),var(150))),
	cf(A1,eq(attr(var(32),'INDEX'),var(151))),
	cf(A1,eq(attr(var(32),'RELS'),var(152))),
	cf(A1,eq(attr(var(32),'RELS_EL'),var(153))),
	cf(A1,eq(attr(var(32),'TOP'),var(154))),
	cf(A1,eq(attr(var(32),'_MSG'),var(155))),
	cf(A1,eq(attr(var(32),'_MSGQEQ'),var(156))),
	cf(A1,in_set(var(156),var(150))),
	cf(A1,eq(attr(var(156),'OUTSCPD'),var(157))),
	cf(A1,eq(attr(var(156),'SC_ARG'),var(154))),
	cf(A1,eq(attr(var(156),'relation'),'qeq')),
	cf(A1,eq(attr(var(157),'type'),'handle')),
	cf(A1,eq(attr(var(154),'type'),'handle')),
	cf(A1,eq(attr(var(151),'PERF'),'-')),
	cf(A1,eq(attr(var(151),'SF'),'prop')),
	cf(A1,eq(attr(var(151),'TENSE'),'past')),
	cf(A1,eq(attr(var(151),'type'),'event')),
	cf(A1,in_set(var(153),var(152))),
	cf(A1,eq(attr(var(153),'ARG0'),var(151))),
	cf(A1,eq(attr(var(153),'L-HNDL'),var(158))),
	cf(A1,eq(attr(var(153),'L-INDEX'),var(159))),
	cf(A1,eq(attr(var(153),'LBL'),var(157))),
	cf(A1,eq(attr(var(153),'R-HNDL'),var(160))),
	cf(A1,eq(attr(var(153),'R-INDEX'),var(161))),
	cf(A1,eq(attr(var(153),'relation'),semform('and',14,[],[]))),
	cf(A1,eq(attr(var(158),'type'),'handle')),
	cf(A1,eq(attr(var(159),'PERF'),'-')),
	cf(A1,eq(attr(var(159),'TENSE'),'past')),
	cf(A1,eq(attr(var(159),'type'),'event')),
	cf(A1,eq(attr(var(160),'type'),'handle')),
	cf(A1,eq(attr(var(161),'PERF'),'-')),
	cf(A1,eq(attr(var(161),'TENSE'),'past')),
	cf(A1,eq(attr(var(161),'type'),'event')),
	cf(A1,eq(attr(var(155),'ARG0'),var(151))),
	cf(A1,eq(attr(var(155),'LBL'),var(154))),
	cf(A2,eq(var(1),var(0))),
	cf(1,eq(attr(var(1),'PRED'),semform('bjeffe',15,[var(162)],[]))),
	cf(1,eq(attr(var(1),'SUBJ'),var(162))),
	cf(1,eq(attr(var(1),'TOPIC'),var(27))),
	cf(A2,eq(attr(var(1),'CHECK'),var(147))),
	cf(1,eq(attr(var(1),'TNS-ASP'),var(148))),
	cf(1,eq(proj(var(1),'m::'),var(149))),
	cf(A2,eq(attr(var(1),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(1),'VFORM'),'fin')),
	cf(1,eq(attr(var(1),'VTYPE'),'main')),
	cf(A2,eq(var(162),var(27))),
	cf(A1,eq(var(162),var(28))),
	cf(1,eq(proj(var(162),'m::'),var(163))),
	cf(A2,eq(attr(var(162),'CASE'),'nom')),
	cf(A1,eq(var(164),var(165))),
	cf(A1,eq(var(163),var(38))),
	cf(A2,eq(var(163),var(146))),
	cf(1,eq(attr(var(163),'H-CONS'),var(123))),
	cf(1,eq(attr(var(163),'RELS_EL'),var(166))),
	cf(A1,eq(var(123),var(14))),
	cf(C1,in_set(var(11),var(123))),
	cf(C2,in_set(var(12),var(123))),
	cf(C2,in_set(var(13),var(123))),
	cf(A2,in_set(var(26),var(123))),
	cf(A1,eq(var(26),var(156))),
	cf(A2,eq(attr(var(26),'OUTSCPD'),var(167))),
	cf(A2,eq(attr(var(26),'SC_ARG'),var(168))),
	cf(A2,eq(attr(var(26),'relation'),'qeq')),
	cf(A1,eq(var(167),var(160))),
	cf(1,eq(attr(var(167),'type'),'handle')),
	cf(A1,eq(var(168),var(154))),
	cf(A2,eq(attr(var(168),'type'),'handle')),
	cf(A1,eq(var(166),var(9))),
	cf(A2,eq(var(166),var(23))),
	cf(1,eq(attr(var(166),'ARG0'),var(132))),
	cf(A1,eq(var(132),var(55))),
	cf(A2,eq(attr(var(132),'NUM'),var(133))),
	cf(C1,eq(attr(var(132),'NATGEND'),'animate')),
	cf(A2,eq(attr(var(132),'PERS'),'3')),
	cf(A2,eq(attr(var(132),'type'),'ref-ind')),
	cf(A1,eq(var(27),var(28))),
	cf(1,eq(attr(var(27),'PRED'),var(169))),
	cf(1,eq(attr(var(27),'CHECK'),var(170))),
	cf(or(C1,B1),eq(attr(var(27),'NTYPE'),var(171))),
	cf(1,eq(attr(var(27),'NUM'),var(172))),
	cf(or(C1,B1),eq(attr(var(27),'REF'),var(173))),
	cf(or(C2,B2),eq(attr(var(27),'SPEC'),var(174))),
	cf(1,eq(proj(var(27),'m::'),var(146))),
	cf(A2,eq(attr(var(27),'CASE'),'nom')),
	cf(1,eq(attr(var(27),'DEF'),'+')),
	cf(1,eq(attr(var(27),'PERS'),'3')),
	cf(A1,eq(var(169),var(33))),
	cf(B1,eq(var(169),semform('Abrams',0,[],[]))),
	cf(B2,eq(var(169),semform('pro',6,[],[]))),
	cf(A2,eq(var(169),var(68))),
	cf(A1,eq(var(170),var(34))),
	cf(A2,eq(var(170),var(69))),
	cf(B2,eq(attr(var(170),'_PREDET'),var(39))),
	cf(B1,eq(attr(var(170),'_SEL'),var(40))),
	cf(A1,eq(var(175),var(91))),
	cf(A2,eq(var(175),var(90))),
	cf(A1,eq(var(176),var(93))),
	cf(A2,eq(var(176),var(92))),
	cf(A1,eq(var(177),var(95))),
	cf(A2,eq(var(177),var(94))),
	cf(A1,eq(var(171),var(35))),
	cf(A2,eq(var(171),var(70))),
	cf(B1,eq(attr(var(171),'NSEM'),var(41))),
	cf(B1,eq(attr(var(171),'NSYN'),'proper')),
	cf(A1,eq(var(172),var(17))),
	cf(B1,eq(var(172),'sg')),
	cf(A2,eq(var(172),var(71))),
	cf(A1,eq(var(101),var(102))),
	cf(A1,eq(var(173),var(36))),
	cf(B1,eq(var(173),'+')),
	cf(A2,eq(var(173),var(72))),
	cf(B2,eq(var(174),var(37))),
	cf(C2,eq(var(174),var(73))),
	cf(B2,eq(attr(var(174),'POSS'),var(43))),
	cf(A1,eq(var(146),var(38))),
	cf(1,eq(attr(var(146),'H-CONS'),var(15))),
	cf(1,eq(attr(var(146),'RELS'),var(25))),
	cf(1,eq(attr(var(146),'RELS_EL'),var(23))),
	cf(1,eq(attr(var(146),'TOP'),var(60))),
	cf(1,eq(attr(var(146),'_QUANT'),var(178))),
	cf(A1,eq(var(15),var(14))),
	cf(A2,eq(var(15),var(123))),
	cf(B2,in_set(var(3),var(15))),
	cf(B2,in_set(var(4),var(15))),
	cf(B1,in_set(var(5),var(15))),
	cf(C1,in_set(var(11),var(15))),
	cf(C2,in_set(var(12),var(15))),
	cf(C2,in_set(var(13),var(15))),
	cf(A1,eq(var(25),var(24))),
	cf(A2,eq(var(25),var(120))),
	cf(B2,in_set(var(6),var(25))),
	cf(B2,in_set(var(7),var(25))),
	cf(A1,in_set(var(8),var(25))),
	cf(A1,in_set(var(9),var(25))),
	cf(B2,in_set(var(10),var(25))),
	cf(A1,eq(var(60),var(61))),
	cf(A2,eq(var(60),var(130))),
	cf(A1,eq(attr(var(60),'type'),'handle')),
	cf(A1,eq(var(178),var(66))),
	cf(A2,eq(var(178),var(124))),
	cf(A1,eq(attr(var(178),'RELS_EL'),var(8))),
	cf(A1,eq(attr(var(178),'TOP'),var(67))),
	cf(A2,eq(attr(var(147),'_ASPVERB'),var(179))),
	cf(A2,eq(attr(var(147),'_MAIN-CL'),'+')),
	cf(A2,eq(attr(var(147),'_PRTasp-FORM'),'på')),
	cf(A2,eq(attr(var(147),'_PRTasp-VERB'),'+')),
	cf(A2,eq(var(179),'+_')),
	cf(A1,eq(var(148),var(31))),
	cf(A2,eq(attr(var(148),'ASP'),'progressive')),
	cf(1,eq(attr(var(148),'MOOD'),'indicative')),
	cf(1,eq(attr(var(148),'TENSE'),'past')),
	cf(1,eq(attr(var(149),'H-CONS'),var(123))),
	cf(1,eq(attr(var(149),'INDEX'),var(180))),
	cf(1,eq(attr(var(149),'RELS'),var(181))),
	cf(1,eq(attr(var(149),'RELS_EL'),var(182))),
	cf(1,eq(attr(var(149),'TOP'),var(168))),
	cf(1,eq(attr(var(149),'_MSG'),var(183))),
	cf(1,eq(attr(var(149),'_MSGQEQ'),var(26))),
	cf(A1,eq(attr(var(149),'_RIGHTSCOPE'),var(153))),
	cf(A1,eq(var(180),var(161))),
	cf(1,eq(attr(var(180),'PERF'),'-')),
	cf(A2,eq(attr(var(180),'PROG'),'+')),
	cf(A2,eq(attr(var(180),'SF'),'prop')),
	cf(1,eq(attr(var(180),'TENSE'),'past')),
	cf(1,eq(attr(var(180),'type'),'event')),
	cf(1,in_set(var(182),var(181))),
	cf(1,eq(attr(var(182),'ARG0'),var(180))),
	cf(1,eq(attr(var(182),'ARG1'),var(132))),
	cf(1,eq(attr(var(182),'LBL'),var(167))),
	cf(1,eq(attr(var(182),'_CAT'),'v')),
	cf(1,eq(attr(var(182),'relation'),semform('bjeffe',16,[],[]))),
	cf(A1,eq(var(183),var(155))),
	cf(A2,eq(attr(var(183),'ARG0'),var(180))),
	cf(A2,eq(attr(var(183),'LBL'),var(168))),
	cf(A1,eq(attr(var(2),'PRED'),semform('drive',12,[var(28)],[]))),
	cf(A1,eq(attr(var(2),'SUBJ'),var(28))),
	cf(A1,eq(attr(var(2),'TOPIC'),var(28))),
	cf(A1,eq(attr(var(2),'CHECK'),var(184))),
	cf(A1,eq(attr(var(2),'TNS-ASP'),var(185))),
	cf(A1,eq(proj(var(2),'m::'),var(186))),
	cf(A1,eq(attr(var(2),'PRT-FORM'),'på')),
	cf(A1,eq(attr(var(2),'VFORM'),'fin')),
	cf(A1,eq(attr(var(2),'VTYPE'),'main')),
	cf(A1,eq(attr(var(184),'_PART'),'+_')),
	cf(A1,eq(attr(var(184),'_PRT-VERB'),'+')),
	cf(A2,eq(var(187),'+_')),
	cf(A2,eq(var(188),var(189))),
	cf(A1,eq(attr(var(185),'MOOD'),'indicative')),
	cf(A1,eq(attr(var(185),'TENSE'),'past')),
	cf(A1,eq(attr(var(186),'H-CONS'),var(14))),
	cf(A1,eq(attr(var(186),'INDEX'),var(159))),
	cf(A1,eq(attr(var(186),'RELS'),var(190))),
	cf(A1,eq(attr(var(186),'RELS_EL'),var(191))),
	cf(A1,eq(attr(var(186),'TOP'),var(154))),
	cf(A1,eq(attr(var(186),'_LEFTSCOPE'),var(153))),
	cf(A1,eq(attr(var(186),'_MSG'),var(155))),
	cf(A1,eq(attr(var(186),'_MSGQEQ'),var(156))),
	cf(A1,in_set(var(191),var(190))),
	cf(A1,eq(attr(var(191),'ARG0'),var(159))),
	cf(A1,eq(attr(var(191),'ARG1'),var(55))),
	cf(A1,eq(attr(var(191),'LBL'),var(158))),
	cf(A1,eq(attr(var(191),'_CAT'),'v')),
	cf(A1,eq(attr(var(191),'_PRT'),'på')),
	cf(A1,eq(attr(var(191),'relation'),semform('drive',13,[],[]))),
	cf(1,eq(attr(var(192),'LEFT_SISTER'),var(193))),
	cf(1,eq(attr(var(193),'RIGHT_DAUGHTER'),var(194))),
	cf(A1,eq(attr(var(193),'RIGHT_SISTER'),var(192))),
	cf(A1,eq(attr(var(194),'LEFT_SISTER'),var(195))),
	cf(A1,eq(attr(var(194),'RIGHT_DAUGHTER'),var(196))),
	cf(A1,eq(attr(var(195),'RIGHT_SISTER'),var(194))),
	cf(A1,eq(attr(var(196),'LEFT_SISTER'),var(197))),
	cf(A1,eq(attr(var(196),'RIGHT_DAUGHTER'),var(198))),
	cf(A1,eq(attr(var(197),'LEFT_SISTER'),var(199))),
	cf(A1,eq(attr(var(197),'RIGHT_SISTER'),var(196))),
	cf(A1,eq(attr(var(199),'RIGHT_SISTER'),var(197))),
	cf(1,eq(proj(var(198),'m::'),var(200))),
	cf(A1,eq(proj(var(201),'m::'),var(202))),
	cf(A1,eq(proj(var(203),'m::'),var(202))),
	cf(A2,eq(proj(var(204),'m::'),var(205))),
	cf(A2,eq(proj(var(206),'m::'),var(205))),
	cf(1,eq(proj(var(207),'m::'),var(200))),
	cf(or(C2,B2),eq(var(208),var(74))),
	cf(or(C2,B2),eq(attr(var(208),'RELS_EL'),var(18))),
	cf(or(C2,B2),eq(attr(var(208),'TOP'),var(130))),
	cf(A1,eq(var(209),var(2))),
	cf(A2,eq(var(209),var(0))),
	cf(A1,eq(attr(var(209),'PRED'),semform('drive',12,[var(28)],[]))),
	cf(A1,eq(attr(var(209),'SUBJ'),var(28))),
	cf(1,eq(attr(var(209),'CHECK'),var(210))),
	cf(1,eq(attr(var(209),'TNS-ASP'),var(211))),
	cf(1,eq(proj(var(209),'m::'),var(212))),
	cf(1,eq(attr(var(209),'VFORM'),'fin')),
	cf(1,eq(attr(var(209),'VTYPE'),'main')),
	cf(A1,eq(var(210),var(184))),
	cf(A2,eq(var(210),var(147))),
	cf(A2,eq(attr(var(210),'_ASPVERB'),var(187))),
	cf(A1,eq(attr(var(210),'_PRT-VERB'),'+')),
	cf(A2,eq(attr(var(210),'_PRTasp-VERB'),'+')),
	cf(A1,eq(var(211),var(185))),
	cf(A2,eq(var(211),var(148))),
	cf(A2,eq(attr(var(211),'ASP'),'progressive')),
	cf(1,eq(attr(var(211),'MOOD'),'indicative')),
	cf(1,eq(attr(var(211),'TENSE'),'past')),
	cf(A1,eq(var(212),var(186))),
	cf(A2,eq(var(212),var(149))),
	cf(A1,eq(attr(var(212),'H-CONS'),var(14))),
	cf(A1,eq(attr(var(212),'RELS'),var(190))),
	cf(1,eq(attr(var(212),'RELS_EL'),var(213))),
	cf(A1,eq(var(213),var(191))),
	cf(A2,eq(var(213),var(182))),
	cf(1,eq(attr(var(213),'ARG0'),var(214))),
	cf(A1,eq(attr(var(213),'LBL'),var(158))),
	cf(A1,eq(attr(var(213),'_CAT'),'v')),
	cf(A1,eq(attr(var(213),'_PRT'),'på')),
	cf(A1,eq(attr(var(213),'relation'),semform('drive',13,[],[]))),
	cf(A2,eq(var(214),var(180))),
	cf(A1,eq(var(214),var(159))),
	cf(A1,eq(attr(var(214),'PERF'),'-')),
	cf(A2,eq(attr(var(214),'PROG'),'+')),
	cf(1,eq(attr(var(214),'type'),'event')),
	cf(A1,eq(var(215),var(2))),
	cf(A2,eq(var(215),var(0))),
	cf(A2,eq(attr(var(215),'CHECK'),var(147))),
	cf(1,eq(attr(var(215),'TNS-ASP'),var(211))),
	cf(A2,eq(proj(var(215),'m::'),var(149))),
	cf(1,eq(attr(var(215),'VFORM'),'fin')),
	cf(A2,eq(attr(var(215),'VTYPE'),'main'))
	],
	% C-Structure:
	[
	cf(1,subtree(796,'ROOT',829,145)),
	cf(1,phi(796,var(0))),
	cf(1,subtree(829,'ROOT',-,1672)),
	cf(1,phi(829,var(0))),
	cf(A2,subtree(1672,'IP',2384,2561)),
	cf(A2,phi(1672,var(0))),
	cf(A2,subtree(2561,'I\'',2452,2457)),
	cf(A2,phi(2561,var(0))),
	cf(A2,subtree(2457,'S',-,930)),
	cf(A2,phi(2457,var(0))),
	cf(A2,subtree(930,'VPaspfin',929,403)),
	cf(A2,phi(930,var(0))),
	cf(A2,subtree(929,'VPaspfin',928,92)),
	cf(A2,phi(929,var(0))),
	cf(A2,subtree(928,'VPaspfin',-,73)),
	cf(A2,phi(928,var(0))),
	cf(A2,subtree(73,'PRTasp',-,57)),
	cf(A2,phi(73,var(0))),
	cf(A2,terminal(57,'på',[57])),
	cf(A2,phi(57,var(0))),
	cf(A2,subtree(92,'CONJasp',-,76)),
	cf(A2,phi(92,var(0))),
	cf(A2,terminal(76,'og',[76])),
	cf(A2,phi(76,var(0))),
	cf(A2,subtree(403,'VPfin',-,399)),
	cf(A2,phi(403,var(0))),
	cf(A1,subtree(1672,'IP',2384,2659)),
	cf(A1,phi(1672,var(0))),
	cf(A1,cproj(1672,var(193))),
	cf(or(C2,B2),subtree(2384,'IP',-,336)),
	cf(or(C2,B2),phi(2384,var(0))),
	cf(or(C2,B2),subtree(336,'POSSP',-,331)),
	cf(or(C2,B2),phi(336,var(29))),
	cf(or(C2,B2),subtree(331,'PROPPgen',-,328)),
	cf(or(C2,B2),phi(331,var(103))),
	cf(or(C2,B2),subtree(328,'PROPgen',327,14)),
	cf(or(C2,B2),phi(328,var(103))),
	cf(or(C2,B2),subtree(327,'PROPgen',325,16)),
	cf(or(C2,B2),phi(327,var(103))),
	cf(or(C2,B2),subtree(325,'PROPgen',323,18)),
	cf(or(C2,B2),phi(325,var(103))),
	cf(or(C2,B2),subtree(323,'PROPgen',320,20)),
	cf(or(C2,B2),phi(323,var(103))),
	cf(or(C2,B2),subtree(320,'PROPgen',-,22)),
	cf(or(C2,B2),phi(320,var(103))),
	cf(or(C2,B2),subtree(22,'PROP_BASE',-,23)),
	cf(or(C2,B2),phi(22,var(103))),
	cf(or(C2,B2),terminal(23,'Abram',[1])),
	cf(or(C2,B2),phi(23,var(216))),
	cf(or(C2,B2),subtree(20,'N_SUFF_BASE',-,21)),
	cf(or(C2,B2),phi(20,var(103))),
	cf(or(C2,B2),terminal(21,'+Prop',[1])),
	cf(or(C2,B2),phi(21,var(103))),
	cf(or(C2,B2),subtree(18,'N_SUFF_BASE',-,19)),
	cf(or(C2,B2),phi(18,var(103))),
	cf(or(C2,B2),terminal(19,'+Indef',[1])),
	cf(or(C2,B2),phi(19,var(103))),
	cf(or(C2,B2),subtree(16,'N_SUFF_BASE',-,17)),
	cf(or(C2,B2),phi(16,var(103))),
	cf(or(C2,B2),terminal(17,'+Sg',[1])),
	cf(or(C2,B2),phi(17,var(103))),
	cf(or(C2,B2),subtree(14,'N_SUFF_BASE',-,15)),
	cf(or(C2,B2),phi(14,var(103))),
	cf(or(C2,B2),terminal(15,'+Gen',[1])),
	cf(or(C2,B2),phi(15,var(103))),
	cf(or(C1,B1),subtree(2384,'IP',-,2671)),
	cf(or(C1,B1),phi(2384,var(0))),
	cf(or(C1,B1),subtree(2671,'PROPP',-,13)),
	cf(or(C1,B1),phi(2671,var(29))),
	cf(or(C1,B1),subtree(13,'PROP',-,1)),
	cf(or(C1,B1),phi(13,var(29))),
	cf(or(C1,B1),terminal(1,'Abrams',[1])),
	cf(or(C1,B1),phi(1,var(208))),
	cf(A1,subtree(2659,'I\'coord',2658,1651)),
	cf(A1,phi(2659,var(0))),
	cf(A1,cproj(2659,var(194))),
	cf(A1,subtree(2658,'I\'coord',2603,95)),
	cf(A1,phi(2658,var(0))),
	cf(A1,cproj(2658,var(194))),
	cf(A1,subtree(2603,'I\'coord',-,2602)),
	cf(A1,phi(2603,var(0))),
	cf(A1,subtree(2602,'I\'',2452,2455)),
	cf(A1,phi(2602,var(2))),
	cf(A1,subtree(2452,'I\'',-,849)),
	cf(A1,phi(2452,var(2))),
	cf(A1,subtree(849,'Vfin',848,43)),
	cf(A1,phi(849,var(2))),
	cf(A1,cproj(849,var(201))),
	cf(A1,subtree(848,'Vfin',847,45)),
	cf(A1,phi(848,var(2))),
	cf(A1,cproj(848,var(201))),
	cf(A1,subtree(847,'Vfin',-,47)),
	cf(A1,phi(847,var(2))),
	cf(A1,cproj(847,var(201))),
	cf(A1,subtree(47,'V_BASE',-,48)),
	cf(A1,phi(47,var(2))),
	cf(A1,cproj(47,var(203))),
	cf(A1,terminal(48,'drive',[25])),
	cf(A1,phi(48,var(2))),
	cf(A1,cproj(48,var(203))),
	cf(A2,subtree(2452,'I\'',-,389)),
	cf(A2,phi(2452,var(0))),
	cf(A2,subtree(389,'Vasp',388,43)),
	cf(A2,phi(389,var(0))),
	cf(A2,cproj(389,var(204))),
	cf(A2,subtree(388,'Vasp',387,45)),
	cf(A2,phi(388,var(0))),
	cf(A2,cproj(388,var(204))),
	cf(A2,subtree(387,'Vasp',-,49)),
	cf(A2,phi(387,var(0))),
	cf(A2,cproj(387,var(204))),
	cf(A2,subtree(49,'Vasp_BASE',-,48)),
	cf(A2,phi(49,var(0))),
	cf(A2,cproj(49,var(206))),
	cf(A2,terminal(48,'drive',[25])),
	cf(A2,phi(48,var(0))),
	cf(A2,cproj(48,var(206))),
	cf(1,subtree(45,'V_SUFF_BASE',-,46)),
	cf(1,phi(45,var(209))),
	cf(1,terminal(46,'+Verb',[25])),
	cf(1,phi(46,var(209))),
	cf(1,subtree(43,'V_SUFF_BASE',-,44)),
	cf(1,phi(43,var(215))),
	cf(1,terminal(44,'+Past',[25])),
	cf(1,phi(44,var(215))),
	cf(A1,subtree(2455,'S',-,2469)),
	cf(A1,phi(2455,var(2))),
	cf(A1,subtree(2469,'VPmain',-,74)),
	cf(A1,phi(2469,var(2))),
	cf(A1,subtree(74,'PRT',-,57)),
	cf(A1,phi(74,var(2))),
	cf(A1,terminal(57,'på',[57])),
	cf(A1,phi(57,var(2))),
	cf(A1,subtree(95,'CONJev',-,76)),
	cf(A1,phi(95,var(0))),
	cf(A1,cproj(95,var(197))),
	cf(A1,terminal(76,'og',[76])),
	cf(A1,phi(76,var(0))),
	cf(A1,cproj(76,var(197))),
	cf(A1,subtree(1651,'I\'',-,399)),
	cf(A1,phi(1651,var(1))),
	cf(1,subtree(399,'Vfin',398,101)),
	cf(1,phi(399,var(1))),
	cf(1,cproj(399,var(198))),
	cf(1,subtree(398,'Vfin',397,103)),
	cf(1,phi(398,var(1))),
	cf(1,cproj(398,var(198))),
	cf(1,subtree(397,'Vfin',-,105)),
	cf(1,phi(397,var(1))),
	cf(1,cproj(397,var(198))),
	cf(1,subtree(105,'V_BASE',-,106)),
	cf(1,phi(105,var(1))),
	cf(1,cproj(105,var(207))),
	cf(1,terminal(106,'bjeffe',[100])),
	cf(1,phi(106,var(1))),
	cf(1,cproj(106,var(207))),
	cf(1,subtree(103,'V_SUFF_BASE',-,104)),
	cf(1,phi(103,var(1))),
	cf(1,terminal(104,'+Verb',[100])),
	cf(1,phi(104,var(1))),
	cf(1,subtree(101,'V_SUFF_BASE',-,102)),
	cf(1,phi(101,var(1))),
	cf(1,terminal(102,'+Past',[100])),
	cf(1,phi(102,var(1))),
	cf(1,subtree(145,'PERIOD',-,138)),
	cf(1,phi(145,var(0))),
	cf(1,terminal(138,'.',[138])),
	cf(1,phi(138,var(0))),
	cf(or(C1,B1),semform_data(0,13,1,7)),
	cf(or(C1,B1),semform_data(1,13,1,7)),
	cf(or(C1,B1),semform_data(2,13,1,7)),
	cf(or(C2,B2),semform_data(3,22,1,5)),
	cf(or(C2,B2),semform_data(4,22,1,5)),
	cf(or(C2,B2),semform_data(5,22,1,5)),
	cf(or(C2,B2),semform_data(6,336,1,7)),
	cf(or(C2,B2),semform_data(7,336,1,7)),
	cf(or(C2,B2),semform_data(8,336,1,7)),
	cf(or(C2,B2),semform_data(9,336,1,7)),
	cf(A1,semform_data(12,47,8,12)),
	cf(A1,semform_data(13,47,8,12)),
	cf(A1,semform_data(14,95,16,18)),
	cf(1,semform_data(15,105,19,26)),
	cf(1,semform_data(16,105,19,26)),
	cf(1,fspan(var(0),1,27)),
	cf(or(C2,B2),fspan(var(29),1,7)),
	cf(or(C2,B2),fspan(var(103),1,7)),
	cf(or(C1,B1),fspan(var(29),1,7)),
	cf(or(C1,B1),fspan(var(208),1,7)),
	cf(A1,fspan(var(1),19,27)),
	cf(A1,fspan(var(2),8,15)),
	cf(A1,fspan(var(215),12,12)),
	cf(A1,fspan(var(209),12,12)),
	cf(1,fspan(var(0),26,27)),
	cf(1,surfaceform(1,'Abrams',1,7)),
	cf(1,surfaceform(25,'drev',8,12)),
	cf(1,surfaceform(57,'på',13,15)),
	cf(1,surfaceform(76,'og',16,18)),
	cf(1,surfaceform(100,'bjeffet',19,27)),
	cf(1,surfaceform(138,'.',26,27))
	]).
