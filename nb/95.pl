% -*- coding: iso-8859-1 -*-

fstructure('Browne ankom tirsdag morgen.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.07 CPU seconds, 148 subtrees unified'),
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
	cf(1,eq(attr(var(0),'TOPIC'),var(17))),
	cf(1,eq(attr(var(0),'CHECK'),var(19))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(20))),
	cf(1,eq(proj(var(0),'m::'),var(21))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(A1,eq(var(15),semform('ankomme',4,[var(17)],[]))),
	cf(A2,eq(var(15),semform('ankomme',4,[var(17),var(16)],['NULL']))),
	cf(A2,eq(attr(var(16),'PRED'),semform('morgen',14,[],[]))),
	cf(A2,eq(attr(var(16),'CHECK'),var(22))),
	cf(A2,eq(attr(var(16),'GEND'),var(23))),
	cf(A2,eq(attr(var(16),'NTYPE'),var(24))),
	cf(A2,eq(attr(var(16),'SPEC'),var(25))),
	cf(A2,eq(proj(var(16),'m::'),var(26))),
	cf(A2,eq(attr(var(16),'CASE'),'obl')),
	cf(A2,eq(attr(var(16),'DEF'),'+')),
	cf(A2,eq(attr(var(16),'NUM'),'sg')),
	cf(A2,eq(attr(var(16),'PERS'),'3')),
	cf(A2,eq(attr(var(16),'REF'),'+')),
	cf(A2,eq(attr(var(22),'_DEF-MORPH'),'-')),
	cf(A2,eq(attr(var(22),'_NOUN'),'+')),
	cf(A2,eq(attr(var(22),'_PREDET'),'+')),
	cf(A2,eq(attr(var(22),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(23),'FEM'),'-')),
	cf(1,eq(attr(var(23),'MASC'),'+')),
	cf(1,eq(attr(var(23),'NEUT'),'-')),
	cf(1,eq(attr(var(24),'NSEM'),var(27))),
	cf(1,eq(attr(var(24),'NSYN'),'common')),
	cf(1,eq(attr(var(27),'TIME'),var(28))),
	cf(1,eq(attr(var(27),'COMMON'),'count')),
	cf(1,eq(attr(var(28),'TEMPNOUN'),'+')),
	cf(1,eq(attr(var(28),'TOFD'),'+')),
	cf(1,eq(attr(var(25),'POSS'),var(29))),
	cf(1,eq(attr(var(29),'PRED'),semform('tirsdag',6,[],[]))),
	cf(1,eq(attr(var(29),'CHECK'),var(30))),
	cf(1,eq(attr(var(29),'GEND'),var(31))),
	cf(1,eq(attr(var(29),'NTYPE'),var(32))),
	cf(1,eq(proj(var(29),'m::'),var(33))),
	cf(1,eq(attr(var(29),'DEF'),'+')),
	cf(1,eq(attr(var(29),'NUM'),'sg')),
	cf(1,eq(attr(var(29),'PERS'),'3')),
	cf(1,eq(attr(var(29),'REF'),'+')),
	cf(1,eq(attr(var(30),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(30),'_NOUN'),'+')),
	cf(1,eq(attr(var(31),'FEM'),'-')),
	cf(1,eq(attr(var(31),'MASC'),'+')),
	cf(1,eq(attr(var(31),'NEUT'),'-')),
	cf(1,eq(attr(var(32),'NSEM'),var(34))),
	cf(1,eq(attr(var(32),'NSYN'),'common')),
	cf(1,eq(attr(var(34),'TIME'),var(35))),
	cf(1,eq(attr(var(34),'COMMON'),'count')),
	cf(1,eq(attr(var(35),'TEMPNOUN'),'+')),
	cf(1,eq(attr(var(35),'WEEKDAY'),'+')),
	cf(1,eq(attr(var(33),'H-CONS'),var(36))),
	cf(1,eq(attr(var(33),'RELS'),var(37))),
	cf(1,eq(attr(var(33),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(33),'TOP'),var(38))),
	cf(1,in_set(var(1),var(36))),
	cf(1,in_set(var(2),var(36))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(39))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(40))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,eq(attr(var(40),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(41))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(42))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(41),'type'),'handle')),
	cf(1,eq(attr(var(42),'type'),'handle')),
	cf(A1,eq(var(37),var(43))),
	cf(A1,in_set(var(3),var(37))),
	cf(1,in_set(var(4),var(37))),
	cf(1,in_set(var(5),var(37))),
	cf(1,in_set(var(6),var(37))),
	cf(A1,in_set(var(7),var(37))),
	cf(1,in_set(var(8),var(37))),
	cf(A2,in_set(var(9),var(37))),
	cf(A2,eq(var(3),var(9))),
	cf(1,eq(attr(var(3),'ARG0'),var(44))),
	cf(1,eq(attr(var(3),'LBL'),var(39))),
	cf(1,eq(attr(var(3),'_CAT'),'n')),
	cf(1,eq(attr(var(3),'relation'),semform('morgen',15,[],[]))),
	cf(A2,eq(var(44),var(45))),
	cf(1,eq(attr(var(44),'DIV'),'-')),
	cf(1,eq(attr(var(44),'GRIND'),'-')),
	cf(1,eq(attr(var(44),'NATGEND'),'gender')),
	cf(1,eq(attr(var(44),'NUM'),'sg')),
	cf(1,eq(attr(var(44),'PERS'),'3')),
	cf(1,eq(attr(var(44),'type'),'ref-ind')),
	cf(1,eq(attr(var(4),'ARG1'),var(46))),
	cf(1,eq(attr(var(4),'ARG2'),var(44))),
	cf(1,eq(attr(var(4),'LBL'),var(39))),
	cf(1,eq(attr(var(4),'relation'),semform('poss',9,[],[]))),
	cf(1,eq(attr(var(46),'NUM'),'sg')),
	cf(1,eq(attr(var(46),'PERS'),'3')),
	cf(1,eq(attr(var(46),'type'),'ref-ind')),
	cf(1,eq(attr(var(5),'ARG0'),var(46))),
	cf(1,eq(attr(var(5),'LBL'),var(41))),
	cf(1,eq(attr(var(5),'CARG'),'tirsdag')),
	cf(1,eq(attr(var(5),'relation'),semform('dofw',7,[],[]))),
	cf(1,eq(attr(var(6),'ARG0'),var(46))),
	cf(1,eq(attr(var(6),'BODY'),var(47))),
	cf(1,eq(attr(var(6),'LBL'),var(38))),
	cf(1,eq(attr(var(6),'RSTR'),var(42))),
	cf(1,eq(attr(var(6),'relation'),semform('proper_q',8,[],[]))),
	cf(1,eq(attr(var(47),'type'),'handle')),
	cf(1,eq(attr(var(38),'type'),'handle')),
	cf(A1,eq(attr(var(7),'ARG0'),var(48))),
	cf(A1,eq(attr(var(7),'ARG1'),var(49))),
	cf(A1,eq(attr(var(7),'ARG2'),var(44))),
	cf(A1,eq(attr(var(7),'LBL'),var(50))),
	cf(A1,eq(attr(var(7),'relation'),semform('unspec_loc',13,[],[]))),
	cf(A1,eq(attr(var(48),'type'),'event')),
	cf(1,eq(attr(var(49),'PERF'),'-')),
	cf(1,eq(attr(var(49),'SF'),'prop')),
	cf(1,eq(attr(var(49),'TENSE'),'past')),
	cf(1,eq(attr(var(49),'type'),'event')),
	cf(1,eq(attr(var(50),'type'),'handle')),
	cf(1,eq(attr(var(8),'ARG0'),var(44))),
	cf(1,eq(attr(var(8),'BODY'),var(51))),
	cf(1,eq(attr(var(8),'LBL'),var(52))),
	cf(1,eq(attr(var(8),'RSTR'),var(40))),
	cf(1,eq(attr(var(8),'relation'),semform('def_explicit_q',10,[],[]))),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(A2,eq(attr(var(9),'ARG0'),var(45))),
	cf(A2,eq(attr(var(9),'LBL'),var(39))),
	cf(A2,eq(attr(var(9),'_CAT'),'n')),
	cf(A2,eq(attr(var(9),'relation'),semform('morgen',15,[],[]))),
	cf(A2,eq(attr(var(45),'DIV'),'-')),
	cf(A2,eq(attr(var(45),'GRIND'),'-')),
	cf(A2,eq(attr(var(45),'NATGEND'),'gender')),
	cf(A2,eq(attr(var(45),'NUM'),'sg')),
	cf(A2,eq(attr(var(45),'PERS'),'3')),
	cf(A2,eq(attr(var(45),'type'),'ref-ind')),
	cf(A2,eq(attr(var(26),'H-CONS'),var(36))),
	cf(A2,eq(attr(var(26),'RELS'),var(37))),
	cf(A2,eq(attr(var(26),'RELS_EL'),var(9))),
	cf(A2,eq(attr(var(26),'TOP'),var(52))),
	cf(A2,eq(attr(var(26),'_ANTECEDVAR'),var(53))),
	cf(A2,eq(attr(var(26),'_QUANT'),var(54))),
	cf(1,eq(attr(var(53),'NUM'),'sg')),
	cf(1,eq(attr(var(53),'PERS'),'3')),
	cf(1,eq(attr(var(53),'type'),'ref-ind')),
	cf(1,eq(attr(var(54),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(54),'TOP'),var(55))),
	cf(1,eq(attr(var(55),'type'),'handle')),
	cf(1,eq(attr(var(17),'PRED'),semform('Browne',0,[],[]))),
	cf(1,eq(attr(var(17),'CHECK'),var(56))),
	cf(1,eq(attr(var(17),'GEND'),var(57))),
	cf(1,eq(attr(var(17),'NTYPE'),var(58))),
	cf(1,eq(proj(var(17),'m::'),var(59))),
	cf(1,eq(attr(var(17),'CASE'),'nom')),
	cf(1,eq(attr(var(17),'DEF'),'+')),
	cf(1,eq(attr(var(17),'NUM'),'sg')),
	cf(1,eq(attr(var(17),'PERS'),'3')),
	cf(1,eq(attr(var(17),'REF'),'+')),
	cf(1,eq(attr(var(56),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(57),'FEM'),'-')),
	cf(1,eq(attr(var(57),'MASC'),'+')),
	cf(1,eq(attr(var(57),'NEUT'),'-')),
	cf(1,eq(attr(var(58),'NSEM'),var(60))),
	cf(1,eq(attr(var(58),'NSYN'),'proper')),
	cf(1,eq(attr(var(60),'PROPER'),var(61))),
	cf(1,eq(attr(var(61),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(59),'H-CONS'),var(62))),
	cf(1,eq(attr(var(59),'RELS'),var(63))),
	cf(1,eq(attr(var(59),'RELS_EL'),var(12))),
	cf(1,eq(attr(var(59),'TOP'),var(64))),
	cf(1,eq(attr(var(59),'_QUANT'),var(65))),
	cf(1,in_set(var(10),var(62))),
	cf(1,in_set(var(11),var(62))),
	cf(1,eq(attr(var(10),'OUTSCPD'),var(66))),
	cf(1,eq(attr(var(10),'SC_ARG'),var(67))),
	cf(1,eq(attr(var(10),'relation'),'qeq')),
	cf(1,eq(attr(var(66),'type'),'handle')),
	cf(1,eq(attr(var(67),'type'),'handle')),
	cf(1,eq(attr(var(11),'OUTSCPD'),var(50))),
	cf(1,eq(attr(var(11),'SC_ARG'),var(68))),
	cf(1,eq(attr(var(11),'relation'),'qeq')),
	cf(1,eq(attr(var(68),'type'),'handle')),
	cf(1,in_set(var(12),var(63))),
	cf(1,in_set(var(13),var(63))),
	cf(1,eq(attr(var(12),'ARG0'),var(53))),
	cf(1,eq(attr(var(12),'LBL'),var(66))),
	cf(1,eq(attr(var(12),'CARG'),'Browne')),
	cf(1,eq(attr(var(12),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(13),'ARG0'),var(53))),
	cf(1,eq(attr(var(13),'BODY'),var(69))),
	cf(1,eq(attr(var(13),'LBL'),var(64))),
	cf(1,eq(attr(var(13),'RSTR'),var(67))),
	cf(1,eq(attr(var(13),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(69),'type'),'handle')),
	cf(1,eq(attr(var(64),'type'),'handle')),
	cf(1,eq(attr(var(65),'RELS_EL'),var(13))),
	cf(1,eq(attr(var(65),'TOP'),var(70))),
	cf(1,eq(attr(var(70),'type'),'handle')),
	cf(A1,in_set(var(71),var(18))),
	cf(A2,eq(var(71),var(16))),
	cf(1,eq(attr(var(71),'PRED'),semform('morgen',14,[],[]))),
	cf(A1,eq(attr(var(71),'CASE'),var(72))),
	cf(1,eq(attr(var(71),'CHECK'),var(73))),
	cf(1,eq(attr(var(71),'GEND'),var(23))),
	cf(1,eq(attr(var(71),'NTYPE'),var(24))),
	cf(1,eq(attr(var(71),'SPEC'),var(25))),
	cf(1,eq(proj(var(71),'m::'),var(74))),
	cf(1,eq(attr(var(71),'DEF'),'+')),
	cf(1,eq(attr(var(71),'NUM'),'sg')),
	cf(1,eq(attr(var(71),'PERS'),'3')),
	cf(1,eq(attr(var(71),'REF'),'+')),
	cf(A1,eq(var(72),'nom')),
	cf(A2,eq(var(72),'obl')),
	cf(A2,eq(var(73),var(22))),
	cf(1,eq(attr(var(73),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(73),'_NOUN'),'+')),
	cf(1,eq(attr(var(73),'_PREDET'),'+')),
	cf(A2,eq(var(74),var(26))),
	cf(1,eq(attr(var(74),'H-CONS'),var(36))),
	cf(1,eq(attr(var(74),'RELS'),var(37))),
	cf(1,eq(attr(var(74),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(74),'TOP'),var(52))),
	cf(A2,eq(attr(var(74),'_ANTECEDVAR'),var(53))),
	cf(1,eq(attr(var(74),'_QUANT'),var(54))),
	cf(A1,eq(attr(var(19),'_AUX-SELECT'),'be')),
	cf(1,eq(attr(var(19),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(20),'MOOD'),'indicative')),
	cf(1,eq(attr(var(20),'TENSE'),'past')),
	cf(1,eq(attr(var(21),'H-CONS'),var(62))),
	cf(1,eq(attr(var(21),'INDEX'),var(49))),
	cf(1,eq(attr(var(21),'RELS'),var(43))),
	cf(1,eq(attr(var(21),'RELS_EL'),var(14))),
	cf(1,eq(attr(var(21),'TOP'),var(68))),
	cf(1,eq(attr(var(21),'_MSG'),var(75))),
	cf(1,eq(attr(var(21),'_MSGQEQ'),var(11))),
	cf(A1,in_set(var(3),var(43))),
	cf(A1,in_set(var(4),var(43))),
	cf(A1,in_set(var(5),var(43))),
	cf(A1,in_set(var(6),var(43))),
	cf(A1,in_set(var(7),var(43))),
	cf(A1,in_set(var(8),var(43))),
	cf(1,in_set(var(14),var(43))),
	cf(1,eq(attr(var(14),'ARG0'),var(49))),
	cf(1,eq(attr(var(14),'ARG1'),var(53))),
	cf(A2,eq(attr(var(14),'ARG2'),var(45))),
	cf(1,eq(attr(var(14),'LBL'),var(50))),
	cf(1,eq(attr(var(14),'_CAT'),'v')),
	cf(1,eq(attr(var(14),'relation'),semform('ankomme',5,[],[]))),
	cf(1,eq(attr(var(75),'ARG0'),var(49))),
	cf(1,eq(attr(var(75),'LBL'),var(68))),
	cf(1,eq(proj(var(76),'m::'),var(77))),
	cf(1,eq(proj(var(78),'m::'),var(77))),
	cf(1,eq(proj(var(79),'m::'),var(80))),
	cf(1,eq(proj(var(81),'m::'),var(80))),
	cf(1,eq(proj(var(82),'m::'),var(83))),
	cf(1,eq(proj(var(84),'m::'),var(83)))
	],
	% C-Structure:
	[
	cf(1,subtree(773,'ROOT',825,74)),
	cf(1,phi(773,var(0))),
	cf(1,subtree(825,'ROOT',-,1759)),
	cf(1,phi(825,var(0))),
	cf(1,subtree(1759,'IP',1681,1757)),
	cf(1,phi(1759,var(0))),
	cf(1,subtree(1681,'IP',-,1823)),
	cf(1,phi(1681,var(0))),
	cf(1,subtree(1823,'PROPP',-,1809)),
	cf(1,phi(1823,var(17))),
	cf(1,subtree(1809,'PROP',1807,2)),
	cf(1,phi(1809,var(17))),
	cf(1,subtree(1807,'PROP',1805,4)),
	cf(1,phi(1807,var(17))),
	cf(1,subtree(1805,'PROP',1803,6)),
	cf(1,phi(1805,var(17))),
	cf(1,subtree(1803,'PROP',1800,8)),
	cf(1,phi(1803,var(17))),
	cf(1,subtree(1800,'PROP',-,10)),
	cf(1,phi(1800,var(17))),
	cf(1,subtree(10,'PROP_BASE',-,11)),
	cf(1,phi(10,var(17))),
	cf(1,terminal(11,'Browne',[1])),
	cf(1,phi(11,var(85))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(17))),
	cf(1,terminal(9,'+Prop',[1])),
	cf(1,phi(9,var(17))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(17))),
	cf(1,terminal(7,'+Masc',[1])),
	cf(1,phi(7,var(17))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(17))),
	cf(1,terminal(5,'+Indef',[1])),
	cf(1,phi(5,var(17))),
	cf(1,subtree(2,'N_SUFF_BASE',-,3)),
	cf(1,phi(2,var(17))),
	cf(1,terminal(3,'+Sg',[1])),
	cf(1,phi(3,var(17))),
	cf(1,subtree(1757,'I\'',1724,1731)),
	cf(1,phi(1757,var(0))),
	cf(1,subtree(1724,'I\'',-,1226)),
	cf(1,phi(1724,var(0))),
	cf(1,subtree(1226,'Vfin',1225,15)),
	cf(1,phi(1226,var(0))),
	cf(1,cproj(1226,var(76))),
	cf(1,subtree(1225,'Vfin',1224,17)),
	cf(1,phi(1225,var(0))),
	cf(1,cproj(1225,var(76))),
	cf(1,subtree(1224,'Vfin',-,19)),
	cf(1,phi(1224,var(0))),
	cf(1,cproj(1224,var(76))),
	cf(1,subtree(19,'V_BASE',-,20)),
	cf(1,phi(19,var(0))),
	cf(1,cproj(19,var(78))),
	cf(1,terminal(20,'ankomme',[14])),
	cf(1,phi(20,var(0))),
	cf(1,cproj(20,var(78))),
	cf(1,subtree(17,'V_SUFF_BASE',-,18)),
	cf(1,phi(17,var(0))),
	cf(1,terminal(18,'+Verb',[14])),
	cf(1,phi(18,var(0))),
	cf(1,subtree(15,'V_SUFF_BASE',-,16)),
	cf(1,phi(15,var(0))),
	cf(1,terminal(16,'+Past',[14])),
	cf(1,phi(16,var(0))),
	cf(1,subtree(1731,'S',-,1734)),
	cf(1,phi(1731,var(0))),
	cf(A1,subtree(1734,'VPmain',-,958)),
	cf(A1,phi(1734,var(0))),
	cf(A2,subtree(1734,'VPmain',-,958)),
	cf(A2,phi(1734,var(0))),
	cf(1,subtree(958,'TofD',957,49)),
	cf(1,phi(958,var(71))),
	cf(1,subtree(957,'TofD',-,47)),
	cf(1,phi(957,var(71))),
	cf(1,subtree(47,'N',978,31)),
	cf(1,phi(47,var(29))),
	cf(1,cproj(47,var(79))),
	cf(1,subtree(978,'N',977,33)),
	cf(1,phi(978,var(29))),
	cf(1,cproj(978,var(79))),
	cf(1,subtree(977,'N',976,35)),
	cf(1,phi(977,var(29))),
	cf(1,cproj(977,var(79))),
	cf(1,subtree(976,'N',963,37)),
	cf(1,phi(976,var(29))),
	cf(1,cproj(976,var(79))),
	cf(1,subtree(963,'N',-,39)),
	cf(1,phi(963,var(29))),
	cf(1,cproj(963,var(79))),
	cf(1,subtree(39,'N_BASE',-,40)),
	cf(1,phi(39,var(29))),
	cf(1,cproj(39,var(81))),
	cf(1,terminal(40,'tirsdag',[30])),
	cf(1,phi(40,var(86))),
	cf(1,cproj(40,var(81))),
	cf(1,subtree(37,'N_SUFF_BASE',-,38)),
	cf(1,phi(37,var(29))),
	cf(1,terminal(38,'+Noun',[30])),
	cf(1,phi(38,var(29))),
	cf(1,subtree(35,'N_SUFF_BASE',-,36)),
	cf(1,phi(35,var(29))),
	cf(1,terminal(36,'+Masc',[30])),
	cf(1,phi(36,var(29))),
	cf(1,subtree(33,'N_SUFF_BASE',-,34)),
	cf(1,phi(33,var(29))),
	cf(1,terminal(34,'+Indef',[30])),
	cf(1,phi(34,var(29))),
	cf(1,subtree(31,'N_SUFF_BASE',-,32)),
	cf(1,phi(31,var(29))),
	cf(1,terminal(32,'+Sg',[30])),
	cf(1,phi(32,var(29))),
	cf(1,subtree(49,'N',845,51)),
	cf(1,phi(49,var(71))),
	cf(1,cproj(49,var(82))),
	cf(1,subtree(845,'N',844,53)),
	cf(1,phi(845,var(71))),
	cf(1,cproj(845,var(82))),
	cf(1,subtree(844,'N',843,55)),
	cf(1,phi(844,var(71))),
	cf(1,cproj(844,var(82))),
	cf(1,subtree(843,'N',839,57)),
	cf(1,phi(843,var(71))),
	cf(1,cproj(843,var(82))),
	cf(1,subtree(839,'N',-,59)),
	cf(1,phi(839,var(71))),
	cf(1,cproj(839,var(82))),
	cf(1,subtree(59,'N_BASE',-,60)),
	cf(1,phi(59,var(71))),
	cf(1,cproj(59,var(84))),
	cf(1,terminal(60,'morgen',[50])),
	cf(1,phi(60,var(71))),
	cf(1,cproj(60,var(84))),
	cf(1,subtree(57,'N_SUFF_BASE',-,58)),
	cf(1,phi(57,var(71))),
	cf(1,terminal(58,'+Noun',[50])),
	cf(1,phi(58,var(71))),
	cf(1,subtree(55,'N_SUFF_BASE',-,56)),
	cf(1,phi(55,var(71))),
	cf(1,terminal(56,'+Masc',[50])),
	cf(1,phi(56,var(71))),
	cf(1,subtree(53,'N_SUFF_BASE',-,54)),
	cf(1,phi(53,var(71))),
	cf(1,terminal(54,'+Indef',[50])),
	cf(1,phi(54,var(71))),
	cf(1,subtree(51,'N_SUFF_BASE',-,52)),
	cf(1,phi(51,var(71))),
	cf(1,terminal(52,'+Sg',[50])),
	cf(1,phi(52,var(71))),
	cf(1,subtree(74,'PERIOD',-,67)),
	cf(1,phi(74,var(0))),
	cf(1,terminal(67,'.',[67])),
	cf(1,phi(67,var(0))),
	cf(1,semform_data(0,10,1,6)),
	cf(1,semform_data(1,10,1,6)),
	cf(1,semform_data(2,10,1,6)),
	cf(1,semform_data(4,19,8,13)),
	cf(1,semform_data(5,19,8,13)),
	cf(1,semform_data(6,39,14,20)),
	cf(1,semform_data(7,39,14,20)),
	cf(1,semform_data(8,39,14,20)),
	cf(1,semform_data(9,957,14,21)),
	cf(1,semform_data(10,957,14,21)),
	cf(A1,semform_data(13,1734,14,29)),
	cf(1,semform_data(14,59,22,27)),
	cf(1,semform_data(15,59,22,27)),
	cf(1,fspan(var(0),1,29)),
	cf(1,fspan(var(17),1,7)),
	cf(1,fspan(var(71),14,29)),
	cf(1,fspan(var(29),14,21)),
	cf(1,surfaceform(1,'Browne',1,7)),
	cf(1,surfaceform(14,'ankom',8,13)),
	cf(1,surfaceform(30,'tirsdag',14,21)),
	cf(1,surfaceform(50,'morgen',22,29)),
	cf(1,surfaceform(67,'.',28,29))
	]).

