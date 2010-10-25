% -*- coding: iso-8859-1 -*-

fstructure('Abrams rakte Browne sigaretten.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.10 CPU seconds, 121 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('rekke-hand',4,[var(9),var(10),var(11)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(9))),
	cf(1,eq(attr(var(0),'OBJben'),var(10))),
	cf(1,eq(attr(var(0),'OBJ'),var(11))),
	cf(1,eq(attr(var(0),'TOPIC'),var(9))),
	cf(1,eq(attr(var(0),'CHECK'),var(12))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(13))),
	cf(1,eq(proj(var(0),'m::'),var(14))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(9),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(15))),
	cf(1,eq(attr(var(9),'NTYPE'),var(16))),
	cf(1,eq(proj(var(9),'m::'),var(17))),
	cf(1,eq(attr(var(9),'CASE'),'nom')),
	cf(1,eq(attr(var(9),'DEF'),'+')),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(9),'REF'),'+')),
	cf(1,eq(attr(var(15),'_SEL'),var(18))),
	cf(1,eq(attr(var(18),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(18),'_ANIM'),'+')),
	cf(1,eq(attr(var(18),'_HUMAN'),'+')),
	cf(1,eq(attr(var(16),'NSEM'),var(19))),
	cf(1,eq(attr(var(16),'NSYN'),'proper')),
	cf(1,eq(attr(var(19),'PROPER'),var(20))),
	cf(1,eq(attr(var(20),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(17),'H-CONS'),var(21))),
	cf(1,eq(attr(var(17),'RELS'),var(22))),
	cf(1,eq(attr(var(17),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(17),'TOP'),var(23))),
	cf(1,eq(attr(var(17),'_QUANT'),var(24))),
	cf(1,in_set(var(1),var(21))),
	cf(1,in_set(var(2),var(21))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(25))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(26))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(27))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(28))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,in_set(var(3),var(22))),
	cf(1,in_set(var(4),var(22))),
	cf(1,eq(attr(var(3),'ARG0'),var(29))),
	cf(1,eq(attr(var(3),'LBL'),var(25))),
	cf(1,eq(attr(var(3),'CARG'),'Abrams')),
	cf(1,eq(attr(var(3),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(29),'NATGEND'),'animate')),
	cf(1,eq(attr(var(29),'NUM'),'sg')),
	cf(1,eq(attr(var(29),'PERS'),'3')),
	cf(1,eq(attr(var(29),'type'),'ref-ind')),
	cf(1,eq(attr(var(4),'ARG0'),var(29))),
	cf(1,eq(attr(var(4),'BODY'),var(30))),
	cf(1,eq(attr(var(4),'LBL'),var(23))),
	cf(1,eq(attr(var(4),'RSTR'),var(26))),
	cf(1,eq(attr(var(4),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(23),'type'),'handle')),
	cf(1,eq(attr(var(24),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(24),'TOP'),var(31))),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(attr(var(10),'PRED'),semform('Browne',10,[],[]))),
	cf(1,eq(attr(var(10),'CHECK'),var(32))),
	cf(1,eq(attr(var(10),'NTYPE'),var(33))),
	cf(1,eq(proj(var(10),'m::'),var(34))),
	cf(1,eq(attr(var(10),'CASE'),'obl')),
	cf(1,eq(attr(var(10),'DEF'),'+')),
	cf(1,eq(attr(var(10),'NUM'),'sg')),
	cf(1,eq(attr(var(10),'PERS'),'3')),
	cf(1,eq(attr(var(10),'REF'),'+')),
	cf(1,eq(attr(var(32),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(33),'NSEM'),var(35))),
	cf(1,eq(attr(var(33),'NSYN'),'proper')),
	cf(1,eq(attr(var(35),'PROPER'),var(36))),
	cf(1,eq(attr(var(36),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(34),'H-CONS'),var(37))),
	cf(1,eq(attr(var(34),'RELS'),var(38))),
	cf(1,eq(attr(var(34),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(34),'TOP'),var(39))),
	cf(1,eq(attr(var(34),'_ANTECEDVAR'),var(29))),
	cf(1,eq(attr(var(34),'_QUANT'),var(40))),
	cf(1,in_set(var(41),var(37))),
	cf(1,eq(attr(var(41),'OUTSCPD'),var(42))),
	cf(1,eq(attr(var(41),'SC_ARG'),var(43))),
	cf(1,eq(attr(var(41),'relation'),'qeq')),
	cf(1,eq(attr(var(42),'type'),'handle')),
	cf(1,eq(attr(var(43),'type'),'handle')),
	cf(1,in_set(var(5),var(38))),
	cf(1,in_set(var(6),var(38))),
	cf(1,eq(attr(var(5),'ARG0'),var(44))),
	cf(1,eq(attr(var(5),'LBL'),var(42))),
	cf(1,eq(attr(var(5),'CARG'),'Browne')),
	cf(1,eq(attr(var(5),'relation'),semform('named',11,[],[]))),
	cf(1,eq(attr(var(44),'NUM'),'sg')),
	cf(1,eq(attr(var(44),'PERS'),'3')),
	cf(1,eq(attr(var(44),'type'),'ref-ind')),
	cf(1,eq(attr(var(6),'ARG0'),var(44))),
	cf(1,eq(attr(var(6),'BODY'),var(45))),
	cf(1,eq(attr(var(6),'LBL'),var(39))),
	cf(1,eq(attr(var(6),'RSTR'),var(43))),
	cf(1,eq(attr(var(6),'relation'),semform('proper_q',12,[],[]))),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,eq(attr(var(40),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(40),'TOP'),var(46))),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,eq(attr(var(11),'PRED'),semform('sigarett',18,[],[]))),
	cf(1,eq(attr(var(11),'CHECK'),var(47))),
	cf(1,eq(attr(var(11),'GEND'),var(48))),
	cf(1,eq(attr(var(11),'NTYPE'),var(49))),
	cf(1,eq(proj(var(11),'m::'),var(50))),
	cf(1,eq(attr(var(11),'CASE'),'obl')),
	cf(1,eq(attr(var(11),'DEF'),'+')),
	cf(1,eq(attr(var(11),'NUM'),'sg')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(1,eq(attr(var(47),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(47),'_NOUN'),'+')),
	cf(1,eq(attr(var(47),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(48),'FEM'),'-')),
	cf(1,eq(attr(var(48),'MASC'),'+')),
	cf(1,eq(attr(var(48),'NEUT'),'-')),
	cf(1,eq(attr(var(49),'NSEM'),var(51))),
	cf(1,eq(attr(var(49),'NSYN'),'common')),
	cf(1,eq(attr(var(51),'COMMON'),'count')),
	cf(1,eq(attr(var(50),'H-CONS'),var(52))),
	cf(1,eq(attr(var(50),'RELS'),var(53))),
	cf(1,eq(attr(var(50),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(50),'TOP'),var(54))),
	cf(1,eq(attr(var(50),'_ANTECEDVAR'),var(29))),
	cf(1,eq(attr(var(50),'_QUANT'),var(55))),
	cf(1,in_set(var(56),var(52))),
	cf(1,eq(attr(var(56),'OUTSCPD'),var(57))),
	cf(1,eq(attr(var(56),'SC_ARG'),var(58))),
	cf(1,eq(attr(var(56),'relation'),'qeq')),
	cf(1,eq(attr(var(57),'type'),'handle')),
	cf(1,eq(attr(var(58),'type'),'handle')),
	cf(1,in_set(var(7),var(53))),
	cf(1,in_set(var(8),var(53))),
	cf(1,eq(attr(var(7),'ARG0'),var(59))),
	cf(1,eq(attr(var(7),'BODY'),var(60))),
	cf(1,eq(attr(var(7),'LBL'),var(54))),
	cf(1,eq(attr(var(7),'RSTR'),var(58))),
	cf(1,eq(attr(var(7),'relation'),semform('def_q',20,[],[]))),
	cf(1,eq(attr(var(59),'DIV'),'-')),
	cf(1,eq(attr(var(59),'GRIND'),'-')),
	cf(1,eq(attr(var(59),'NATGEND'),'gender')),
	cf(1,eq(attr(var(59),'NUM'),'sg')),
	cf(1,eq(attr(var(59),'PERS'),'3')),
	cf(1,eq(attr(var(59),'type'),'ref-ind')),
	cf(1,eq(attr(var(60),'type'),'handle')),
	cf(1,eq(attr(var(54),'type'),'handle')),
	cf(1,eq(attr(var(8),'ARG0'),var(59))),
	cf(1,eq(attr(var(8),'LBL'),var(57))),
	cf(1,eq(attr(var(8),'_CAT'),'n')),
	cf(1,eq(attr(var(8),'relation'),semform('sigarett',19,[],[]))),
	cf(1,eq(attr(var(55),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(55),'TOP'),var(61))),
	cf(1,eq(attr(var(61),'type'),'handle')),
	cf(1,eq(attr(var(12),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(13),'MOOD'),'indicative')),
	cf(1,eq(attr(var(13),'TENSE'),'past')),
	cf(1,eq(attr(var(14),'H-CONS'),var(21))),
	cf(1,eq(attr(var(14),'INDEX'),var(62))),
	cf(1,eq(attr(var(14),'RELS'),var(63))),
	cf(1,eq(attr(var(14),'RELS_EL'),var(64))),
	cf(1,eq(attr(var(14),'TOP'),var(28))),
	cf(1,eq(attr(var(14),'_MSG'),var(65))),
	cf(1,eq(attr(var(14),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(62),'PERF'),'-')),
	cf(1,eq(attr(var(62),'SF'),'prop')),
	cf(1,eq(attr(var(62),'TENSE'),'past')),
	cf(1,eq(attr(var(62),'type'),'event')),
	cf(1,in_set(var(64),var(63))),
	cf(1,eq(attr(var(64),'ARG0'),var(62))),
	cf(1,eq(attr(var(64),'ARG1'),var(29))),
	cf(1,eq(attr(var(64),'ARG2'),var(59))),
	cf(1,eq(attr(var(64),'ARG3'),var(44))),
	cf(1,eq(attr(var(64),'LBL'),var(27))),
	cf(1,eq(attr(var(64),'_CAT'),'v')),
	cf(1,eq(attr(var(64),'relation'),semform('rekke-hand',5,[],[]))),
	cf(1,eq(attr(var(65),'ARG0'),var(62))),
	cf(1,eq(attr(var(65),'LBL'),var(28))),
	cf(1,eq(proj(var(66),'m::'),var(67))),
	cf(1,eq(proj(var(68),'m::'),var(67))),
	cf(1,eq(proj(var(69),'m::'),var(70))),
	cf(1,eq(proj(var(71),'m::'),var(70)))
	],
	% C-Structure:
	[
	cf(1,subtree(1141,'ROOT',1165,81)),
	cf(1,phi(1141,var(0))),
	cf(1,subtree(1165,'ROOT',-,1817)),
	cf(1,phi(1165,var(0))),
	cf(1,subtree(1817,'IP',1225,1815)),
	cf(1,phi(1817,var(0))),
	cf(1,subtree(1225,'IP',-,254)),
	cf(1,phi(1225,var(0))),
	cf(1,subtree(254,'PROPP',-,13)),
	cf(1,phi(254,var(9))),
	cf(1,subtree(13,'PROP',-,1)),
	cf(1,phi(13,var(9))),
	cf(1,terminal(1,'Abrams',[1])),
	cf(1,phi(1,var(72))),
	cf(1,subtree(1815,'I\'',1782,1789)),
	cf(1,phi(1815,var(0))),
	cf(1,subtree(1782,'I\'',-,1406)),
	cf(1,phi(1782,var(0))),
	cf(1,subtree(1406,'Vfin',1405,17)),
	cf(1,phi(1406,var(0))),
	cf(1,cproj(1406,var(66))),
	cf(1,subtree(1405,'Vfin',1404,36)),
	cf(1,phi(1405,var(0))),
	cf(1,cproj(1405,var(66))),
	cf(1,subtree(1404,'Vfin',-,40)),
	cf(1,phi(1404,var(0))),
	cf(1,cproj(1404,var(66))),
	cf(1,subtree(40,'V_BASE',-,39)),
	cf(1,phi(40,var(0))),
	cf(1,cproj(40,var(68))),
	cf(1,terminal(39,'rekke',[15])),
	cf(1,phi(39,var(73))),
	cf(1,cproj(39,var(68))),
	cf(1,subtree(36,'V_SUFF_BASE',-,37)),
	cf(1,phi(36,var(0))),
	cf(1,terminal(37,'+Verb',[15])),
	cf(1,phi(37,var(0))),
	cf(1,subtree(17,'V_SUFF_BASE',-,18)),
	cf(1,phi(17,var(0))),
	cf(1,terminal(18,'+Past',[15])),
	cf(1,phi(18,var(0))),
	cf(1,subtree(1789,'S',-,1792)),
	cf(1,phi(1789,var(0))),
	cf(1,subtree(1792,'VPmain',1805,995)),
	cf(1,phi(1792,var(0))),
	cf(1,subtree(1805,'VPmain',-,1077)),
	cf(1,phi(1805,var(0))),
	cf(1,subtree(1077,'PROPP',-,483)),
	cf(1,phi(1077,var(10))),
	cf(1,subtree(483,'PROP',482,48)),
	cf(1,phi(483,var(10))),
	cf(1,subtree(482,'PROP',479,50)),
	cf(1,phi(482,var(10))),
	cf(1,subtree(479,'PROP',-,52)),
	cf(1,phi(479,var(10))),
	cf(1,subtree(52,'PROP_BASE',-,53)),
	cf(1,phi(52,var(10))),
	cf(1,terminal(53,'Browne',[47])),
	cf(1,phi(53,var(10))),
	cf(1,subtree(50,'N_SUFF_BASE',-,51)),
	cf(1,phi(50,var(10))),
	cf(1,terminal(51,'+Prop',[47])),
	cf(1,phi(51,var(10))),
	cf(1,subtree(48,'N_SUFF_BASE',-,49)),
	cf(1,phi(48,var(10))),
	cf(1,terminal(49,'+Indef',[47])),
	cf(1,phi(49,var(10))),
	cf(1,subtree(995,'NP',-,56)),
	cf(1,phi(995,var(11))),
	cf(1,subtree(56,'N',475,58)),
	cf(1,phi(56,var(11))),
	cf(1,cproj(56,var(69))),
	cf(1,subtree(475,'N',474,60)),
	cf(1,phi(475,var(11))),
	cf(1,cproj(475,var(69))),
	cf(1,subtree(474,'N',473,62)),
	cf(1,phi(474,var(11))),
	cf(1,cproj(474,var(69))),
	cf(1,subtree(473,'N',445,64)),
	cf(1,phi(473,var(11))),
	cf(1,cproj(473,var(69))),
	cf(1,subtree(445,'N',-,66)),
	cf(1,phi(445,var(11))),
	cf(1,cproj(445,var(69))),
	cf(1,subtree(66,'N_BASE',-,67)),
	cf(1,phi(66,var(11))),
	cf(1,cproj(66,var(71))),
	cf(1,terminal(67,'sigarett',[57])),
	cf(1,phi(67,var(11))),
	cf(1,cproj(67,var(71))),
	cf(1,subtree(64,'N_SUFF_BASE',-,65)),
	cf(1,phi(64,var(11))),
	cf(1,terminal(65,'+Noun',[57])),
	cf(1,phi(65,var(11))),
	cf(1,subtree(62,'N_SUFF_BASE',-,63)),
	cf(1,phi(62,var(11))),
	cf(1,terminal(63,'+Masc',[57])),
	cf(1,phi(63,var(11))),
	cf(1,subtree(60,'N_SUFF_BASE',-,61)),
	cf(1,phi(60,var(11))),
	cf(1,terminal(61,'+Def',[57])),
	cf(1,phi(61,var(74))),
	cf(1,subtree(58,'N_SUFF_BASE',-,59)),
	cf(1,phi(58,var(11))),
	cf(1,terminal(59,'+Sg',[57])),
	cf(1,phi(59,var(11))),
	cf(1,subtree(81,'PERIOD',-,74)),
	cf(1,phi(81,var(0))),
	cf(1,terminal(74,'.',[74])),
	cf(1,phi(74,var(0))),
	cf(1,semform_data(0,13,1,7)),
	cf(1,semform_data(1,13,1,7)),
	cf(1,semform_data(2,13,1,7)),
	cf(1,semform_data(4,40,8,12)),
	cf(1,semform_data(5,40,8,12)),
	cf(1,semform_data(10,52,14,19)),
	cf(1,semform_data(11,52,14,19)),
	cf(1,semform_data(12,52,14,19)),
	cf(1,semform_data(18,66,21,28)),
	cf(1,semform_data(19,66,21,28)),
	cf(1,semform_data(20,60,31,31)),
	cf(1,fspan(var(0),1,32)),
	cf(1,fspan(var(9),1,7)),
	cf(1,fspan(var(11),21,32)),
	cf(1,fspan(var(10),14,20)),
	cf(1,surfaceform(1,'Abrams',1,7)),
	cf(1,surfaceform(15,'rakte',8,13)),
	cf(1,surfaceform(47,'Browne',14,20)),
	cf(1,surfaceform(57,'sigaretten',21,32)),
	cf(1,surfaceform(74,'.',31,32))
	]).
