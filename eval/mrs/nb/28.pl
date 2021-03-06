% -*- coding: iso-8859-1 -*-

fstructure('Abrams lurte på hvilken hund som bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.15 CPU seconds, 149 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('lure',4,[var(12),var(13)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(12))),
	cf(1,eq(attr(var(0),'COMP'),var(13))),
	cf(1,eq(attr(var(0),'TOPIC'),var(12))),
	cf(1,eq(attr(var(0),'CHECK'),var(14))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(15))),
	cf(1,eq(proj(var(0),'m::'),var(16))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(12),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(12),'CHECK'),var(17))),
	cf(1,eq(attr(var(12),'NTYPE'),var(18))),
	cf(1,eq(proj(var(12),'m::'),var(19))),
	cf(1,eq(attr(var(12),'CASE'),'nom')),
	cf(1,eq(attr(var(12),'DEF'),'+')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(12),'REF'),'+')),
	cf(1,eq(attr(var(17),'_SEL'),var(20))),
	cf(1,eq(attr(var(20),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(20),'_ANIM'),'+')),
	cf(1,eq(attr(var(20),'_HUMAN'),'+')),
	cf(1,eq(attr(var(18),'NSEM'),var(21))),
	cf(1,eq(attr(var(18),'NSYN'),'proper')),
	cf(1,eq(attr(var(21),'PROPER'),var(22))),
	cf(1,eq(attr(var(22),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(19),'H-CONS'),var(23))),
	cf(1,eq(attr(var(19),'RELS'),var(24))),
	cf(1,eq(attr(var(19),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(19),'TOP'),var(25))),
	cf(1,eq(attr(var(19),'_QUANT'),var(26))),
	cf(1,in_set(var(1),var(23))),
	cf(1,in_set(var(2),var(23))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(27))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(28))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(29))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(30))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,in_set(var(3),var(24))),
	cf(1,in_set(var(4),var(24))),
	cf(1,eq(attr(var(3),'ARG0'),var(31))),
	cf(1,eq(attr(var(3),'LBL'),var(27))),
	cf(1,eq(attr(var(3),'CARG'),'Abrams')),
	cf(1,eq(attr(var(3),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(31),'NATGEND'),'animate')),
	cf(1,eq(attr(var(31),'NUM'),'sg')),
	cf(1,eq(attr(var(31),'PERS'),'3')),
	cf(1,eq(attr(var(31),'type'),'ref-ind')),
	cf(1,eq(attr(var(4),'ARG0'),var(31))),
	cf(1,eq(attr(var(4),'BODY'),var(32))),
	cf(1,eq(attr(var(4),'LBL'),var(25))),
	cf(1,eq(attr(var(4),'RSTR'),var(28))),
	cf(1,eq(attr(var(4),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(26),'TOP'),var(33))),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(13),'PRED'),semform('bjeffe',11,[var(34)],[]))),
	cf(1,eq(attr(var(13),'SUBJ'),var(34))),
	cf(1,eq(attr(var(13),'CHECK'),var(35))),
	cf(1,eq(attr(var(13),'FOCUS-INT'),var(34))),
	cf(1,eq(attr(var(13),'TNS-ASP'),var(36))),
	cf(1,eq(proj(var(13),'m::'),var(37))),
	cf(1,eq(attr(var(13),'CASE'),'obl')),
	cf(1,eq(attr(var(13),'CLAUSE-TYPE'),'wh-int')),
	cf(1,eq(attr(var(13),'COMP-FORM'),'som')),
	cf(1,eq(attr(var(13),'TOPCP'),'-')),
	cf(1,eq(attr(var(13),'VFORM'),'fin')),
	cf(1,eq(attr(var(13),'VTYPE'),'main')),
	cf(1,eq(attr(var(34),'PRED'),semform('hund',9,[],[]))),
	cf(1,eq(attr(var(34),'CHECK'),var(38))),
	cf(1,eq(attr(var(34),'GEND'),var(39))),
	cf(1,eq(attr(var(34),'NTYPE'),var(40))),
	cf(1,eq(attr(var(34),'SPEC'),var(41))),
	cf(1,eq(proj(var(34),'m::'),var(42))),
	cf(1,eq(attr(var(34),'DEF'),'-')),
	cf(1,eq(attr(var(34),'NUM'),'sg')),
	cf(1,eq(attr(var(34),'PERS'),'3')),
	cf(1,eq(attr(var(34),'REF'),'+')),
	cf(1,eq(attr(var(38),'_SEL'),var(43))),
	cf(1,eq(attr(var(38),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(38),'_NOUN'),'+')),
	cf(1,eq(attr(var(38),'_PREDEF'),'+')),
	cf(1,eq(attr(var(38),'_PREDET'),'+')),
	cf(1,eq(attr(var(43),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(43),'_ANIM'),'+')),
	cf(1,eq(attr(var(43),'_HUMAN'),'-')),
	cf(1,eq(attr(var(39),'FEM'),'-')),
	cf(1,eq(attr(var(39),'MASC'),'+')),
	cf(1,eq(attr(var(39),'NEUT'),'-')),
	cf(1,eq(attr(var(40),'NSEM'),var(44))),
	cf(1,eq(attr(var(40),'NSYN'),'common')),
	cf(1,eq(attr(var(44),'COMMON'),'count')),
	cf(1,eq(attr(var(41),'DET'),var(45))),
	cf(1,eq(attr(var(45),'DET-FORM'),'hvilken')),
	cf(1,eq(attr(var(45),'DET-TYPE'),'int')),
	cf(1,eq(attr(var(42),'H-CONS'),var(46))),
	cf(1,eq(attr(var(42),'RELS'),var(47))),
	cf(1,eq(attr(var(42),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(42),'TOP'),var(48))),
	cf(1,eq(attr(var(42),'_QUANT'),var(49))),
	cf(1,in_set(var(5),var(46))),
	cf(1,in_set(var(6),var(46))),
	cf(1,eq(attr(var(5),'OUTSCPD'),var(50))),
	cf(1,eq(attr(var(5),'SC_ARG'),var(51))),
	cf(1,eq(attr(var(5),'relation'),'qeq')),
	cf(1,eq(attr(var(50),'type'),'handle')),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(6),'OUTSCPD'),var(52))),
	cf(1,eq(attr(var(6),'SC_ARG'),var(53))),
	cf(1,eq(attr(var(6),'relation'),'qeq')),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(1,eq(attr(var(53),'type'),'handle')),
	cf(1,in_set(var(7),var(47))),
	cf(1,in_set(var(8),var(47))),
	cf(1,eq(attr(var(7),'ARG0'),var(54))),
	cf(1,eq(attr(var(7),'BODY'),var(55))),
	cf(1,eq(attr(var(7),'LBL'),var(48))),
	cf(1,eq(attr(var(7),'RSTR'),var(51))),
	cf(1,eq(attr(var(7),'relation'),semform('hvilken_q',8,[],[]))),
	cf(1,eq(attr(var(54),'DIV'),'-')),
	cf(1,eq(attr(var(54),'GRIND'),'-')),
	cf(1,eq(attr(var(54),'NATGEND'),'gender')),
	cf(1,eq(attr(var(54),'NUM'),'sg')),
	cf(1,eq(attr(var(54),'PERS'),'3')),
	cf(1,eq(attr(var(54),'type'),'ref-ind')),
	cf(1,eq(attr(var(55),'type'),'handle')),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(8),'ARG0'),var(54))),
	cf(1,eq(attr(var(8),'LBL'),var(50))),
	cf(1,eq(attr(var(8),'_CAT'),'n')),
	cf(1,eq(attr(var(8),'relation'),semform('hund',10,[],[]))),
	cf(1,eq(attr(var(49),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(49),'TOP'),var(56))),
	cf(1,eq(attr(var(56),'type'),'handle')),
	cf(1,eq(attr(var(35),'_MAIN-CL'),'-')),
	cf(1,eq(attr(var(35),'_P-SELFORM'),'på')),
	cf(1,eq(attr(var(35),'_PREPEXISTS'),'+')),
	cf(1,eq(attr(var(35),'_UNIQUEOCCURRENCE'),'+_')),
	cf(1,eq(attr(var(36),'MOOD'),'indicative')),
	cf(1,eq(attr(var(36),'TENSE'),'past')),
	cf(1,eq(attr(var(37),'H-CONS'),var(46))),
	cf(1,eq(attr(var(37),'INDEX'),var(57))),
	cf(1,eq(attr(var(37),'RELS'),var(58))),
	cf(1,eq(attr(var(37),'RELS_EL'),var(10))),
	cf(1,eq(attr(var(37),'TOP'),var(53))),
	cf(1,eq(attr(var(37),'_MSG'),var(59))),
	cf(1,eq(attr(var(37),'_MSGQEQ'),var(6))),
	cf(1,eq(attr(var(57),'PERF'),'-')),
	cf(1,eq(attr(var(57),'SF'),'ques')),
	cf(1,eq(attr(var(57),'TENSE'),'past')),
	cf(1,eq(attr(var(57),'type'),'event')),
	cf(1,in_set(var(9),var(58))),
	cf(1,in_set(var(10),var(58))),
	cf(1,eq(attr(var(9),'ARG0'),var(60))),
	cf(1,eq(attr(var(9),'ARG1'),var(61))),
	cf(1,eq(attr(var(9),'ARG2'),var(53))),
	cf(1,eq(attr(var(9),'LBL'),var(29))),
	cf(1,eq(attr(var(9),'_CAT'),'p')),
	cf(1,eq(attr(var(9),'_CATSUFF'),'sel')),
	cf(1,eq(attr(var(9),'relation'),semform('på',7,[],[]))),
	cf(1,eq(attr(var(60),'type'),'event')),
	cf(1,eq(attr(var(61),'PERF'),'-')),
	cf(1,eq(attr(var(61),'SF'),'prop')),
	cf(1,eq(attr(var(61),'TENSE'),'past')),
	cf(1,eq(attr(var(61),'type'),'event')),
	cf(1,eq(attr(var(10),'ARG0'),var(57))),
	cf(1,eq(attr(var(10),'ARG1'),var(54))),
	cf(1,eq(attr(var(10),'LBL'),var(52))),
	cf(1,eq(attr(var(10),'_CAT'),'v')),
	cf(1,eq(attr(var(10),'relation'),semform('bjeffe',12,[],[]))),
	cf(1,eq(attr(var(59),'ARG0'),var(57))),
	cf(1,eq(attr(var(59),'LBL'),var(53))),
	cf(1,eq(attr(var(14),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(15),'MOOD'),'indicative')),
	cf(1,eq(attr(var(15),'TENSE'),'past')),
	cf(1,eq(attr(var(16),'H-CONS'),var(23))),
	cf(1,eq(attr(var(16),'INDEX'),var(61))),
	cf(1,eq(attr(var(16),'RELS'),var(62))),
	cf(1,eq(attr(var(16),'RELS_EL'),var(11))),
	cf(1,eq(attr(var(16),'TOP'),var(30))),
	cf(1,eq(attr(var(16),'_MSG'),var(63))),
	cf(1,eq(attr(var(16),'_MSGQEQ'),var(2))),
	cf(1,in_set(var(9),var(62))),
	cf(1,in_set(var(11),var(62))),
	cf(1,eq(attr(var(11),'ARG0'),var(61))),
	cf(1,eq(attr(var(11),'ARG1'),var(31))),
	cf(1,eq(attr(var(11),'ARG2'),var(53))),
	cf(1,eq(attr(var(11),'LBL'),var(29))),
	cf(1,eq(attr(var(11),'_CAT'),'v')),
	cf(1,eq(attr(var(11),'_PRT'),'på')),
	cf(1,eq(attr(var(11),'relation'),semform('lure',5,[],[]))),
	cf(1,eq(attr(var(63),'ARG0'),var(61))),
	cf(1,eq(attr(var(63),'LBL'),var(30))),
	cf(1,eq(proj(var(64),'m::'),var(65))),
	cf(1,eq(proj(var(66),'m::'),var(65))),
	cf(1,eq(proj(var(67),'m::'),var(68))),
	cf(1,eq(proj(var(69),'m::'),var(68))),
	cf(1,eq(proj(var(70),'m::'),var(71))),
	cf(1,eq(proj(var(72),'m::'),var(71))),
	cf(1,eq(attr(var(73),'PRED'),semform('på',6,[var(13)],[]))),
	cf(1,eq(attr(var(73),'OBJ'),var(13))),
	cf(1,eq(attr(var(73),'CHECK'),var(74))),
	cf(1,eq(proj(var(73),'m::'),var(75))),
	cf(1,eq(attr(var(73),'PFORM'),'på')),
	cf(1,eq(attr(var(73),'PTYPE'),'sem')),
	cf(1,eq(attr(var(74),'_ANTECED'),var(76))),
	cf(1,eq(attr(var(74),'_MOVED-OBJ'),'-')),
	cf(1,eq(proj(var(76),'m::'),var(77))),
	cf(1,eq(attr(var(76),'NUM'),'sg')),
	cf(1,eq(attr(var(76),'PERS'),'3')),
	cf(1,eq(attr(var(77),'ARG0'),var(31))),
	cf(1,eq(attr(var(75),'RELS'),var(58))),
	cf(1,eq(attr(var(75),'RELS_EL'),var(9)))
	],
	% C-Structure:
	[
	cf(1,subtree(932,'ROOT',973,158)),
	cf(1,phi(932,var(0))),
	cf(1,subtree(973,'ROOT',-,1555)),
	cf(1,phi(973,var(0))),
	cf(1,subtree(1555,'IP',2669,3850)),
	cf(1,phi(1555,var(0))),
	cf(1,subtree(2669,'IP',-,331)),
	cf(1,phi(2669,var(0))),
	cf(1,subtree(331,'PROPP',-,13)),
	cf(1,phi(331,var(12))),
	cf(1,subtree(13,'PROP',-,1)),
	cf(1,phi(13,var(12))),
	cf(1,terminal(1,'Abrams',[1])),
	cf(1,phi(1,var(78))),
	cf(1,subtree(3850,'I\'',3594,3599)),
	cf(1,phi(3850,var(0))),
	cf(1,subtree(3594,'I\'',-,2945)),
	cf(1,phi(3594,var(0))),
	cf(1,subtree(2945,'Vfin',2944,17)),
	cf(1,phi(2945,var(0))),
	cf(1,cproj(2945,var(64))),
	cf(1,subtree(2944,'Vfin',2943,19)),
	cf(1,phi(2944,var(0))),
	cf(1,cproj(2944,var(64))),
	cf(1,subtree(2943,'Vfin',-,21)),
	cf(1,phi(2943,var(0))),
	cf(1,cproj(2943,var(64))),
	cf(1,subtree(21,'V_BASE',-,22)),
	cf(1,phi(21,var(0))),
	cf(1,cproj(21,var(66))),
	cf(1,terminal(22,'lure',[15])),
	cf(1,phi(22,var(0))),
	cf(1,cproj(22,var(66))),
	cf(1,subtree(19,'V_SUFF_BASE',-,20)),
	cf(1,phi(19,var(0))),
	cf(1,terminal(20,'+Verb',[15])),
	cf(1,phi(20,var(0))),
	cf(1,subtree(17,'V_SUFF_BASE',-,18)),
	cf(1,phi(17,var(0))),
	cf(1,terminal(18,'+Past',[15])),
	cf(1,phi(18,var(0))),
	cf(1,subtree(3599,'S',-,3611)),
	cf(1,phi(3599,var(0))),
	cf(1,subtree(3611,'VPmain',-,2622)),
	cf(1,phi(3611,var(0))),
	cf(1,subtree(2622,'PPsel2',2589,2619)),
	cf(1,phi(2622,var(73))),
	cf(1,subtree(2589,'PPsel2',-,54)),
	cf(1,phi(2589,var(73))),
	cf(1,subtree(54,'Psel2',-,36)),
	cf(1,phi(54,var(73))),
	cf(1,terminal(36,'på',[36])),
	cf(1,phi(36,var(79))),
	cf(1,subtree(2619,'WhP',2626,955)),
	cf(1,phi(2619,var(13))),
	cf(1,subtree(2626,'WhP',-,2623)),
	cf(1,phi(2626,var(13))),
	cf(1,subtree(2623,'DPint',2607,2040)),
	cf(1,phi(2623,var(34))),
	cf(1,subtree(2607,'DPint',-,64)),
	cf(1,phi(2607,var(34))),
	cf(1,subtree(64,'Dint',-,55)),
	cf(1,phi(64,var(34))),
	cf(1,terminal(55,'hvilken',[55])),
	cf(1,phi(55,var(80))),
	cf(1,subtree(2040,'NP',-,90)),
	cf(1,phi(2040,var(34))),
	cf(1,subtree(90,'N',1672,74)),
	cf(1,phi(90,var(34))),
	cf(1,cproj(90,var(67))),
	cf(1,subtree(1672,'N',1671,76)),
	cf(1,phi(1672,var(34))),
	cf(1,cproj(1672,var(67))),
	cf(1,subtree(1671,'N',1670,78)),
	cf(1,phi(1671,var(34))),
	cf(1,cproj(1671,var(67))),
	cf(1,subtree(1670,'N',1642,80)),
	cf(1,phi(1670,var(34))),
	cf(1,cproj(1670,var(67))),
	cf(1,subtree(1642,'N',-,82)),
	cf(1,phi(1642,var(34))),
	cf(1,cproj(1642,var(67))),
	cf(1,subtree(82,'N_BASE',-,83)),
	cf(1,phi(82,var(34))),
	cf(1,cproj(82,var(69))),
	cf(1,terminal(83,'hund',[73])),
	cf(1,phi(83,var(34))),
	cf(1,cproj(83,var(69))),
	cf(1,subtree(80,'N_SUFF_BASE',-,81)),
	cf(1,phi(80,var(34))),
	cf(1,terminal(81,'+Noun',[73])),
	cf(1,phi(81,var(34))),
	cf(1,subtree(78,'N_SUFF_BASE',-,79)),
	cf(1,phi(78,var(34))),
	cf(1,terminal(79,'+Masc',[73])),
	cf(1,phi(79,var(34))),
	cf(1,subtree(76,'N_SUFF_BASE',-,77)),
	cf(1,phi(76,var(34))),
	cf(1,terminal(77,'+Indef',[73])),
	cf(1,phi(77,var(34))),
	cf(1,subtree(74,'N_SUFF_BASE',-,75)),
	cf(1,phi(74,var(34))),
	cf(1,terminal(75,'+Sg',[73])),
	cf(1,phi(75,var(34))),
	cf(1,subtree(955,'CPrel',493,953)),
	cf(1,phi(955,var(13))),
	cf(1,subtree(493,'CPrel',-,110)),
	cf(1,phi(493,var(13))),
	cf(1,subtree(110,'Crel',-,91)),
	cf(1,phi(110,var(13))),
	cf(1,terminal(91,'som',[91])),
	cf(1,phi(91,var(13))),
	cf(1,subtree(953,'Ssub',-,539)),
	cf(1,phi(953,var(13))),
	cf(1,subtree(539,'VPfin',-,535)),
	cf(1,phi(539,var(13))),
	cf(1,subtree(535,'Vfin',534,114)),
	cf(1,phi(535,var(13))),
	cf(1,cproj(535,var(70))),
	cf(1,subtree(534,'Vfin',533,116)),
	cf(1,phi(534,var(13))),
	cf(1,cproj(534,var(70))),
	cf(1,subtree(533,'Vfin',-,118)),
	cf(1,phi(533,var(13))),
	cf(1,cproj(533,var(70))),
	cf(1,subtree(118,'V_BASE',-,119)),
	cf(1,phi(118,var(13))),
	cf(1,cproj(118,var(72))),
	cf(1,terminal(119,'bjeffe',[113])),
	cf(1,phi(119,var(13))),
	cf(1,cproj(119,var(72))),
	cf(1,subtree(116,'V_SUFF_BASE',-,117)),
	cf(1,phi(116,var(13))),
	cf(1,terminal(117,'+Verb',[113])),
	cf(1,phi(117,var(13))),
	cf(1,subtree(114,'V_SUFF_BASE',-,115)),
	cf(1,phi(114,var(13))),
	cf(1,terminal(115,'+Past',[113])),
	cf(1,phi(115,var(13))),
	cf(1,subtree(158,'PERIOD',-,151)),
	cf(1,phi(158,var(0))),
	cf(1,terminal(151,'.',[151])),
	cf(1,phi(151,var(0))),
	cf(1,semform_data(0,13,1,7)),
	cf(1,semform_data(1,13,1,7)),
	cf(1,semform_data(2,13,1,7)),
	cf(1,semform_data(4,21,8,11)),
	cf(1,semform_data(5,21,8,11)),
	cf(1,semform_data(6,54,14,16)),
	cf(1,semform_data(7,54,14,16)),
	cf(1,semform_data(8,64,17,24)),
	cf(1,semform_data(9,82,25,28)),
	cf(1,semform_data(10,82,25,28)),
	cf(1,semform_data(11,118,34,41)),
	cf(1,semform_data(12,118,34,41)),
	cf(1,fspan(var(0),1,42)),
	cf(1,fspan(var(12),1,7)),
	cf(1,fspan(var(73),14,42)),
	cf(1,fspan(var(13),17,42)),
	cf(1,fspan(var(34),17,29)),
	cf(1,surfaceform(1,'Abrams',1,7)),
	cf(1,surfaceform(15,'lurte',8,13)),
	cf(1,surfaceform(36,'på',14,16)),
	cf(1,surfaceform(55,'hvilken',17,24)),
	cf(1,surfaceform(73,'hund',25,29)),
	cf(1,surfaceform(91,'som',30,33)),
	cf(1,surfaceform(113,'bjeffet',34,42)),
	cf(1,surfaceform(151,'.',41,42))
	]).

