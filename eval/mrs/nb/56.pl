% -*- coding: iso-8859-1 -*-

fstructure('Hunden bjeffet hver dag.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.10 CPU seconds, 227 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',4,[var(9)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(9))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(10))),
	cf(1,eq(attr(var(0),'TOPIC'),var(9))),
	cf(1,eq(attr(var(0),'CHECK'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(12))),
	cf(1,eq(proj(var(0),'m::'),var(13))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(9),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(14))),
	cf(1,eq(attr(var(9),'GEND'),var(15))),
	cf(1,eq(attr(var(9),'NTYPE'),var(16))),
	cf(1,eq(proj(var(9),'m::'),var(17))),
	cf(1,eq(attr(var(9),'CASE'),'nom')),
	cf(1,eq(attr(var(9),'DEF'),'+')),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(14),'_SEL'),var(18))),
	cf(1,eq(attr(var(14),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(14),'_NOUN'),'+')),
	cf(1,eq(attr(var(18),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(18),'_ANIM'),'+')),
	cf(1,eq(attr(var(18),'_HUMAN'),'-')),
	cf(1,eq(attr(var(15),'FEM'),'-')),
	cf(1,eq(attr(var(15),'MASC'),'+')),
	cf(1,eq(attr(var(15),'NEUT'),'-')),
	cf(1,eq(attr(var(16),'NSEM'),var(19))),
	cf(1,eq(attr(var(16),'NSYN'),'common')),
	cf(1,eq(attr(var(19),'COMMON'),'count')),
	cf(1,eq(attr(var(17),'H-CONS'),var(20))),
	cf(1,eq(attr(var(17),'RELS'),var(21))),
	cf(1,eq(attr(var(17),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(17),'TOP'),var(22))),
	cf(1,eq(attr(var(17),'_QUANT'),var(23))),
	cf(1,in_set(var(1),var(20))),
	cf(1,in_set(var(2),var(20))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(24))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(25))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(26))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(27))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,in_set(var(3),var(21))),
	cf(1,in_set(var(4),var(21))),
	cf(1,eq(attr(var(3),'ARG0'),var(28))),
	cf(1,eq(attr(var(3),'BODY'),var(29))),
	cf(1,eq(attr(var(3),'LBL'),var(22))),
	cf(1,eq(attr(var(3),'RSTR'),var(25))),
	cf(1,eq(attr(var(3),'relation'),semform('def_q',3,[],[]))),
	cf(1,eq(attr(var(28),'DIV'),'-')),
	cf(1,eq(attr(var(28),'GRIND'),'-')),
	cf(1,eq(attr(var(28),'NATGEND'),'gender')),
	cf(1,eq(attr(var(28),'NUM'),'sg')),
	cf(1,eq(attr(var(28),'PERS'),'3')),
	cf(1,eq(attr(var(28),'type'),'ref-ind')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(28))),
	cf(1,eq(attr(var(4),'LBL'),var(24))),
	cf(1,eq(attr(var(4),'_CAT'),'n')),
	cf(1,eq(attr(var(4),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(23),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(23),'TOP'),var(30))),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,in_set(var(31),var(10))),
	cf(1,eq(attr(var(31),'PRED'),semform('dag',9,[],[]))),
	cf(1,eq(attr(var(31),'CHECK'),var(32))),
	cf(1,eq(attr(var(31),'GEND'),var(33))),
	cf(1,eq(attr(var(31),'NTYPE'),var(34))),
	cf(1,eq(attr(var(31),'SPEC'),var(35))),
	cf(1,eq(proj(var(31),'m::'),var(36))),
	cf(1,eq(attr(var(31),'CASE'),'nom')),
	cf(1,eq(attr(var(31),'NUM'),'sg')),
	cf(1,eq(attr(var(31),'PERS'),'3')),
	cf(1,eq(attr(var(31),'REF'),'+')),
	cf(1,eq(attr(var(32),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(32),'_NOUN'),'+')),
	cf(1,eq(attr(var(32),'_PREDET'),'+')),
	cf(1,eq(attr(var(32),'_PREQ'),'+')),
	cf(1,eq(attr(var(33),'FEM'),'-')),
	cf(1,eq(attr(var(33),'MASC'),'+')),
	cf(1,eq(attr(var(33),'NEUT'),'-')),
	cf(1,eq(attr(var(34),'NSEM'),var(37))),
	cf(1,eq(attr(var(34),'NSYN'),'common')),
	cf(1,eq(attr(var(37),'TIME'),var(38))),
	cf(1,eq(attr(var(37),'COMMON'),'count')),
	cf(1,eq(attr(var(38),'TEMPNOUN'),'+')),
	cf(1,eq(attr(var(35),'QUANT'),var(39))),
	cf(1,eq(attr(var(39),'PRED'),semform('hver',6,[],[]))),
	cf(1,eq(attr(var(39),'QUANT-TYPE'),'universal')),
	cf(1,eq(attr(var(36),'H-CONS'),var(40))),
	cf(1,eq(attr(var(36),'RELS'),var(41))),
	cf(1,eq(attr(var(36),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(36),'TOP'),var(42))),
	cf(1,eq(attr(var(36),'_QUANT'),var(43))),
	cf(1,in_set(var(44),var(40))),
	cf(1,eq(attr(var(44),'OUTSCPD'),var(45))),
	cf(1,eq(attr(var(44),'SC_ARG'),var(46))),
	cf(1,eq(attr(var(44),'relation'),'qeq')),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,in_set(var(5),var(41))),
	cf(1,in_set(var(6),var(41))),
	cf(1,in_set(var(7),var(41))),
	cf(1,in_set(var(8),var(41))),
	cf(1,eq(attr(var(5),'ARG0'),var(47))),
	cf(1,eq(attr(var(5),'ARG1'),var(48))),
	cf(1,eq(attr(var(5),'ARG2'),var(49))),
	cf(1,eq(attr(var(5),'LBL'),var(26))),
	cf(1,eq(attr(var(5),'relation'),semform('unspec_loc',8,[],[]))),
	cf(1,eq(attr(var(47),'type'),'event')),
	cf(1,eq(attr(var(48),'PERF'),'-')),
	cf(1,eq(attr(var(48),'SF'),'prop')),
	cf(1,eq(attr(var(48),'TENSE'),'past')),
	cf(1,eq(attr(var(48),'type'),'event')),
	cf(1,eq(attr(var(49),'DIV'),'-')),
	cf(1,eq(attr(var(49),'GRIND'),'-')),
	cf(1,eq(attr(var(49),'NATGEND'),'gender')),
	cf(1,eq(attr(var(49),'NUM'),'sg')),
	cf(1,eq(attr(var(49),'PERS'),'3')),
	cf(1,eq(attr(var(49),'type'),'ref-ind')),
	cf(1,eq(attr(var(6),'ARG0'),var(49))),
	cf(1,eq(attr(var(6),'LBL'),var(45))),
	cf(1,eq(attr(var(6),'_CAT'),'n')),
	cf(1,eq(attr(var(6),'relation'),semform('dag',10,[],[]))),
	cf(1,eq(attr(var(7),'ARG0'),var(48))),
	cf(1,eq(attr(var(7),'ARG1'),var(28))),
	cf(1,eq(attr(var(7),'LBL'),var(26))),
	cf(1,eq(attr(var(7),'_CAT'),'v')),
	cf(1,eq(attr(var(7),'relation'),semform('bjeffe',5,[],[]))),
	cf(1,eq(attr(var(8),'ARG0'),var(49))),
	cf(1,eq(attr(var(8),'BODY'),var(50))),
	cf(1,eq(attr(var(8),'LBL'),var(42))),
	cf(1,eq(attr(var(8),'RSTR'),var(46))),
	cf(1,eq(attr(var(8),'relation'),semform('hver_q',7,[],[]))),
	cf(1,eq(attr(var(50),'type'),'handle')),
	cf(1,eq(attr(var(42),'type'),'handle')),
	cf(1,eq(attr(var(43),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(43),'TOP'),var(51))),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(11),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(12),'MOOD'),'indicative')),
	cf(1,eq(attr(var(12),'TENSE'),'past')),
	cf(1,eq(attr(var(13),'H-CONS'),var(20))),
	cf(1,eq(attr(var(13),'INDEX'),var(48))),
	cf(1,eq(attr(var(13),'RELS'),var(41))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(13),'TOP'),var(27))),
	cf(1,eq(attr(var(13),'_MSG'),var(52))),
	cf(1,eq(attr(var(13),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(52),'ARG0'),var(48))),
	cf(1,eq(attr(var(52),'LBL'),var(27))),
	cf(1,eq(proj(var(53),'m::'),var(54))),
	cf(1,eq(proj(var(55),'m::'),var(54))),
	cf(1,eq(proj(var(56),'m::'),var(57))),
	cf(1,eq(proj(var(58),'m::'),var(57))),
	cf(1,eq(proj(var(59),'m::'),var(60))),
	cf(1,eq(proj(var(61),'m::'),var(60)))
	],
	% C-Structure:
	[
	cf(1,subtree(882,'ROOT',921,121)),
	cf(1,phi(882,var(0))),
	cf(1,subtree(921,'ROOT',-,1619)),
	cf(1,phi(921,var(0))),
	cf(1,subtree(1619,'IP',2123,2289)),
	cf(1,phi(1619,var(0))),
	cf(1,subtree(2123,'IP',-,2344)),
	cf(1,phi(2123,var(0))),
	cf(1,subtree(2344,'NP',-,2)),
	cf(1,phi(2344,var(9))),
	cf(1,subtree(2,'N',317,4)),
	cf(1,phi(2,var(9))),
	cf(1,cproj(2,var(53))),
	cf(1,subtree(317,'N',313,6)),
	cf(1,phi(317,var(9))),
	cf(1,cproj(317,var(53))),
	cf(1,subtree(313,'N',309,8)),
	cf(1,phi(313,var(9))),
	cf(1,cproj(313,var(53))),
	cf(1,subtree(309,'N',306,10)),
	cf(1,phi(309,var(9))),
	cf(1,cproj(309,var(53))),
	cf(1,subtree(306,'N',-,12)),
	cf(1,phi(306,var(9))),
	cf(1,cproj(306,var(53))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(9))),
	cf(1,cproj(12,var(55))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(9))),
	cf(1,cproj(13,var(55))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(9))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(9))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(9))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(9))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(9))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(62))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(9))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(9))),
	cf(1,subtree(2289,'I\'',2231,2238)),
	cf(1,phi(2289,var(0))),
	cf(1,subtree(2231,'I\'',-,385)),
	cf(1,phi(2231,var(0))),
	cf(1,subtree(385,'Vfin',384,22)),
	cf(1,phi(385,var(0))),
	cf(1,cproj(385,var(56))),
	cf(1,subtree(384,'Vfin',383,24)),
	cf(1,phi(384,var(0))),
	cf(1,cproj(384,var(56))),
	cf(1,subtree(383,'Vfin',-,26)),
	cf(1,phi(383,var(0))),
	cf(1,cproj(383,var(56))),
	cf(1,subtree(26,'V_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,cproj(26,var(58))),
	cf(1,terminal(27,'bjeffe',[20])),
	cf(1,phi(27,var(0))),
	cf(1,cproj(27,var(58))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(25,'+Verb',[20])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(23,'+Past',[20])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(2238,'S',-,2244)),
	cf(1,phi(2238,var(0))),
	cf(1,subtree(2244,'VPmain',-,1428)),
	cf(1,phi(2244,var(0))),
	cf(1,subtree(1428,'ALLQP',1424,1099)),
	cf(1,phi(1428,var(31))),
	cf(1,subtree(1424,'ALLQP',-,81)),
	cf(1,phi(1424,var(31))),
	cf(1,subtree(81,'ALLQ',-,59)),
	cf(1,phi(81,var(31))),
	cf(1,terminal(59,'hver',[59])),
	cf(1,phi(59,var(63))),
	cf(1,subtree(1099,'NP',-,83)),
	cf(1,phi(1099,var(31))),
	cf(1,subtree(83,'N',1026,85)),
	cf(1,phi(83,var(31))),
	cf(1,cproj(83,var(59))),
	cf(1,subtree(1026,'N',1025,87)),
	cf(1,phi(1026,var(31))),
	cf(1,cproj(1026,var(59))),
	cf(1,subtree(1025,'N',1024,89)),
	cf(1,phi(1025,var(31))),
	cf(1,cproj(1025,var(59))),
	cf(1,subtree(1024,'N',988,91)),
	cf(1,phi(1024,var(31))),
	cf(1,cproj(1024,var(59))),
	cf(1,subtree(988,'N',-,93)),
	cf(1,phi(988,var(31))),
	cf(1,cproj(988,var(59))),
	cf(1,subtree(93,'N_BASE',-,94)),
	cf(1,phi(93,var(31))),
	cf(1,cproj(93,var(61))),
	cf(1,terminal(94,'dag',[84])),
	cf(1,phi(94,var(31))),
	cf(1,cproj(94,var(61))),
	cf(1,subtree(91,'N_SUFF_BASE',-,92)),
	cf(1,phi(91,var(31))),
	cf(1,terminal(92,'+Noun',[84])),
	cf(1,phi(92,var(31))),
	cf(1,subtree(89,'N_SUFF_BASE',-,90)),
	cf(1,phi(89,var(31))),
	cf(1,terminal(90,'+Masc',[84])),
	cf(1,phi(90,var(31))),
	cf(1,subtree(87,'N_SUFF_BASE',-,88)),
	cf(1,phi(87,var(31))),
	cf(1,terminal(88,'+Indef',[84])),
	cf(1,phi(88,var(31))),
	cf(1,subtree(85,'N_SUFF_BASE',-,86)),
	cf(1,phi(85,var(31))),
	cf(1,terminal(86,'+Sg',[84])),
	cf(1,phi(86,var(31))),
	cf(1,subtree(121,'PERIOD',-,114)),
	cf(1,phi(121,var(0))),
	cf(1,terminal(114,'.',[114])),
	cf(1,phi(114,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(1,semform_data(4,26,8,15)),
	cf(1,semform_data(5,26,8,15)),
	cf(1,semform_data(6,81,16,20)),
	cf(1,semform_data(7,81,16,20)),
	cf(1,semform_data(8,2244,16,25)),
	cf(1,semform_data(9,93,21,23)),
	cf(1,semform_data(10,93,21,23)),
	cf(1,fspan(var(0),1,25)),
	cf(1,fspan(var(9),1,7)),
	cf(1,fspan(var(31),16,25)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'bjeffet',8,15)),
	cf(1,surfaceform(59,'hver',16,20)),
	cf(1,surfaceform(84,'dag',21,25)),
	cf(1,surfaceform(114,'.',24,25))
	]).
