% -*- coding: iso-8859-1 -*-

fstructure('Hunden kunne bjeffe.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.06 CPU seconds, 93 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),var(7))),
	cf(1,eq(attr(var(0),'SUBJ'),var(8))),
	cf(1,eq(attr(var(0),'XCOMP'),var(9))),
	cf(1,eq(attr(var(0),'TOPIC'),var(8))),
	cf(1,eq(attr(var(0),'CHECK'),var(10))),
	cf(1,eq(attr(var(0),'MOD-TYPE'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(12))),
	cf(1,eq(proj(var(0),'m::'),var(13))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'aux')),
	cf(A1,eq(var(7),semform('root-kunne',6,[var(8),var(9)],[]))),
	cf(A2,eq(var(7),semform('epist-kunne',9,[var(9)],[var(8)]))),
	cf(1,eq(attr(var(8),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(14))),
	cf(1,eq(attr(var(8),'GEND'),var(15))),
	cf(1,eq(attr(var(8),'NTYPE'),var(16))),
	cf(1,eq(proj(var(8),'m::'),var(17))),
	cf(1,eq(attr(var(8),'CASE'),'nom')),
	cf(1,eq(attr(var(8),'DEF'),'+')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'PERS'),'3')),
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
	cf(1,eq(attr(var(17),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(17),'TOP'),var(22))),
	cf(1,eq(attr(var(17),'_QUANT'),var(23))),
	cf(A2,in_set(var(1),var(20))),
	cf(1,in_set(var(2),var(20))),
	cf(1,in_set(var(3),var(20))),
	cf(A1,in_set(var(4),var(20))),
	cf(A2,eq(attr(var(1),'OUTSCPD'),var(24))),
	cf(A2,eq(attr(var(1),'SC_ARG'),var(25))),
	cf(A2,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),var(26))),
	cf(A1,eq(attr(var(25),'DIV'),'-')),
	cf(A1,eq(attr(var(25),'GRIND'),'-')),
	cf(A1,eq(attr(var(25),'NATGEND'),'gender')),
	cf(A1,eq(attr(var(25),'NUM'),'sg')),
	cf(A1,eq(attr(var(25),'PERS'),'3')),
	cf(A2,eq(var(26),'handle')),
	cf(A1,eq(var(26),'ref-ind')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(27))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(28))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(29))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(30))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(A1,eq(attr(var(4),'OUTSCPD'),var(24))),
	cf(A1,eq(attr(var(4),'SC_ARG'),var(31))),
	cf(A1,eq(attr(var(4),'relation'),'qeq')),
	cf(A1,eq(attr(var(31),'type'),'handle')),
	cf(1,in_set(var(5),var(21))),
	cf(1,in_set(var(6),var(21))),
	cf(1,eq(attr(var(5),'ARG0'),var(32))),
	cf(1,eq(attr(var(5),'BODY'),var(33))),
	cf(1,eq(attr(var(5),'LBL'),var(22))),
	cf(1,eq(attr(var(5),'RSTR'),var(28))),
	cf(1,eq(attr(var(5),'relation'),semform('def_q',3,[],[]))),
	cf(A1,eq(var(32),var(25))),
	cf(1,eq(attr(var(32),'DIV'),'-')),
	cf(1,eq(attr(var(32),'GRIND'),'-')),
	cf(1,eq(attr(var(32),'NATGEND'),'gender')),
	cf(1,eq(attr(var(32),'NUM'),'sg')),
	cf(1,eq(attr(var(32),'PERS'),'3')),
	cf(1,eq(attr(var(32),'type'),'ref-ind')),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(6),'ARG0'),var(32))),
	cf(1,eq(attr(var(6),'LBL'),var(27))),
	cf(1,eq(attr(var(6),'_CAT'),'n')),
	cf(1,eq(attr(var(6),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(23),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(23),'TOP'),var(34))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(9),'PRED'),semform('bjeffe',11,[var(8)],[]))),
	cf(1,eq(attr(var(9),'SUBJ'),var(8))),
	cf(1,eq(attr(var(9),'CHECK'),var(35))),
	cf(A2,eq(attr(var(9),'TNS-ASP'),var(36))),
	cf(1,eq(proj(var(9),'m::'),var(37))),
	cf(A1,eq(attr(var(9),'PERF'),'-')),
	cf(1,eq(attr(var(9),'VFORM'),'inf')),
	cf(1,eq(attr(var(9),'VTYPE'),'main')),
	cf(1,eq(attr(var(35),'_AUX1COMP'),'+')),
	cf(1,eq(attr(var(35),'_TOPVP'),'-')),
	cf(A2,eq(attr(var(36),'TENSE'),'notense')),
	cf(1,eq(attr(var(37),'H-CONS'),var(20))),
	cf(1,eq(attr(var(37),'INDEX'),var(38))),
	cf(1,eq(attr(var(37),'RELS'),var(39))),
	cf(1,eq(attr(var(37),'RELS_EL'),var(40))),
	cf(A1,eq(attr(var(37),'TOP'),var(31))),
	cf(1,eq(attr(var(37),'_MSG'),var(41))),
	cf(A1,eq(attr(var(37),'_MSGQEQ'),var(4))),
	cf(1,eq(attr(var(38),'PERF'),'-')),
	cf(1,eq(attr(var(38),'TENSE'),'notense')),
	cf(1,eq(attr(var(38),'type'),'event')),
	cf(1,in_set(var(40),var(39))),
	cf(1,eq(attr(var(40),'ARG0'),var(38))),
	cf(1,eq(attr(var(40),'ARG1'),var(32))),
	cf(1,eq(attr(var(40),'LBL'),var(24))),
	cf(1,eq(attr(var(40),'_CAT'),'v')),
	cf(1,eq(attr(var(40),'relation'),semform('bjeffe',12,[],[]))),
	cf(A2,eq(var(41),var(42))),
	cf(A1,eq(attr(var(41),'ARG0'),var(38))),
	cf(A1,eq(attr(var(41),'LBL'),var(31))),
	cf(1,eq(attr(var(10),'_MAIN-CL'),'+')),
	cf(A1,eq(var(11),'root')),
	cf(A2,eq(var(11),'epistemic')),
	cf(1,eq(attr(var(12),'MOOD'),'indicative')),
	cf(1,eq(attr(var(12),'TENSE'),'past')),
	cf(1,eq(attr(var(13),'H-CONS'),var(20))),
	cf(1,eq(attr(var(13),'INDEX'),var(43))),
	cf(1,eq(attr(var(13),'RELS'),var(44))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(45))),
	cf(1,eq(attr(var(13),'TOP'),var(30))),
	cf(1,eq(attr(var(13),'_MSG'),var(42))),
	cf(1,eq(attr(var(13),'_MSGQEQ'),var(3))),
	cf(1,eq(attr(var(43),'PERF'),'-')),
	cf(1,eq(attr(var(43),'SF'),'prop')),
	cf(1,eq(attr(var(43),'TENSE'),'past')),
	cf(1,eq(attr(var(43),'type'),'event')),
	cf(1,in_set(var(45),var(44))),
	cf(1,eq(attr(var(45),'ARG0'),var(43))),
	cf(1,eq(attr(var(45),'ARG1'),var(25))),
	cf(A1,eq(attr(var(45),'ARG2'),var(31))),
	cf(1,eq(attr(var(45),'LBL'),var(29))),
	cf(1,eq(attr(var(45),'relation'),var(46))),
	cf(1,eq(attr(var(45),'_CAT'),'v')),
	cf(A1,eq(var(46),semform('root-kunne',8,[],[]))),
	cf(A2,eq(var(46),semform('epist-kunne',10,[],[]))),
	cf(1,eq(attr(var(42),'ARG0'),var(43))),
	cf(1,eq(attr(var(42),'LBL'),var(30))),
	cf(1,eq(proj(var(47),'m::'),var(48))),
	cf(1,eq(proj(var(49),'m::'),var(48))),
	cf(1,eq(proj(var(50),'m::'),var(51))),
	cf(1,eq(proj(var(52),'m::'),var(51))),
	cf(1,eq(proj(var(53),'m::'),var(54))),
	cf(1,eq(proj(var(55),'m::'),var(54)))
	],
	% C-Structure:
	[
	cf(1,subtree(804,'ROOT',843,59)),
	cf(1,phi(804,var(0))),
	cf(1,subtree(843,'ROOT',-,1245)),
	cf(1,phi(843,var(0))),
	cf(1,subtree(1245,'IP',1277,1338)),
	cf(1,phi(1245,var(0))),
	cf(1,subtree(1277,'IP',-,1368)),
	cf(1,phi(1277,var(0))),
	cf(1,subtree(1368,'NP',-,2)),
	cf(1,phi(1368,var(8))),
	cf(1,subtree(2,'N',255,4)),
	cf(1,phi(2,var(8))),
	cf(1,cproj(2,var(47))),
	cf(1,subtree(255,'N',251,6)),
	cf(1,phi(255,var(8))),
	cf(1,cproj(255,var(47))),
	cf(1,subtree(251,'N',247,8)),
	cf(1,phi(251,var(8))),
	cf(1,cproj(251,var(47))),
	cf(1,subtree(247,'N',244,10)),
	cf(1,phi(247,var(8))),
	cf(1,cproj(247,var(47))),
	cf(1,subtree(244,'N',-,12)),
	cf(1,phi(244,var(8))),
	cf(1,cproj(244,var(47))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(8))),
	cf(1,cproj(12,var(49))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(8))),
	cf(1,cproj(13,var(49))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(8))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(8))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(8))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(8))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(8))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(56))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(8))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(8))),
	cf(1,subtree(1338,'I\'',1313,1316)),
	cf(1,phi(1338,var(0))),
	cf(1,subtree(1313,'I\'',-,323)),
	cf(1,phi(1313,var(0))),
	cf(1,subtree(323,'Vfin',322,21)),
	cf(1,phi(323,var(0))),
	cf(1,cproj(323,var(50))),
	cf(1,subtree(322,'Vfin',321,23)),
	cf(1,phi(322,var(0))),
	cf(1,cproj(322,var(50))),
	cf(1,subtree(321,'Vfin',-,25)),
	cf(1,phi(321,var(0))),
	cf(1,cproj(321,var(50))),
	cf(1,subtree(25,'V_BASE',-,26)),
	cf(1,phi(25,var(0))),
	cf(1,cproj(25,var(52))),
	cf(1,terminal(26,'kunne',[20])),
	cf(1,phi(26,var(57))),
	cf(1,cproj(26,var(52))),
	cf(1,subtree(23,'V_SUFF_BASE',-,24)),
	cf(1,phi(23,var(0))),
	cf(1,terminal(24,'+Aux',[20])),
	cf(1,phi(24,var(0))),
	cf(1,subtree(21,'V_SUFF_BASE',-,22)),
	cf(1,phi(21,var(0))),
	cf(1,terminal(22,'+Past',[20])),
	cf(1,phi(22,var(0))),
	cf(1,subtree(1316,'S',-,1321)),
	cf(1,phi(1316,var(0))),
	cf(1,subtree(1321,'VPmain',-,935)),
	cf(1,phi(1321,var(0))),
	cf(1,subtree(935,'VP',-,932)),
	cf(1,phi(935,var(9))),
	cf(1,subtree(932,'V',931,39)),
	cf(1,phi(932,var(9))),
	cf(1,cproj(932,var(53))),
	cf(1,subtree(931,'V',930,41)),
	cf(1,phi(931,var(9))),
	cf(1,cproj(931,var(53))),
	cf(1,subtree(930,'V',-,43)),
	cf(1,phi(930,var(9))),
	cf(1,cproj(930,var(53))),
	cf(1,subtree(43,'V_BASE',-,44)),
	cf(1,phi(43,var(9))),
	cf(1,cproj(43,var(55))),
	cf(1,terminal(44,'bjeffe',[38])),
	cf(1,phi(44,var(9))),
	cf(1,cproj(44,var(55))),
	cf(1,subtree(41,'V_SUFF_BASE',-,42)),
	cf(1,phi(41,var(9))),
	cf(1,terminal(42,'+Verb',[38])),
	cf(1,phi(42,var(9))),
	cf(1,subtree(39,'V_SUFF_BASE',-,40)),
	cf(1,phi(39,var(9))),
	cf(1,terminal(40,'+Infin',[38])),
	cf(1,phi(40,var(9))),
	cf(1,subtree(59,'PERIOD',-,52)),
	cf(1,phi(59,var(0))),
	cf(1,terminal(52,'.',[52])),
	cf(1,phi(52,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(A1,semform_data(6,25,8,12)),
	cf(A1,semform_data(8,25,8,12)),
	cf(A2,semform_data(9,25,8,12)),
	cf(A2,semform_data(10,25,8,12)),
	cf(1,semform_data(11,43,14,19)),
	cf(1,semform_data(12,43,14,19)),
	cf(1,fspan(var(0),1,21)),
	cf(1,fspan(var(8),1,7)),
	cf(1,fspan(var(9),14,21)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'kunne',8,13)),
	cf(1,surfaceform(38,'bjeffe',14,21)),
	cf(1,surfaceform(52,'.',20,21))
	]).
