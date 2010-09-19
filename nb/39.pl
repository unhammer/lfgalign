% -*- coding: iso-8859-1 -*-

fstructure('Hunden kommer til å bjeffe.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.15 CPU seconds, 162 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('future',6,[var(5)],[var(6)]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(6))),
	cf(1,eq(attr(var(0),'XCOMP'),var(5))),
	cf(1,eq(attr(var(0),'TOPIC'),var(6))),
	cf(1,eq(attr(var(0),'CHECK'),var(7))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(8))),
	cf(1,eq(proj(var(0),'m::'),var(9))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(6),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(6),'CHECK'),var(10))),
	cf(1,eq(attr(var(6),'GEND'),var(11))),
	cf(1,eq(attr(var(6),'NTYPE'),var(12))),
	cf(1,eq(proj(var(6),'m::'),var(13))),
	cf(1,eq(attr(var(6),'CASE'),'nom')),
	cf(1,eq(attr(var(6),'DEF'),'+')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(6),'PERS'),'3')),
	cf(1,eq(attr(var(10),'_SEL'),var(14))),
	cf(1,eq(attr(var(10),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(10),'_NOUN'),'+')),
	cf(1,eq(attr(var(14),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(14),'_ANIM'),'+')),
	cf(1,eq(attr(var(14),'_HUMAN'),'-')),
	cf(1,eq(attr(var(11),'FEM'),'-')),
	cf(1,eq(attr(var(11),'MASC'),'+')),
	cf(1,eq(attr(var(11),'NEUT'),'-')),
	cf(1,eq(attr(var(12),'NSEM'),var(15))),
	cf(1,eq(attr(var(12),'NSYN'),'common')),
	cf(1,eq(attr(var(15),'COMMON'),'count')),
	cf(1,eq(attr(var(13),'H-CONS'),var(16))),
	cf(1,eq(attr(var(13),'RELS'),var(17))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(2))),
	cf(1,eq(attr(var(13),'TOP'),var(18))),
	cf(1,eq(attr(var(13),'_QUANT'),var(19))),
	cf(1,in_set(var(20),var(16))),
	cf(1,eq(attr(var(20),'OUTSCPD'),var(21))),
	cf(1,eq(attr(var(20),'SC_ARG'),var(22))),
	cf(1,eq(attr(var(20),'relation'),'qeq')),
	cf(1,eq(attr(var(21),'type'),'handle')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,in_set(var(1),var(17))),
	cf(1,in_set(var(2),var(17))),
	cf(1,eq(attr(var(1),'ARG0'),var(23))),
	cf(1,eq(attr(var(1),'BODY'),var(24))),
	cf(1,eq(attr(var(1),'LBL'),var(18))),
	cf(1,eq(attr(var(1),'RSTR'),var(22))),
	cf(1,eq(attr(var(1),'relation'),semform('def_q',3,[],[]))),
	cf(1,eq(attr(var(23),'DIV'),'-')),
	cf(1,eq(attr(var(23),'GRIND'),'-')),
	cf(1,eq(attr(var(23),'NATGEND'),'gender')),
	cf(1,eq(attr(var(23),'NUM'),'sg')),
	cf(1,eq(attr(var(23),'PERS'),'3')),
	cf(1,eq(attr(var(23),'type'),'ref-ind')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(18),'type'),'handle')),
	cf(1,eq(attr(var(2),'ARG0'),var(23))),
	cf(1,eq(attr(var(2),'LBL'),var(21))),
	cf(1,eq(attr(var(2),'_CAT'),'n')),
	cf(1,eq(attr(var(2),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(19),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(19),'TOP'),var(25))),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(5),'PRED'),semform('bjeffe',11,[var(6)],[]))),
	cf(1,eq(attr(var(5),'SUBJ'),var(6))),
	cf(1,eq(attr(var(5),'CHECK'),var(26))),
	cf(1,eq(attr(var(5),'GEND'),var(27))),
	cf(1,eq(proj(var(5),'m::'),var(28))),
	cf(1,eq(attr(var(5),'CASE'),'obl')),
	cf(1,eq(attr(var(5),'COMP-FORM'),'å')),
	cf(1,eq(attr(var(5),'NUM'),'sg')),
	cf(1,eq(attr(var(5),'VFORM'),'inf')),
	cf(1,eq(attr(var(5),'VTYPE'),'main')),
	cf(1,eq(attr(var(26),'_P-SELFORM'),'til')),
	cf(1,eq(attr(var(26),'_PREPEXISTS'),'+')),
	cf(1,eq(attr(var(26),'_TOPVP'),'-')),
	cf(1,eq(attr(var(26),'_UNIQUEOCCURRENCE'),'+_')),
	cf(1,eq(attr(var(27),'FEM'),'-')),
	cf(1,eq(attr(var(27),'MASC'),'-')),
	cf(1,eq(attr(var(27),'NEUT'),'+')),
	cf(1,eq(attr(var(28),'H-CONS'),var(16))),
	cf(1,eq(attr(var(28),'RELS'),var(29))),
	cf(1,eq(attr(var(28),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(28),'_MSG'),var(30))),
	cf(1,in_set(var(3),var(29))),
	cf(1,in_set(var(4),var(29))),
	cf(1,eq(attr(var(3),'ARG0'),var(31))),
	cf(1,eq(attr(var(3),'ARG1'),var(32))),
	cf(1,eq(attr(var(3),'ARG2'),var(32))),
	cf(1,eq(attr(var(3),'LBL'),var(33))),
	cf(1,eq(attr(var(3),'_CAT'),'p')),
	cf(1,eq(attr(var(3),'_CATSUFF'),'sel')),
	cf(1,eq(attr(var(3),'relation'),semform('til',10,[],[]))),
	cf(1,eq(attr(var(31),'type'),'event')),
	cf(1,eq(attr(var(32),'PERF'),'-')),
	cf(1,eq(attr(var(32),'SF'),'prop')),
	cf(1,eq(attr(var(32),'TENSE'),'fut')),
	cf(1,eq(attr(var(32),'type'),'event')),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(32))),
	cf(1,eq(attr(var(4),'ARG1'),var(23))),
	cf(1,eq(attr(var(4),'LBL'),var(33))),
	cf(1,eq(attr(var(4),'_CAT'),'v')),
	cf(1,eq(attr(var(4),'relation'),semform('bjeffe',12,[],[]))),
	cf(1,eq(attr(var(30),'ARG0'),var(32))),
	cf(1,eq(attr(var(30),'LBL'),var(34))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(7),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(8),'MOOD'),'indicative')),
	cf(1,eq(attr(var(8),'TENSE'),'pres')),
	cf(1,eq(attr(var(9),'H-CONS'),var(35))),
	cf(1,eq(attr(var(9),'INDEX'),var(32))),
	cf(1,eq(attr(var(9),'RELS'),var(36))),
	cf(1,eq(attr(var(9),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(9),'TOP'),var(34))),
	cf(1,eq(attr(var(9),'_MSG'),var(30))),
	cf(1,eq(attr(var(9),'_MSGQEQ'),var(37))),
	cf(1,in_set(var(37),var(35))),
	cf(1,eq(attr(var(37),'OUTSCPD'),var(33))),
	cf(1,eq(attr(var(37),'SC_ARG'),var(34))),
	cf(1,eq(attr(var(37),'relation'),'qeq')),
	cf(1,in_set(var(3),var(36))),
	cf(1,in_set(var(4),var(36))),
	cf(1,eq(proj(var(38),'m::'),var(39))),
	cf(1,eq(proj(var(40),'m::'),var(39))),
	cf(1,eq(proj(var(41),'m::'),var(42))),
	cf(1,eq(proj(var(43),'m::'),var(42))),
	cf(1,eq(proj(var(44),'m::'),var(45))),
	cf(1,eq(proj(var(46),'m::'),var(45))),
	cf(1,eq(attr(var(47),'PRED'),semform('til',9,[var(5)],[]))),
	cf(1,eq(attr(var(47),'OBJ'),var(5))),
	cf(1,eq(attr(var(47),'CHECK'),var(48))),
	cf(1,eq(proj(var(47),'m::'),var(49))),
	cf(1,eq(attr(var(47),'PFORM'),'til')),
	cf(1,eq(attr(var(47),'PTYPE'),'sem')),
	cf(1,eq(attr(var(48),'_ANTECED'),var(50))),
	cf(1,eq(attr(var(48),'_MOVED-OBJ'),'-')),
	cf(1,eq(proj(var(50),'m::'),var(51))),
	cf(1,eq(attr(var(50),'NUM'),'sg')),
	cf(1,eq(attr(var(50),'PERS'),'3')),
	cf(1,eq(attr(var(51),'ARG0'),var(23))),
	cf(1,eq(attr(var(49),'RELS'),var(29))),
	cf(1,eq(attr(var(49),'RELS_EL'),var(3)))
	],
	% C-Structure:
	[
	cf(1,subtree(1000,'ROOT',1045,122)),
	cf(1,phi(1000,var(0))),
	cf(1,subtree(1045,'ROOT',-,1333)),
	cf(1,phi(1045,var(0))),
	cf(1,subtree(1333,'IP',2301,2427)),
	cf(1,phi(1333,var(0))),
	cf(1,subtree(2301,'IP',-,2533)),
	cf(1,phi(2301,var(0))),
	cf(1,subtree(2533,'NP',-,2)),
	cf(1,phi(2533,var(6))),
	cf(1,subtree(2,'N',318,4)),
	cf(1,phi(2,var(6))),
	cf(1,cproj(2,var(38))),
	cf(1,subtree(318,'N',314,6)),
	cf(1,phi(318,var(6))),
	cf(1,cproj(318,var(38))),
	cf(1,subtree(314,'N',310,8)),
	cf(1,phi(314,var(6))),
	cf(1,cproj(314,var(38))),
	cf(1,subtree(310,'N',307,10)),
	cf(1,phi(310,var(6))),
	cf(1,cproj(310,var(38))),
	cf(1,subtree(307,'N',-,12)),
	cf(1,phi(307,var(6))),
	cf(1,cproj(307,var(38))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(6))),
	cf(1,cproj(12,var(40))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(6))),
	cf(1,cproj(13,var(40))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(6))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(6))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(6))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(6))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(6))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(52))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(6))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(6))),
	cf(1,subtree(2427,'I\'',2360,2367)),
	cf(1,phi(2427,var(0))),
	cf(1,subtree(2360,'I\'',-,386)),
	cf(1,phi(2360,var(0))),
	cf(1,subtree(386,'Vfin',385,22)),
	cf(1,phi(386,var(0))),
	cf(1,cproj(386,var(41))),
	cf(1,subtree(385,'Vfin',384,24)),
	cf(1,phi(385,var(0))),
	cf(1,cproj(385,var(41))),
	cf(1,subtree(384,'Vfin',-,26)),
	cf(1,phi(384,var(0))),
	cf(1,cproj(384,var(41))),
	cf(1,subtree(26,'V_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,cproj(26,var(43))),
	cf(1,terminal(27,'komme',[20])),
	cf(1,phi(27,var(0))),
	cf(1,cproj(27,var(43))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(25,'+Verb',[20])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(23,'+Pres',[20])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(2367,'S',-,2373)),
	cf(1,phi(2367,var(0))),
	cf(1,subtree(2373,'VPmain',-,1837)),
	cf(1,phi(2373,var(0))),
	cf(1,subtree(1837,'PPsel2',1829,1463)),
	cf(1,phi(1837,var(47))),
	cf(1,subtree(1829,'PPsel2',-,63)),
	cf(1,phi(1829,var(47))),
	cf(1,subtree(63,'Psel2',-,43)),
	cf(1,phi(63,var(47))),
	cf(1,terminal(43,'til',[43])),
	cf(1,phi(43,var(53))),
	cf(1,subtree(1463,'VP\'',1462,1137)),
	cf(1,phi(1463,var(5))),
	cf(1,subtree(1462,'VP\'',-,98)),
	cf(1,phi(1462,var(5))),
	cf(1,subtree(98,'PARTinf',-,64)),
	cf(1,phi(98,var(5))),
	cf(1,terminal(64,'å',[64])),
	cf(1,phi(64,var(5))),
	cf(1,subtree(1137,'VP',-,1134)),
	cf(1,phi(1137,var(5))),
	cf(1,subtree(1134,'V',1133,102)),
	cf(1,phi(1134,var(5))),
	cf(1,cproj(1134,var(44))),
	cf(1,subtree(1133,'V',1132,104)),
	cf(1,phi(1133,var(5))),
	cf(1,cproj(1133,var(44))),
	cf(1,subtree(1132,'V',-,106)),
	cf(1,phi(1132,var(5))),
	cf(1,cproj(1132,var(44))),
	cf(1,subtree(106,'V_BASE',-,107)),
	cf(1,phi(106,var(5))),
	cf(1,cproj(106,var(46))),
	cf(1,terminal(107,'bjeffe',[101])),
	cf(1,phi(107,var(5))),
	cf(1,cproj(107,var(46))),
	cf(1,subtree(104,'V_SUFF_BASE',-,105)),
	cf(1,phi(104,var(5))),
	cf(1,terminal(105,'+Verb',[101])),
	cf(1,phi(105,var(5))),
	cf(1,subtree(102,'V_SUFF_BASE',-,103)),
	cf(1,phi(102,var(5))),
	cf(1,terminal(103,'+Infin',[101])),
	cf(1,phi(103,var(5))),
	cf(1,subtree(122,'PERIOD',-,115)),
	cf(1,phi(122,var(0))),
	cf(1,terminal(115,'.',[115])),
	cf(1,phi(115,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(1,semform_data(6,26,8,12)),
	cf(1,semform_data(9,63,15,18)),
	cf(1,semform_data(10,63,15,18)),
	cf(1,semform_data(11,106,21,26)),
	cf(1,semform_data(12,106,21,26)),
	cf(1,fspan(var(0),1,28)),
	cf(1,fspan(var(6),1,7)),
	cf(1,fspan(var(47),15,28)),
	cf(1,fspan(var(5),19,28)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'kommer',8,14)),
	cf(1,surfaceform(43,'til',15,18)),
	cf(1,surfaceform(64,'å',19,20)),
	cf(1,surfaceform(101,'bjeffe',21,28)),
	cf(1,surfaceform(115,'.',27,28))
	]).

