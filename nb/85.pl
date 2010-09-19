% -*- coding: iso-8859-1 -*-

fstructure('Browne anser Abrams som gammel.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.26 CPU seconds, 182 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('anse',4,[var(11),var(12),var(13)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(11))),
	cf(1,eq(attr(var(0),'OBJ'),var(12))),
	cf(1,eq(attr(var(0),'PREDLINK'),var(13))),
	cf(1,eq(attr(var(0),'TOPIC'),var(11))),
	cf(1,eq(attr(var(0),'CHECK'),var(14))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(15))),
	cf(1,eq(proj(var(0),'m::'),var(16))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(11),'PRED'),semform('Browne',0,[],[]))),
	cf(1,eq(attr(var(11),'CHECK'),var(17))),
	cf(1,eq(attr(var(11),'GEND'),var(18))),
	cf(1,eq(attr(var(11),'NTYPE'),var(19))),
	cf(1,eq(proj(var(11),'m::'),var(20))),
	cf(1,eq(attr(var(11),'CASE'),'nom')),
	cf(1,eq(attr(var(11),'DEF'),'+')),
	cf(1,eq(attr(var(11),'NUM'),'sg')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(1,eq(attr(var(11),'REF'),'+')),
	cf(1,eq(attr(var(17),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(18),'FEM'),'-')),
	cf(1,eq(attr(var(18),'MASC'),'+')),
	cf(1,eq(attr(var(18),'NEUT'),'-')),
	cf(1,eq(attr(var(19),'NSEM'),var(21))),
	cf(1,eq(attr(var(19),'NSYN'),'proper')),
	cf(1,eq(attr(var(21),'PROPER'),var(22))),
	cf(1,eq(attr(var(22),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(20),'H-CONS'),var(23))),
	cf(1,eq(attr(var(20),'RELS'),var(24))),
	cf(1,eq(attr(var(20),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(20),'TOP'),var(25))),
	cf(1,eq(attr(var(20),'_QUANT'),var(26))),
	cf(1,in_set(var(1),var(23))),
	cf(1,in_set(var(2),var(23))),
	cf(1,in_set(var(3),var(23))),
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
	cf(1,eq(attr(var(3),'OUTSCPD'),var(31))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(32))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(1,in_set(var(4),var(24))),
	cf(1,in_set(var(5),var(24))),
	cf(1,eq(attr(var(4),'ARG0'),var(33))),
	cf(1,eq(attr(var(4),'LBL'),var(27))),
	cf(1,eq(attr(var(4),'CARG'),'Browne')),
	cf(1,eq(attr(var(4),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(33),'NUM'),'sg')),
	cf(1,eq(attr(var(33),'PERS'),'3')),
	cf(1,eq(attr(var(33),'type'),'ref-ind')),
	cf(1,eq(attr(var(5),'ARG0'),var(33))),
	cf(1,eq(attr(var(5),'BODY'),var(34))),
	cf(1,eq(attr(var(5),'LBL'),var(25))),
	cf(1,eq(attr(var(5),'RSTR'),var(28))),
	cf(1,eq(attr(var(5),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(26),'TOP'),var(35))),
	cf(1,eq(attr(var(35),'type'),'handle')),
	cf(1,eq(attr(var(12),'PRED'),semform('Abrams',7,[],[]))),
	cf(1,eq(attr(var(12),'CHECK'),var(36))),
	cf(1,eq(attr(var(12),'GEND'),var(37))),
	cf(1,eq(attr(var(12),'NTYPE'),var(38))),
	cf(1,eq(proj(var(12),'m::'),var(39))),
	cf(1,eq(attr(var(12),'CASE'),'obl')),
	cf(1,eq(attr(var(12),'DEF'),'+')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(12),'REF'),'+')),
	cf(1,eq(attr(var(36),'_SEL'),var(40))),
	cf(1,eq(attr(var(36),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(40),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(40),'_ANIM'),'+')),
	cf(1,eq(attr(var(40),'_HUMAN'),'+')),
	cf(1,eq(attr(var(37),'NEUT'),'-')),
	cf(1,eq(attr(var(38),'NSEM'),var(41))),
	cf(1,eq(attr(var(38),'NSYN'),'proper')),
	cf(1,eq(attr(var(41),'PROPER'),var(42))),
	cf(1,eq(attr(var(42),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(39),'H-CONS'),var(43))),
	cf(1,eq(attr(var(39),'RELS'),var(44))),
	cf(1,eq(attr(var(39),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(39),'TOP'),var(45))),
	cf(1,eq(attr(var(39),'_ANTECEDVAR'),var(33))),
	cf(1,eq(attr(var(39),'_QUANT'),var(46))),
	cf(1,in_set(var(47),var(43))),
	cf(1,eq(attr(var(47),'OUTSCPD'),var(48))),
	cf(1,eq(attr(var(47),'SC_ARG'),var(49))),
	cf(1,eq(attr(var(47),'relation'),'qeq')),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(49),'type'),'handle')),
	cf(1,in_set(var(6),var(44))),
	cf(1,in_set(var(7),var(44))),
	cf(1,eq(attr(var(6),'ARG0'),var(50))),
	cf(1,eq(attr(var(6),'LBL'),var(48))),
	cf(1,eq(attr(var(6),'CARG'),'Abrams')),
	cf(1,eq(attr(var(6),'relation'),semform('named',8,[],[]))),
	cf(1,eq(attr(var(50),'NATGEND'),'animate')),
	cf(1,eq(attr(var(50),'NUM'),'sg')),
	cf(1,eq(attr(var(50),'PERS'),'3')),
	cf(1,eq(attr(var(50),'type'),'ref-ind')),
	cf(1,eq(attr(var(7),'ARG0'),var(50))),
	cf(1,eq(attr(var(7),'BODY'),var(51))),
	cf(1,eq(attr(var(7),'LBL'),var(45))),
	cf(1,eq(attr(var(7),'RSTR'),var(49))),
	cf(1,eq(attr(var(7),'relation'),semform('proper_q',9,[],[]))),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(46),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(46),'TOP'),var(52))),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(1,eq(attr(var(13),'PRED'),semform('gammel',14,[],[]))),
	cf(1,eq(attr(var(13),'CHECK'),var(53))),
	cf(1,eq(attr(var(13),'GEND'),var(37))),
	cf(1,eq(proj(var(13),'m::'),var(54))),
	cf(1,eq(attr(var(13),'ATYPE'),'predicative')),
	cf(1,eq(attr(var(13),'DEF'),'-')),
	cf(1,eq(attr(var(13),'MEASDIM'),'age')),
	cf(1,eq(attr(var(13),'NUM'),'sg')),
	cf(1,eq(attr(var(53),'_ADVERBIAL'),'-')),
	cf(1,eq(attr(var(53),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(53),'_DEG-MORPH'),'positive')),
	cf(1,eq(attr(var(53),'_P-SELFORM'),'som')),
	cf(1,eq(attr(var(53),'_PREDICATIVE'),'+')),
	cf(1,eq(attr(var(53),'_PREPEXISTS'),'+')),
	cf(1,eq(attr(var(54),'INDEX'),var(55))),
	cf(1,eq(attr(var(54),'RELS'),var(56))),
	cf(1,eq(attr(var(54),'RELS_EL'),var(9))),
	cf(1,eq(attr(var(54),'TOP'),var(32))),
	cf(1,eq(attr(var(54),'_MSG'),var(57))),
	cf(1,eq(attr(var(54),'_MSGQEQ'),var(3))),
	cf(1,eq(attr(var(55),'PERF'),'-')),
	cf(1,eq(attr(var(55),'TENSE'),'notense')),
	cf(1,eq(attr(var(55),'type'),'event')),
	cf(1,in_set(var(8),var(56))),
	cf(1,in_set(var(9),var(56))),
	cf(1,eq(attr(var(8),'ARG0'),var(58))),
	cf(1,eq(attr(var(8),'ARG1'),var(59))),
	cf(1,eq(attr(var(8),'ARG2'),var(32))),
	cf(1,eq(attr(var(8),'LBL'),var(29))),
	cf(1,eq(attr(var(8),'_CAT'),'p')),
	cf(1,eq(attr(var(8),'_CATSUFF'),'sel')),
	cf(1,eq(attr(var(8),'relation'),semform('som',13,[],[]))),
	cf(1,eq(attr(var(58),'type'),'event')),
	cf(1,eq(attr(var(59),'PERF'),'-')),
	cf(1,eq(attr(var(59),'SF'),'prop')),
	cf(1,eq(attr(var(59),'TENSE'),'pres')),
	cf(1,eq(attr(var(59),'type'),'event')),
	cf(1,eq(attr(var(9),'ARG0'),var(55))),
	cf(1,eq(attr(var(9),'ARG1'),var(50))),
	cf(1,eq(attr(var(9),'LBL'),var(31))),
	cf(1,eq(attr(var(9),'_CAT'),'a')),
	cf(1,eq(attr(var(9),'relation'),semform('gammel',15,[],[]))),
	cf(1,eq(attr(var(57),'ARG0'),var(55))),
	cf(1,eq(attr(var(57),'LBL'),var(32))),
	cf(1,eq(attr(var(14),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(15),'MOOD'),'indicative')),
	cf(1,eq(attr(var(15),'TENSE'),'pres')),
	cf(1,eq(attr(var(16),'H-CONS'),var(23))),
	cf(1,eq(attr(var(16),'INDEX'),var(59))),
	cf(1,eq(attr(var(16),'RELS'),var(60))),
	cf(1,eq(attr(var(16),'RELS_EL'),var(10))),
	cf(1,eq(attr(var(16),'TOP'),var(30))),
	cf(1,eq(attr(var(16),'_MSG'),var(61))),
	cf(1,eq(attr(var(16),'_MSGQEQ'),var(2))),
	cf(1,in_set(var(8),var(60))),
	cf(1,in_set(var(10),var(60))),
	cf(1,eq(attr(var(10),'ARG0'),var(59))),
	cf(1,eq(attr(var(10),'ARG1'),var(33))),
	cf(1,eq(attr(var(10),'ARG2'),var(50))),
	cf(1,eq(attr(var(10),'ARG3'),var(32))),
	cf(1,eq(attr(var(10),'LBL'),var(29))),
	cf(1,eq(attr(var(10),'_CAT'),'v')),
	cf(1,eq(attr(var(10),'_PRT'),'som')),
	cf(1,eq(attr(var(10),'relation'),semform('anse',6,[],[]))),
	cf(1,eq(attr(var(61),'ARG0'),var(59))),
	cf(1,eq(attr(var(61),'LBL'),var(30))),
	cf(1,eq(proj(var(62),'m::'),var(63))),
	cf(1,eq(proj(var(64),'m::'),var(63))),
	cf(1,eq(attr(var(65),'PRED'),semform('som',12,[var(13)],[]))),
	cf(1,eq(attr(var(65),'OBJ'),var(13))),
	cf(1,eq(proj(var(65),'m::'),var(66))),
	cf(1,eq(attr(var(65),'PFORM'),'som')),
	cf(1,eq(attr(var(65),'PTYPE'),'sem')),
	cf(1,eq(attr(var(66),'RELS'),var(56))),
	cf(1,eq(attr(var(66),'RELS_EL'),var(8)))
	],
	% C-Structure:
	[
	cf(1,subtree(1302,'ROOT',1299,103)),
	cf(1,phi(1302,var(0))),
	cf(1,subtree(1299,'ROOT',-,2254)),
	cf(1,phi(1299,var(0))),
	cf(1,subtree(2254,'IP',2131,2252)),
	cf(1,phi(2254,var(0))),
	cf(1,subtree(2131,'IP',-,2381)),
	cf(1,phi(2131,var(0))),
	cf(1,subtree(2381,'PROPP',-,2366)),
	cf(1,phi(2381,var(11))),
	cf(1,subtree(2366,'PROP',2364,2)),
	cf(1,phi(2366,var(11))),
	cf(1,subtree(2364,'PROP',2362,4)),
	cf(1,phi(2364,var(11))),
	cf(1,subtree(2362,'PROP',2360,6)),
	cf(1,phi(2362,var(11))),
	cf(1,subtree(2360,'PROP',2357,8)),
	cf(1,phi(2360,var(11))),
	cf(1,subtree(2357,'PROP',-,10)),
	cf(1,phi(2357,var(11))),
	cf(1,subtree(10,'PROP_BASE',-,11)),
	cf(1,phi(10,var(11))),
	cf(1,terminal(11,'Browne',[1])),
	cf(1,phi(11,var(67))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(11))),
	cf(1,terminal(9,'+Prop',[1])),
	cf(1,phi(9,var(11))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(11))),
	cf(1,terminal(7,'+Masc',[1])),
	cf(1,phi(7,var(11))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(11))),
	cf(1,terminal(5,'+Indef',[1])),
	cf(1,phi(5,var(11))),
	cf(1,subtree(2,'N_SUFF_BASE',-,3)),
	cf(1,phi(2,var(11))),
	cf(1,terminal(3,'+Sg',[1])),
	cf(1,phi(3,var(11))),
	cf(1,subtree(2252,'I\'',2189,2198)),
	cf(1,phi(2252,var(0))),
	cf(1,subtree(2189,'I\'',-,1514)),
	cf(1,phi(2189,var(0))),
	cf(1,subtree(1514,'Vfin',1513,16)),
	cf(1,phi(1514,var(0))),
	cf(1,cproj(1514,var(62))),
	cf(1,subtree(1513,'Vfin',1512,18)),
	cf(1,phi(1513,var(0))),
	cf(1,cproj(1513,var(62))),
	cf(1,subtree(1512,'Vfin',-,20)),
	cf(1,phi(1512,var(0))),
	cf(1,cproj(1512,var(62))),
	cf(1,subtree(20,'V_BASE',-,21)),
	cf(1,phi(20,var(0))),
	cf(1,cproj(20,var(64))),
	cf(1,terminal(21,'anse',[14])),
	cf(1,phi(21,var(68))),
	cf(1,cproj(21,var(64))),
	cf(1,subtree(18,'V_SUFF_BASE',-,19)),
	cf(1,phi(18,var(0))),
	cf(1,terminal(19,'+Verb',[14])),
	cf(1,phi(19,var(0))),
	cf(1,subtree(16,'V_SUFF_BASE',-,17)),
	cf(1,phi(16,var(0))),
	cf(1,terminal(17,'+Pres',[14])),
	cf(1,phi(17,var(0))),
	cf(1,subtree(2198,'S',-,2206)),
	cf(1,phi(2198,var(0))),
	cf(1,subtree(2206,'VPmain',2218,1794)),
	cf(1,phi(2206,var(0))),
	cf(1,subtree(2218,'VPmain',-,1273)),
	cf(1,phi(2218,var(0))),
	cf(1,subtree(1273,'PROPP',-,53)),
	cf(1,phi(1273,var(12))),
	cf(1,subtree(53,'PROP',-,45)),
	cf(1,phi(53,var(12))),
	cf(1,terminal(45,'Abrams',[45])),
	cf(1,phi(45,var(12))),
	cf(1,subtree(1794,'PPapred',1792,779)),
	cf(1,phi(1794,var(65))),
	cf(1,subtree(1792,'PPapred',-,70)),
	cf(1,phi(1792,var(65))),
	cf(1,subtree(70,'Ppred',-,54)),
	cf(1,phi(70,var(65))),
	cf(1,terminal(54,'som',[54])),
	cf(1,phi(54,var(69))),
	cf(1,subtree(779,'AP',-,397)),
	cf(1,phi(779,var(13))),
	cf(1,subtree(397,'A',396,77)),
	cf(1,phi(397,var(13))),
	cf(1,subtree(396,'A',395,79)),
	cf(1,phi(396,var(13))),
	cf(1,subtree(395,'A',394,81)),
	cf(1,phi(395,var(13))),
	cf(1,subtree(394,'A',393,83)),
	cf(1,phi(394,var(13))),
	cf(1,subtree(393,'A',378,85)),
	cf(1,phi(393,var(13))),
	cf(1,subtree(378,'A',-,87)),
	cf(1,phi(378,var(13))),
	cf(1,subtree(87,'A_BASE',-,88)),
	cf(1,phi(87,var(13))),
	cf(1,terminal(88,'gammel',[76])),
	cf(1,phi(88,var(13))),
	cf(1,subtree(85,'A_SUFF_BASE',-,86)),
	cf(1,phi(85,var(13))),
	cf(1,terminal(86,'+Adj',[76])),
	cf(1,phi(86,var(13))),
	cf(1,subtree(83,'A_SUFF_BASE',-,84)),
	cf(1,phi(83,var(13))),
	cf(1,terminal(84,'+Pos',[76])),
	cf(1,phi(84,var(13))),
	cf(1,subtree(81,'N_SUFF_BASE',-,82)),
	cf(1,phi(81,var(13))),
	cf(1,terminal(82,'+MF',[76])),
	cf(1,phi(82,var(13))),
	cf(1,subtree(79,'N_SUFF_BASE',-,80)),
	cf(1,phi(79,var(13))),
	cf(1,terminal(80,'+Indef',[76])),
	cf(1,phi(80,var(13))),
	cf(1,subtree(77,'N_SUFF_BASE',-,78)),
	cf(1,phi(77,var(13))),
	cf(1,terminal(78,'+Sg',[76])),
	cf(1,phi(78,var(13))),
	cf(1,subtree(103,'PERIOD',-,96)),
	cf(1,phi(103,var(0))),
	cf(1,terminal(96,'.',[96])),
	cf(1,phi(96,var(0))),
	cf(1,semform_data(0,10,1,6)),
	cf(1,semform_data(1,10,1,6)),
	cf(1,semform_data(2,10,1,6)),
	cf(1,semform_data(4,20,8,13)),
	cf(1,semform_data(6,20,8,13)),
	cf(1,semform_data(7,53,14,20)),
	cf(1,semform_data(8,53,14,20)),
	cf(1,semform_data(9,53,14,20)),
	cf(1,semform_data(12,70,21,24)),
	cf(1,semform_data(13,70,21,24)),
	cf(1,semform_data(14,87,25,30)),
	cf(1,semform_data(15,87,25,30)),
	cf(1,fspan(var(0),1,32)),
	cf(1,fspan(var(11),1,7)),
	cf(1,fspan(var(65),21,32)),
	cf(1,fspan(var(12),14,20)),
	cf(1,fspan(var(13),25,32)),
	cf(1,surfaceform(1,'Browne',1,7)),
	cf(1,surfaceform(14,'anser',8,13)),
	cf(1,surfaceform(45,'Abrams',14,20)),
	cf(1,surfaceform(54,'som',21,24)),
	cf(1,surfaceform(76,'gammel',25,32)),
	cf(1,surfaceform(96,'.',31,32))
	]).

