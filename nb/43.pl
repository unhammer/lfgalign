% -*- coding: iso-8859-1 -*-

fstructure('Hunden bjeffet svakt.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.06 CPU seconds, 143 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',4,[var(7)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(7))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(8))),
	cf(1,eq(attr(var(0),'TOPIC'),var(7))),
	cf(1,eq(attr(var(0),'CHECK'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(proj(var(0),'m::'),var(11))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(7),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(7),'CHECK'),var(12))),
	cf(1,eq(attr(var(7),'GEND'),var(13))),
	cf(1,eq(attr(var(7),'NTYPE'),var(14))),
	cf(1,eq(proj(var(7),'m::'),var(15))),
	cf(1,eq(attr(var(7),'CASE'),'nom')),
	cf(1,eq(attr(var(7),'DEF'),'+')),
	cf(1,eq(attr(var(7),'NUM'),'sg')),
	cf(1,eq(attr(var(7),'PERS'),'3')),
	cf(1,eq(attr(var(12),'_SEL'),var(16))),
	cf(1,eq(attr(var(12),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(12),'_NOUN'),'+')),
	cf(1,eq(attr(var(16),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(16),'_ANIM'),'+')),
	cf(1,eq(attr(var(16),'_HUMAN'),'-')),
	cf(1,eq(attr(var(13),'FEM'),'-')),
	cf(1,eq(attr(var(13),'MASC'),'+')),
	cf(1,eq(attr(var(13),'NEUT'),'-')),
	cf(1,eq(attr(var(14),'NSEM'),var(17))),
	cf(1,eq(attr(var(14),'NSYN'),'common')),
	cf(1,eq(attr(var(17),'COMMON'),'count')),
	cf(1,eq(attr(var(15),'H-CONS'),var(18))),
	cf(1,eq(attr(var(15),'RELS'),var(19))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(15),'TOP'),var(20))),
	cf(1,eq(attr(var(15),'_QUANT'),var(21))),
	cf(1,in_set(var(1),var(18))),
	cf(1,in_set(var(2),var(18))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(22))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(23))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(23),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(24))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(25))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,in_set(var(3),var(19))),
	cf(1,in_set(var(4),var(19))),
	cf(1,eq(attr(var(3),'ARG0'),var(26))),
	cf(1,eq(attr(var(3),'BODY'),var(27))),
	cf(1,eq(attr(var(3),'LBL'),var(20))),
	cf(1,eq(attr(var(3),'RSTR'),var(23))),
	cf(1,eq(attr(var(3),'relation'),semform('def_q',3,[],[]))),
	cf(1,eq(attr(var(26),'DIV'),'-')),
	cf(1,eq(attr(var(26),'GRIND'),'-')),
	cf(1,eq(attr(var(26),'NATGEND'),'gender')),
	cf(1,eq(attr(var(26),'NUM'),'sg')),
	cf(1,eq(attr(var(26),'PERS'),'3')),
	cf(1,eq(attr(var(26),'type'),'ref-ind')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(26))),
	cf(1,eq(attr(var(4),'LBL'),var(22))),
	cf(1,eq(attr(var(4),'_CAT'),'n')),
	cf(1,eq(attr(var(4),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(21),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(21),'TOP'),var(28))),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,in_set(var(29),var(8))),
	cf(1,scopes(var(29),var(9))),
	cf(1,eq(attr(var(29),'PRED'),semform('svak',6,[],[]))),
	cf(1,eq(attr(var(29),'CHECK'),var(30))),
	cf(1,eq(attr(var(29),'GEND'),var(31))),
	cf(1,eq(proj(var(29),'m::'),var(32))),
	cf(1,eq(attr(var(29),'DEF'),'-')),
	cf(1,eq(attr(var(29),'NUM'),'sg')),
	cf(1,eq(attr(var(30),'_ADVERBIAL'),'+')),
	cf(1,eq(attr(var(30),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(30),'_DEG-MORPH'),'positive')),
	cf(1,eq(attr(var(31),'FEM'),'-')),
	cf(1,eq(attr(var(31),'MASC'),'-')),
	cf(1,eq(attr(var(31),'NEUT'),'+')),
	cf(1,eq(attr(var(32),'INDEX'),var(33))),
	cf(1,eq(attr(var(32),'RELS'),var(34))),
	cf(1,eq(attr(var(32),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(33),'PERF'),'-')),
	cf(1,eq(attr(var(33),'SF'),'prop')),
	cf(1,eq(attr(var(33),'TENSE'),'past')),
	cf(1,eq(attr(var(33),'type'),'event')),
	cf(1,in_set(var(5),var(34))),
	cf(1,eq(attr(var(5),'ARG0'),var(35))),
	cf(1,eq(attr(var(5),'ARG1'),var(33))),
	cf(1,eq(attr(var(5),'LBL'),var(36))),
	cf(1,eq(attr(var(5),'_CAT'),'a')),
	cf(1,eq(attr(var(5),'relation'),semform('svak',7,[],[]))),
	cf(1,eq(attr(var(35),'PERF'),'-')),
	cf(1,eq(attr(var(35),'TENSE'),'notense')),
	cf(1,eq(attr(var(35),'type'),'event')),
	cf(1,eq(attr(var(36),'handle'),'NXTLBL')),
	cf(1,eq(proj(var(9),'m::'),var(6))),
	cf(1,eq(attr(var(9),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(9),'_MRSPTR'),'+')),
	cf(1,eq(attr(var(9),'_POSTFINADV'),'+')),
	cf(1,eq(attr(var(6),'ARG0'),var(33))),
	cf(1,eq(attr(var(6),'ARG1'),var(26))),
	cf(1,eq(attr(var(6),'LBL'),var(24))),
	cf(1,eq(attr(var(6),'_CAT'),'v')),
	cf(1,eq(attr(var(6),'relation'),semform('bjeffe',5,[],[]))),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'past')),
	cf(1,eq(attr(var(11),'H-CONS'),var(18))),
	cf(1,eq(attr(var(11),'INDEX'),var(33))),
	cf(1,eq(attr(var(11),'RELS'),var(37))),
	cf(1,eq(attr(var(11),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(11),'TOP'),var(25))),
	cf(1,eq(attr(var(11),'_MSG'),var(38))),
	cf(1,eq(attr(var(11),'_MSGQEQ'),var(2))),
	cf(1,in_set(var(5),var(37))),
	cf(1,in_set(var(6),var(37))),
	cf(1,eq(attr(var(38),'ARG0'),var(33))),
	cf(1,eq(attr(var(38),'LBL'),var(25))),
	cf(1,eq(proj(var(39),'m::'),var(40))),
	cf(1,eq(proj(var(41),'m::'),var(40))),
	cf(1,eq(proj(var(42),'m::'),var(43))),
	cf(1,eq(proj(var(44),'m::'),var(43)))
	],
	% C-Structure:
	[
	cf(1,subtree(859,'ROOT',959,88)),
	cf(1,phi(859,var(0))),
	cf(1,subtree(959,'ROOT',-,1487)),
	cf(1,phi(959,var(0))),
	cf(1,subtree(1487,'IP',1370,1485)),
	cf(1,phi(1487,var(0))),
	cf(1,subtree(1370,'IP',-,1528)),
	cf(1,phi(1370,var(0))),
	cf(1,subtree(1528,'NP',-,2)),
	cf(1,phi(1528,var(7))),
	cf(1,subtree(2,'N',284,4)),
	cf(1,phi(2,var(7))),
	cf(1,cproj(2,var(39))),
	cf(1,subtree(284,'N',280,6)),
	cf(1,phi(284,var(7))),
	cf(1,cproj(284,var(39))),
	cf(1,subtree(280,'N',276,8)),
	cf(1,phi(280,var(7))),
	cf(1,cproj(280,var(39))),
	cf(1,subtree(276,'N',273,10)),
	cf(1,phi(276,var(7))),
	cf(1,cproj(276,var(39))),
	cf(1,subtree(273,'N',-,12)),
	cf(1,phi(273,var(7))),
	cf(1,cproj(273,var(39))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(7))),
	cf(1,cproj(12,var(41))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(7))),
	cf(1,cproj(13,var(41))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(7))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(7))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(7))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(7))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(7))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(45))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(7))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(7))),
	cf(1,subtree(1485,'I\'',1438,1441)),
	cf(1,phi(1485,var(0))),
	cf(1,subtree(1438,'I\'',-,352)),
	cf(1,phi(1438,var(0))),
	cf(1,subtree(352,'Vfin',351,22)),
	cf(1,phi(352,var(0))),
	cf(1,cproj(352,var(42))),
	cf(1,subtree(351,'Vfin',350,24)),
	cf(1,phi(351,var(0))),
	cf(1,cproj(351,var(42))),
	cf(1,subtree(350,'Vfin',-,26)),
	cf(1,phi(350,var(0))),
	cf(1,cproj(350,var(42))),
	cf(1,subtree(26,'V_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,cproj(26,var(44))),
	cf(1,terminal(27,'bjeffe',[20])),
	cf(1,phi(27,var(0))),
	cf(1,cproj(27,var(44))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(25,'+Verb',[20])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(23,'+Past',[20])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(1441,'S',-,513)),
	cf(1,phi(1441,var(0))),
	cf(1,subtree(513,'AP',-,511)),
	cf(1,phi(513,var(29))),
	cf(1,subtree(511,'A',510,62)),
	cf(1,phi(511,var(29))),
	cf(1,subtree(510,'A',509,64)),
	cf(1,phi(510,var(29))),
	cf(1,subtree(509,'A',508,66)),
	cf(1,phi(509,var(29))),
	cf(1,subtree(508,'A',507,68)),
	cf(1,phi(508,var(29))),
	cf(1,subtree(507,'A',500,70)),
	cf(1,phi(507,var(29))),
	cf(1,subtree(500,'A',-,72)),
	cf(1,phi(500,var(29))),
	cf(1,subtree(72,'A_BASE',-,73)),
	cf(1,phi(72,var(29))),
	cf(1,terminal(73,'svak',[61])),
	cf(1,phi(73,var(29))),
	cf(1,subtree(70,'A_SUFF_BASE',-,71)),
	cf(1,phi(70,var(29))),
	cf(1,terminal(71,'+Adj',[61])),
	cf(1,phi(71,var(29))),
	cf(1,subtree(68,'A_SUFF_BASE',-,69)),
	cf(1,phi(68,var(29))),
	cf(1,terminal(69,'+Pos',[61])),
	cf(1,phi(69,var(29))),
	cf(1,subtree(66,'N_SUFF_BASE',-,67)),
	cf(1,phi(66,var(29))),
	cf(1,terminal(67,'+Neut',[61])),
	cf(1,phi(67,var(29))),
	cf(1,subtree(64,'N_SUFF_BASE',-,65)),
	cf(1,phi(64,var(29))),
	cf(1,terminal(65,'+Indef',[61])),
	cf(1,phi(65,var(29))),
	cf(1,subtree(62,'N_SUFF_BASE',-,63)),
	cf(1,phi(62,var(29))),
	cf(1,terminal(63,'+Sg',[61])),
	cf(1,phi(63,var(29))),
	cf(1,subtree(88,'PERIOD',-,81)),
	cf(1,phi(88,var(0))),
	cf(1,terminal(81,'.',[81])),
	cf(1,phi(81,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(1,semform_data(4,26,8,15)),
	cf(1,semform_data(5,26,8,15)),
	cf(1,semform_data(6,72,16,19)),
	cf(1,semform_data(7,72,16,19)),
	cf(1,fspan(var(0),1,22)),
	cf(1,fspan(var(7),1,7)),
	cf(1,fspan(var(29),16,22)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'bjeffet',8,15)),
	cf(1,surfaceform(61,'svakt',16,22)),
	cf(1,surfaceform(81,'.',21,22))
	]).

