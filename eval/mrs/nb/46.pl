% -*- coding: iso-8859-1 -*-

fstructure('Hunden bjeffer nå.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.05 CPU seconds, 100 subtrees unified'),
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
	cf(1,eq(attr(var(31),'PRED'),semform('_nå',6,[],[]))),
	cf(1,eq(proj(var(31),'m::'),var(32))),
	cf(1,eq(attr(var(31),'ADV-TYPE'),'temp')),
	cf(1,eq(attr(var(32),'H-CONS'),var(33))),
	cf(1,eq(attr(var(32),'RELS'),var(34))),
	cf(1,eq(attr(var(32),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(32),'_LOCARG0'),var(35))),
	cf(1,eq(attr(var(32),'_LOCLBL'),var(36))),
	cf(1,eq(attr(var(32),'_QUANT'),var(37))),
	cf(1,eq(attr(var(32),'_TOPLBL'),var(26))),
	cf(1,in_set(var(38),var(33))),
	cf(1,eq(attr(var(38),'OUTSCPD'),var(36))),
	cf(1,eq(attr(var(38),'SC_ARG'),var(39))),
	cf(1,eq(attr(var(38),'relation'),'qeq')),
	cf(1,eq(attr(var(36),'type'),'handle')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,in_set(var(5),var(34))),
	cf(1,in_set(var(6),var(34))),
	cf(1,in_set(var(7),var(34))),
	cf(1,in_set(var(8),var(34))),
	cf(1,eq(attr(var(5),'ARG0'),var(40))),
	cf(1,eq(attr(var(5),'ARG1'),var(41))),
	cf(1,eq(attr(var(5),'ARG2'),var(42))),
	cf(1,eq(attr(var(5),'LBL'),var(26))),
	cf(1,eq(attr(var(5),'relation'),semform('unspec_loc',7,[],[]))),
	cf(1,eq(attr(var(40),'PERF'),'-')),
	cf(1,eq(attr(var(40),'TENSE'),'notense')),
	cf(1,eq(attr(var(40),'type'),'event')),
	cf(1,eq(attr(var(41),'PERF'),'-')),
	cf(1,eq(attr(var(41),'SF'),'prop')),
	cf(1,eq(attr(var(41),'TENSE'),'pres')),
	cf(1,eq(attr(var(41),'type'),'event')),
	cf(1,eq(attr(var(42),'NUM'),'sg')),
	cf(1,eq(attr(var(42),'PERS'),'3')),
	cf(1,eq(attr(var(42),'type'),'ref-ind')),
	cf(1,eq(attr(var(6),'ARG0'),var(35))),
	cf(1,eq(attr(var(6),'ARG1'),var(42))),
	cf(1,eq(attr(var(6),'LBL'),var(36))),
	cf(1,eq(attr(var(6),'relation'),semform('_nå',8,[],[]))),
	cf(1,eq(attr(var(35),'type'),'event')),
	cf(1,eq(attr(var(7),'ARG0'),var(42))),
	cf(1,eq(attr(var(7),'LBL'),var(36))),
	cf(1,eq(attr(var(7),'relation'),semform('tid',10,[],[]))),
	cf(1,eq(attr(var(8),'ARG0'),var(42))),
	cf(1,eq(attr(var(8),'BODY'),var(43))),
	cf(1,eq(attr(var(8),'LBL'),var(44))),
	cf(1,eq(attr(var(8),'RSTR'),var(39))),
	cf(1,eq(attr(var(8),'relation'),semform('def_q',9,[],[]))),
	cf(1,eq(attr(var(43),'type'),'handle')),
	cf(1,eq(attr(var(44),'type'),'handle')),
	cf(1,eq(attr(var(37),'RELS'),var(34))),
	cf(1,eq(attr(var(37),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(37),'TOP'),var(45))),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(11),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(11),'_POSTFINADV'),'+')),
	cf(1,eq(attr(var(12),'MOOD'),'indicative')),
	cf(1,eq(attr(var(12),'TENSE'),'pres')),
	cf(1,eq(attr(var(13),'H-CONS'),var(20))),
	cf(1,eq(attr(var(13),'INDEX'),var(41))),
	cf(1,eq(attr(var(13),'RELS'),var(46))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(47))),
	cf(1,eq(attr(var(13),'TOP'),var(27))),
	cf(1,eq(attr(var(13),'_MSG'),var(48))),
	cf(1,eq(attr(var(13),'_MSGQEQ'),var(2))),
	cf(1,in_set(var(47),var(46))),
	cf(1,eq(attr(var(47),'ARG0'),var(41))),
	cf(1,eq(attr(var(47),'ARG1'),var(28))),
	cf(1,eq(attr(var(47),'LBL'),var(26))),
	cf(1,eq(attr(var(47),'_CAT'),'v')),
	cf(1,eq(attr(var(47),'relation'),semform('bjeffe',5,[],[]))),
	cf(1,eq(attr(var(48),'ARG0'),var(41))),
	cf(1,eq(attr(var(48),'LBL'),var(27))),
	cf(1,eq(attr(var(49),'LEFT_SISTER'),var(50))),
	cf(1,eq(attr(var(50),'RIGHT_DAUGHTER'),var(51))),
	cf(1,eq(attr(var(50),'RIGHT_SISTER'),var(49))),
	cf(1,eq(attr(var(51),'LEFT_SISTER'),var(52))),
	cf(1,eq(attr(var(51),'RIGHT_DAUGHTER'),var(53))),
	cf(1,eq(attr(var(52),'RIGHT_SISTER'),var(51))),
	cf(1,eq(attr(var(53),'LEFT_SISTER'),var(54))),
	cf(1,eq(attr(var(53),'RIGHT_DAUGHTER'),var(55))),
	cf(1,eq(attr(var(54),'RIGHT_SISTER'),var(53))),
	cf(1,eq(proj(var(54),'m::'),var(56))),
	cf(1,eq(proj(var(57),'m::'),var(58))),
	cf(1,eq(proj(var(59),'m::'),var(58))),
	cf(1,eq(proj(var(60),'m::'),var(56)))
	],
	% C-Structure:
	[
	cf(1,subtree(818,'ROOT',857,72)),
	cf(1,phi(818,var(0))),
	cf(1,subtree(857,'ROOT',-,1254)),
	cf(1,phi(857,var(0))),
	cf(1,subtree(1254,'IP',1287,1351)),
	cf(1,phi(1254,var(0))),
	cf(1,cproj(1254,var(50))),
	cf(1,subtree(1287,'IP',-,1387)),
	cf(1,phi(1287,var(0))),
	cf(1,subtree(1387,'NP',-,2)),
	cf(1,phi(1387,var(9))),
	cf(1,subtree(2,'N',268,4)),
	cf(1,phi(2,var(9))),
	cf(1,cproj(2,var(57))),
	cf(1,subtree(268,'N',264,6)),
	cf(1,phi(268,var(9))),
	cf(1,cproj(268,var(57))),
	cf(1,subtree(264,'N',260,8)),
	cf(1,phi(264,var(9))),
	cf(1,cproj(264,var(57))),
	cf(1,subtree(260,'N',257,10)),
	cf(1,phi(260,var(9))),
	cf(1,cproj(260,var(57))),
	cf(1,subtree(257,'N',-,12)),
	cf(1,phi(257,var(9))),
	cf(1,cproj(257,var(57))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(9))),
	cf(1,cproj(12,var(59))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(9))),
	cf(1,cproj(13,var(59))),
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
	cf(1,phi(7,var(61))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(9))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(9))),
	cf(1,subtree(1351,'I\'',1323,1327)),
	cf(1,phi(1351,var(0))),
	cf(1,cproj(1351,var(51))),
	cf(1,subtree(1323,'I\'',-,336)),
	cf(1,phi(1323,var(0))),
	cf(1,subtree(336,'Vfin',335,22)),
	cf(1,phi(336,var(0))),
	cf(1,cproj(336,var(54))),
	cf(1,subtree(335,'Vfin',334,24)),
	cf(1,phi(335,var(0))),
	cf(1,cproj(335,var(54))),
	cf(1,subtree(334,'Vfin',-,26)),
	cf(1,phi(334,var(0))),
	cf(1,cproj(334,var(54))),
	cf(1,subtree(26,'V_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,cproj(26,var(60))),
	cf(1,terminal(27,'bjeffe',[20])),
	cf(1,phi(27,var(0))),
	cf(1,cproj(27,var(60))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(25,'+Verb',[20])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(23,'+Pres',[20])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(1327,'S',-,64)),
	cf(1,phi(1327,var(0))),
	cf(1,cproj(1327,var(53))),
	cf(1,subtree(64,'ADVloc',-,37)),
	cf(1,phi(64,var(31))),
	cf(1,terminal(37,'nå',[37])),
	cf(1,phi(37,var(62))),
	cf(1,subtree(72,'PERIOD',-,65)),
	cf(1,phi(72,var(0))),
	cf(1,terminal(65,'.',[65])),
	cf(1,phi(65,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(1,semform_data(4,26,8,13)),
	cf(1,semform_data(5,26,8,13)),
	cf(1,semform_data(6,64,16,19)),
	cf(1,semform_data(7,64,16,19)),
	cf(1,semform_data(8,64,16,19)),
	cf(1,semform_data(9,64,16,19)),
	cf(1,semform_data(10,64,16,19)),
	cf(1,fspan(var(0),1,19)),
	cf(1,fspan(var(9),1,7)),
	cf(1,fspan(var(31),16,19)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'bjeffer',8,15)),
	cf(1,surfaceform(37,'nå',16,19)),
	cf(1,surfaceform(65,'.',18,19))
	]).

