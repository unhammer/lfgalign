% -*- coding: iso-8859-1 -*-

fstructure('Det plaget Abrams at Browne bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.10 CPU seconds, 164 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('plage',0,[var(8),var(9)],[var(10)]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(10))),
	cf(1,eq(attr(var(0),'OBJ'),var(9))),
	cf(1,eq(attr(var(0),'COMP'),var(8))),
	cf(1,eq(attr(var(0),'CHECK'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(12))),
	cf(1,eq(proj(var(0),'m::'),var(13))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(10),'GEND'),var(14))),
	cf(1,eq(attr(var(10),'NTYPE'),var(15))),
	cf(1,eq(proj(var(10),'m::'),var(16))),
	cf(1,eq(attr(var(10),'NUM'),'sg')),
	cf(1,eq(attr(var(10),'PERS'),'3')),
	cf(1,eq(attr(var(10),'PRON-FORM'),'det')),
	cf(1,eq(attr(var(10),'PRON-TYPE'),'expl_')),
	cf(1,eq(attr(var(10),'REF'),'-')),
	cf(1,eq(attr(var(14),'FEM'),'-')),
	cf(1,eq(attr(var(14),'MASC'),'-')),
	cf(1,eq(attr(var(14),'NEUT'),'+')),
	cf(1,eq(attr(var(15),'NSYN'),'pronoun')),
	cf(1,eq(attr(var(16),'RELS_EL'),var(17))),
	cf(1,eq(attr(var(17),'ARG0'),var(18))),
	cf(1,eq(attr(var(9),'PRED'),semform('Abrams',2,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(19))),
	cf(1,eq(attr(var(9),'NTYPE'),var(20))),
	cf(1,eq(proj(var(9),'m::'),var(21))),
	cf(1,eq(attr(var(9),'CASE'),'obl')),
	cf(1,eq(attr(var(9),'DEF'),'+')),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(9),'REF'),'+')),
	cf(1,eq(attr(var(19),'_SEL'),var(22))),
	cf(1,eq(attr(var(19),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(22),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(22),'_ANIM'),'+')),
	cf(1,eq(attr(var(22),'_HUMAN'),'+')),
	cf(1,eq(attr(var(20),'NSEM'),var(23))),
	cf(1,eq(attr(var(20),'NSYN'),'proper')),
	cf(1,eq(attr(var(23),'PROPER'),var(24))),
	cf(1,eq(attr(var(24),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(21),'H-CONS'),var(25))),
	cf(1,eq(attr(var(21),'RELS'),var(26))),
	cf(1,eq(attr(var(21),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(21),'TOP'),var(27))),
	cf(1,eq(attr(var(21),'_ANTECEDVAR'),var(18))),
	cf(1,eq(attr(var(21),'_QUANT'),var(28))),
	cf(1,in_set(var(29),var(25))),
	cf(1,eq(attr(var(29),'OUTSCPD'),var(30))),
	cf(1,eq(attr(var(29),'SC_ARG'),var(31))),
	cf(1,eq(attr(var(29),'relation'),'qeq')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,in_set(var(1),var(26))),
	cf(1,in_set(var(2),var(26))),
	cf(1,eq(attr(var(1),'ARG0'),var(32))),
	cf(1,eq(attr(var(1),'LBL'),var(30))),
	cf(1,eq(attr(var(1),'CARG'),'Abrams')),
	cf(1,eq(attr(var(1),'relation'),semform('named',3,[],[]))),
	cf(1,eq(attr(var(32),'NATGEND'),'animate')),
	cf(1,eq(attr(var(32),'NUM'),'sg')),
	cf(1,eq(attr(var(32),'PERS'),'3')),
	cf(1,eq(attr(var(32),'type'),'ref-ind')),
	cf(1,eq(attr(var(2),'ARG0'),var(32))),
	cf(1,eq(attr(var(2),'BODY'),var(33))),
	cf(1,eq(attr(var(2),'LBL'),var(27))),
	cf(1,eq(attr(var(2),'RSTR'),var(31))),
	cf(1,eq(attr(var(2),'relation'),semform('proper_q',4,[],[]))),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(28),'RELS_EL'),var(2))),
	cf(1,eq(attr(var(28),'TOP'),var(34))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(8),'PRED'),semform('bjeffe',11,[var(35)],[]))),
	cf(1,eq(attr(var(8),'SUBJ'),var(35))),
	cf(1,eq(attr(var(8),'CHECK'),var(36))),
	cf(1,eq(attr(var(8),'GEND'),var(37))),
	cf(1,eq(attr(var(8),'TNS-ASP'),var(38))),
	cf(1,eq(proj(var(8),'m::'),var(39))),
	cf(1,eq(attr(var(8),'CLAUSE-TYPE'),'nominal')),
	cf(1,eq(attr(var(8),'COMP-FORM'),'at')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'TOPCP'),'-')),
	cf(1,eq(attr(var(8),'VFORM'),'fin')),
	cf(1,eq(attr(var(8),'VTYPE'),'main')),
	cf(1,eq(attr(var(35),'PRED'),semform('Browne',7,[],[]))),
	cf(1,eq(attr(var(35),'CHECK'),var(40))),
	cf(1,eq(attr(var(35),'NTYPE'),var(41))),
	cf(1,eq(proj(var(35),'m::'),var(42))),
	cf(1,eq(attr(var(35),'CASE'),'nom')),
	cf(1,eq(attr(var(35),'DEF'),'+')),
	cf(1,eq(attr(var(35),'NUM'),'sg')),
	cf(1,eq(attr(var(35),'PERS'),'3')),
	cf(1,eq(attr(var(35),'REF'),'+')),
	cf(1,eq(attr(var(40),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(41),'NSEM'),var(43))),
	cf(1,eq(attr(var(41),'NSYN'),'proper')),
	cf(1,eq(attr(var(43),'PROPER'),var(44))),
	cf(1,eq(attr(var(44),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(42),'H-CONS'),var(45))),
	cf(1,eq(attr(var(42),'RELS'),var(46))),
	cf(1,eq(attr(var(42),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(42),'TOP'),var(47))),
	cf(1,eq(attr(var(42),'_QUANT'),var(48))),
	cf(1,in_set(var(3),var(45))),
	cf(1,in_set(var(4),var(45))),
	cf(1,in_set(var(5),var(45))),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(49))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(50))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(49),'type'),'handle')),
	cf(1,eq(attr(var(50),'type'),'handle')),
	cf(1,eq(attr(var(4),'OUTSCPD'),var(51))),
	cf(1,eq(attr(var(4),'SC_ARG'),var(52))),
	cf(1,eq(attr(var(4),'relation'),'qeq')),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(1,eq(attr(var(5),'OUTSCPD'),var(53))),
	cf(1,eq(attr(var(5),'SC_ARG'),var(54))),
	cf(1,eq(attr(var(5),'relation'),'qeq')),
	cf(1,eq(attr(var(53),'type'),'handle')),
	cf(1,eq(attr(var(54),'type'),'handle')),
	cf(1,in_set(var(6),var(46))),
	cf(1,in_set(var(7),var(46))),
	cf(1,eq(attr(var(6),'ARG0'),var(55))),
	cf(1,eq(attr(var(6),'LBL'),var(49))),
	cf(1,eq(attr(var(6),'CARG'),'Browne')),
	cf(1,eq(attr(var(6),'relation'),semform('named',8,[],[]))),
	cf(1,eq(attr(var(55),'NUM'),'sg')),
	cf(1,eq(attr(var(55),'PERS'),'3')),
	cf(1,eq(attr(var(55),'type'),'ref-ind')),
	cf(1,eq(attr(var(7),'ARG0'),var(55))),
	cf(1,eq(attr(var(7),'BODY'),var(56))),
	cf(1,eq(attr(var(7),'LBL'),var(47))),
	cf(1,eq(attr(var(7),'RSTR'),var(50))),
	cf(1,eq(attr(var(7),'relation'),semform('proper_q',9,[],[]))),
	cf(1,eq(attr(var(56),'type'),'handle')),
	cf(1,eq(attr(var(47),'type'),'handle')),
	cf(1,eq(attr(var(48),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(48),'TOP'),var(57))),
	cf(1,eq(attr(var(57),'type'),'handle')),
	cf(1,eq(attr(var(36),'_MAIN-CL'),'-')),
	cf(1,eq(attr(var(37),'FEM'),'-')),
	cf(1,eq(attr(var(37),'MASC'),'-')),
	cf(1,eq(attr(var(37),'NEUT'),'+')),
	cf(1,eq(attr(var(38),'MOOD'),'indicative')),
	cf(1,eq(attr(var(38),'TENSE'),'past')),
	cf(1,eq(attr(var(39),'H-CONS'),var(45))),
	cf(1,eq(attr(var(39),'INDEX'),var(58))),
	cf(1,eq(attr(var(39),'RELS'),var(59))),
	cf(1,eq(attr(var(39),'RELS_EL'),var(60))),
	cf(1,eq(attr(var(39),'TOP'),var(54))),
	cf(1,eq(attr(var(39),'_MSG'),var(61))),
	cf(1,eq(attr(var(39),'_MSGQEQ'),var(5))),
	cf(1,eq(attr(var(58),'PERF'),'-')),
	cf(1,eq(attr(var(58),'SF'),'prop')),
	cf(1,eq(attr(var(58),'TENSE'),'past')),
	cf(1,eq(attr(var(58),'type'),'event')),
	cf(1,in_set(var(60),var(59))),
	cf(1,eq(attr(var(60),'ARG0'),var(58))),
	cf(1,eq(attr(var(60),'ARG1'),var(55))),
	cf(1,eq(attr(var(60),'LBL'),var(53))),
	cf(1,eq(attr(var(60),'_CAT'),'v')),
	cf(1,eq(attr(var(60),'relation'),semform('bjeffe',12,[],[]))),
	cf(1,eq(attr(var(61),'ARG0'),var(58))),
	cf(1,eq(attr(var(61),'LBL'),var(54))),
	cf(1,eq(attr(var(11),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(11),'_VCONSTR'),'extrapos')),
	cf(1,eq(attr(var(12),'MOOD'),'indicative')),
	cf(1,eq(attr(var(12),'TENSE'),'past')),
	cf(1,eq(attr(var(13),'H-CONS'),var(45))),
	cf(1,eq(attr(var(13),'INDEX'),var(62))),
	cf(1,eq(attr(var(13),'RELS'),var(63))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(64))),
	cf(1,eq(attr(var(13),'TOP'),var(52))),
	cf(1,eq(attr(var(13),'_MSG'),var(65))),
	cf(1,eq(attr(var(13),'_MSGQEQ'),var(4))),
	cf(1,eq(attr(var(62),'PERF'),'-')),
	cf(1,eq(attr(var(62),'SF'),'prop')),
	cf(1,eq(attr(var(62),'TENSE'),'past')),
	cf(1,eq(attr(var(62),'type'),'event')),
	cf(1,in_set(var(64),var(63))),
	cf(1,eq(attr(var(64),'ARG0'),var(62))),
	cf(1,eq(attr(var(64),'ARG1'),var(54))),
	cf(1,eq(attr(var(64),'ARG2'),var(32))),
	cf(1,eq(attr(var(64),'LBL'),var(51))),
	cf(1,eq(attr(var(64),'_CAT'),'v')),
	cf(1,eq(attr(var(64),'relation'),semform('plage',1,[],[]))),
	cf(1,eq(attr(var(65),'ARG0'),var(62))),
	cf(1,eq(attr(var(65),'LBL'),var(52))),
	cf(1,eq(proj(var(66),'m::'),var(67))),
	cf(1,eq(proj(var(68),'m::'),var(67))),
	cf(1,eq(proj(var(69),'m::'),var(70))),
	cf(1,eq(proj(var(71),'m::'),var(70)))
	],
	% C-Structure:
	[
	cf(1,subtree(2425,'ROOT',2449,146)),
	cf(1,phi(2425,var(0))),
	cf(1,subtree(2449,'ROOT',-,2602)),
	cf(1,phi(2449,var(0))),
	cf(1,subtree(2602,'IP',2709,2938)),
	cf(1,phi(2602,var(0))),
	cf(1,subtree(2709,'IP',-,26)),
	cf(1,phi(2709,var(0))),
	cf(1,subtree(26,'PRONexpl',-,3)),
	cf(1,phi(26,var(10))),
	cf(1,terminal(3,'det',[3])),
	cf(1,phi(3,var(10))),
	cf(1,subtree(2938,'I\'',2835,2842)),
	cf(1,phi(2938,var(0))),
	cf(1,subtree(2835,'I\'',-,1729)),
	cf(1,phi(2835,var(0))),
	cf(1,subtree(1729,'Vfin',1728,44)),
	cf(1,phi(1729,var(0))),
	cf(1,cproj(1729,var(66))),
	cf(1,subtree(1728,'Vfin',1727,46)),
	cf(1,phi(1728,var(0))),
	cf(1,cproj(1728,var(66))),
	cf(1,subtree(1727,'Vfin',-,48)),
	cf(1,phi(1727,var(0))),
	cf(1,cproj(1727,var(66))),
	cf(1,subtree(48,'V_BASE',-,49)),
	cf(1,phi(48,var(0))),
	cf(1,cproj(48,var(68))),
	cf(1,terminal(49,'plage',[42])),
	cf(1,phi(49,var(72))),
	cf(1,cproj(49,var(68))),
	cf(1,subtree(46,'V_SUFF_BASE',-,47)),
	cf(1,phi(46,var(0))),
	cf(1,terminal(47,'+Verb',[42])),
	cf(1,phi(47,var(0))),
	cf(1,subtree(44,'V_SUFF_BASE',-,45)),
	cf(1,phi(44,var(0))),
	cf(1,terminal(45,'+Past',[42])),
	cf(1,phi(45,var(0))),
	cf(1,subtree(2842,'S',-,2850)),
	cf(1,phi(2842,var(0))),
	cf(1,subtree(2850,'VPmain',2869,2185)),
	cf(1,phi(2850,var(0))),
	cf(1,subtree(2869,'VPmain',-,2292)),
	cf(1,phi(2869,var(0))),
	cf(1,subtree(2292,'PROPP',-,75)),
	cf(1,phi(2292,var(9))),
	cf(1,subtree(75,'PROP',-,67)),
	cf(1,phi(75,var(9))),
	cf(1,terminal(67,'Abrams',[67])),
	cf(1,phi(67,var(9))),
	cf(1,subtree(2185,'CPnom',2184,1567)),
	cf(1,phi(2185,var(8))),
	cf(1,subtree(2184,'CPnom',-,90)),
	cf(1,phi(2184,var(8))),
	cf(1,subtree(90,'Cnom',-,76)),
	cf(1,phi(90,var(8))),
	cf(1,terminal(76,'at',[76])),
	cf(1,phi(76,var(8))),
	cf(1,subtree(1567,'Ssub',1564,1110)),
	cf(1,phi(1567,var(8))),
	cf(1,subtree(1564,'Ssub',-,1596)),
	cf(1,phi(1564,var(8))),
	cf(1,subtree(1596,'PROPP',-,869)),
	cf(1,phi(1596,var(35))),
	cf(1,subtree(869,'PROP',868,92)),
	cf(1,phi(869,var(35))),
	cf(1,subtree(868,'PROP',865,94)),
	cf(1,phi(868,var(35))),
	cf(1,subtree(865,'PROP',-,96)),
	cf(1,phi(865,var(35))),
	cf(1,subtree(96,'PROP_BASE',-,97)),
	cf(1,phi(96,var(35))),
	cf(1,terminal(97,'Browne',[91])),
	cf(1,phi(97,var(73))),
	cf(1,subtree(94,'N_SUFF_BASE',-,95)),
	cf(1,phi(94,var(35))),
	cf(1,terminal(95,'+Prop',[91])),
	cf(1,phi(95,var(35))),
	cf(1,subtree(92,'N_SUFF_BASE',-,93)),
	cf(1,phi(92,var(35))),
	cf(1,terminal(93,'+Indef',[91])),
	cf(1,phi(93,var(35))),
	cf(1,subtree(1110,'VPfin',-,1106)),
	cf(1,phi(1110,var(8))),
	cf(1,subtree(1106,'Vfin',1105,102)),
	cf(1,phi(1106,var(8))),
	cf(1,cproj(1106,var(69))),
	cf(1,subtree(1105,'Vfin',1104,104)),
	cf(1,phi(1105,var(8))),
	cf(1,cproj(1105,var(69))),
	cf(1,subtree(1104,'Vfin',-,106)),
	cf(1,phi(1104,var(8))),
	cf(1,cproj(1104,var(69))),
	cf(1,subtree(106,'V_BASE',-,107)),
	cf(1,phi(106,var(8))),
	cf(1,cproj(106,var(71))),
	cf(1,terminal(107,'bjeffe',[101])),
	cf(1,phi(107,var(8))),
	cf(1,cproj(107,var(71))),
	cf(1,subtree(104,'V_SUFF_BASE',-,105)),
	cf(1,phi(104,var(8))),
	cf(1,terminal(105,'+Verb',[101])),
	cf(1,phi(105,var(8))),
	cf(1,subtree(102,'V_SUFF_BASE',-,103)),
	cf(1,phi(102,var(8))),
	cf(1,terminal(103,'+Past',[101])),
	cf(1,phi(103,var(8))),
	cf(1,subtree(146,'PERIOD',-,139)),
	cf(1,phi(146,var(0))),
	cf(1,terminal(139,'.',[139])),
	cf(1,phi(139,var(0))),
	cf(1,semform_data(0,48,5,9)),
	cf(1,semform_data(1,48,5,9)),
	cf(1,semform_data(2,75,12,18)),
	cf(1,semform_data(3,75,12,18)),
	cf(1,semform_data(4,75,12,18)),
	cf(1,semform_data(7,96,22,27)),
	cf(1,semform_data(8,96,22,27)),
	cf(1,semform_data(9,96,22,27)),
	cf(1,semform_data(11,106,29,36)),
	cf(1,semform_data(12,106,29,36)),
	cf(1,fspan(var(0),1,37)),
	cf(1,fspan(var(10),1,4)),
	cf(1,fspan(var(8),19,37)),
	cf(1,fspan(var(9),12,18)),
	cf(1,fspan(var(35),22,28)),
	cf(1,surfaceform(3,'det',1,4)),
	cf(1,surfaceform(42,'plaget',5,11)),
	cf(1,surfaceform(67,'Abrams',12,18)),
	cf(1,surfaceform(76,'at',19,21)),
	cf(1,surfaceform(91,'Browne',22,28)),
	cf(1,surfaceform(101,'bjeffet',29,37)),
	cf(1,surfaceform(139,'.',36,37))
	]).

