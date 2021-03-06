% -*- coding: iso-8859-1 -*-

fstructure('Abrams overlot til Browne å bjeffe.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.19 CPU seconds, 200 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('overlate',4,[var(10),var(11),var(12)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(10))),
	cf(1,eq(attr(var(0),'OBL-BEN'),var(11))),
	cf(1,eq(attr(var(0),'XCOMP'),var(12))),
	cf(1,eq(attr(var(0),'TOPIC'),var(10))),
	cf(1,eq(attr(var(0),'CHECK'),var(13))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(14))),
	cf(1,eq(proj(var(0),'m::'),var(15))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(10),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(10),'CHECK'),var(16))),
	cf(1,eq(attr(var(10),'NTYPE'),var(17))),
	cf(1,eq(proj(var(10),'m::'),var(18))),
	cf(1,eq(attr(var(10),'CASE'),'nom')),
	cf(1,eq(attr(var(10),'DEF'),'+')),
	cf(1,eq(attr(var(10),'NUM'),'sg')),
	cf(1,eq(attr(var(10),'PERS'),'3')),
	cf(1,eq(attr(var(10),'REF'),'+')),
	cf(1,eq(attr(var(16),'_SEL'),var(19))),
	cf(1,eq(attr(var(19),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(19),'_ANIM'),'+')),
	cf(1,eq(attr(var(19),'_HUMAN'),'+')),
	cf(1,eq(attr(var(17),'NSEM'),var(20))),
	cf(1,eq(attr(var(17),'NSYN'),'proper')),
	cf(1,eq(attr(var(20),'PROPER'),var(21))),
	cf(1,eq(attr(var(21),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(18),'H-CONS'),var(22))),
	cf(1,eq(attr(var(18),'RELS'),var(23))),
	cf(1,eq(attr(var(18),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(18),'TOP'),var(24))),
	cf(1,eq(attr(var(18),'_QUANT'),var(25))),
	cf(1,in_set(var(1),var(22))),
	cf(1,in_set(var(2),var(22))),
	cf(1,in_set(var(3),var(22))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(26))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(27))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(28))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(29))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(30))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(31))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,in_set(var(4),var(23))),
	cf(1,in_set(var(5),var(23))),
	cf(1,eq(attr(var(4),'ARG0'),var(32))),
	cf(1,eq(attr(var(4),'LBL'),var(26))),
	cf(1,eq(attr(var(4),'CARG'),'Abrams')),
	cf(1,eq(attr(var(4),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(32),'NATGEND'),'animate')),
	cf(1,eq(attr(var(32),'NUM'),'sg')),
	cf(1,eq(attr(var(32),'PERS'),'3')),
	cf(1,eq(attr(var(32),'type'),'ref-ind')),
	cf(1,eq(attr(var(5),'ARG0'),var(32))),
	cf(1,eq(attr(var(5),'BODY'),var(33))),
	cf(1,eq(attr(var(5),'LBL'),var(24))),
	cf(1,eq(attr(var(5),'RSTR'),var(27))),
	cf(1,eq(attr(var(5),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(25),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(25),'TOP'),var(34))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(11),'PRED'),semform('Browne',9,[],[]))),
	cf(1,eq(attr(var(11),'CHECK'),var(35))),
	cf(1,eq(attr(var(11),'NTYPE'),var(36))),
	cf(1,eq(proj(var(11),'m::'),var(37))),
	cf(1,eq(attr(var(11),'CASE'),'obl')),
	cf(1,eq(attr(var(11),'DEF'),'+')),
	cf(1,eq(attr(var(11),'NUM'),'sg')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(1,eq(attr(var(11),'PFORM'),'til')),
	cf(1,eq(attr(var(11),'POSS-TYPE'),'pp')),
	cf(1,eq(attr(var(11),'PTYPE'),'nosem')),
	cf(1,eq(attr(var(11),'REF'),'+')),
	cf(1,eq(attr(var(35),'_ANTECED'),var(38))),
	cf(1,eq(attr(var(35),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(35),'_PREPEXISTS'),'+')),
	cf(1,eq(proj(var(38),'m::'),var(39))),
	cf(1,eq(attr(var(39),'ARG0'),var(40))),
	cf(1,eq(attr(var(36),'NSEM'),var(41))),
	cf(1,eq(attr(var(36),'NSYN'),'proper')),
	cf(1,eq(attr(var(41),'PROPER'),var(42))),
	cf(1,eq(attr(var(42),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(37),'H-CONS'),var(43))),
	cf(1,eq(attr(var(37),'RELS'),var(44))),
	cf(1,eq(attr(var(37),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(37),'TOP'),var(45))),
	cf(1,eq(attr(var(37),'_ANTECEDVAR'),var(40))),
	cf(1,eq(attr(var(37),'_QUANT'),var(46))),
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
	cf(1,eq(attr(var(6),'CARG'),'Browne')),
	cf(1,eq(attr(var(6),'relation'),semform('named',10,[],[]))),
	cf(1,eq(attr(var(50),'NUM'),'sg')),
	cf(1,eq(attr(var(50),'PERS'),'3')),
	cf(1,eq(attr(var(50),'type'),'ref-ind')),
	cf(1,eq(attr(var(7),'ARG0'),var(50))),
	cf(1,eq(attr(var(7),'BODY'),var(51))),
	cf(1,eq(attr(var(7),'LBL'),var(45))),
	cf(1,eq(attr(var(7),'RSTR'),var(49))),
	cf(1,eq(attr(var(7),'relation'),semform('proper_q',11,[],[]))),
	cf(1,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(46),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(46),'TOP'),var(52))),
	cf(1,eq(attr(var(52),'type'),'handle')),
	cf(1,eq(attr(var(12),'PRED'),semform('bjeffe',12,[var(11)],[]))),
	cf(1,eq(attr(var(12),'SUBJ'),var(11))),
	cf(1,eq(attr(var(12),'CHECK'),var(53))),
	cf(1,eq(attr(var(12),'GEND'),var(54))),
	cf(1,eq(proj(var(12),'m::'),var(55))),
	cf(1,eq(attr(var(12),'COMP-FORM'),'å')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'VFORM'),'inf')),
	cf(1,eq(attr(var(12),'VTYPE'),'main')),
	cf(1,eq(attr(var(53),'_TOPVP'),'-')),
	cf(1,eq(attr(var(54),'FEM'),'-')),
	cf(1,eq(attr(var(54),'MASC'),'-')),
	cf(1,eq(attr(var(54),'NEUT'),'+')),
	cf(1,eq(attr(var(55),'H-CONS'),var(43))),
	cf(1,eq(attr(var(55),'INDEX'),var(56))),
	cf(1,eq(attr(var(55),'RELS'),var(57))),
	cf(1,eq(attr(var(55),'RELS_EL'),var(58))),
	cf(1,eq(attr(var(55),'TOP'),var(31))),
	cf(1,eq(attr(var(55),'_MSG'),var(59))),
	cf(1,eq(attr(var(55),'_MSGQEQ'),var(3))),
	cf(1,eq(attr(var(56),'PERF'),'-')),
	cf(1,eq(attr(var(56),'TENSE'),'notense')),
	cf(1,eq(attr(var(56),'type'),'event')),
	cf(1,in_set(var(58),var(57))),
	cf(1,eq(attr(var(58),'ARG0'),var(56))),
	cf(1,eq(attr(var(58),'ARG1'),var(50))),
	cf(1,eq(attr(var(58),'LBL'),var(30))),
	cf(1,eq(attr(var(58),'_CAT'),'v')),
	cf(1,eq(attr(var(58),'relation'),semform('bjeffe',13,[],[]))),
	cf(1,eq(attr(var(59),'ARG0'),var(56))),
	cf(1,eq(attr(var(59),'LBL'),var(31))),
	cf(1,eq(attr(var(13),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(14),'MOOD'),'indicative')),
	cf(1,eq(attr(var(14),'TENSE'),'past')),
	cf(1,eq(attr(var(15),'H-CONS'),var(22))),
	cf(1,eq(attr(var(15),'INDEX'),var(60))),
	cf(1,eq(attr(var(15),'RELS'),var(61))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(15),'TOP'),var(29))),
	cf(1,eq(attr(var(15),'_MSG'),var(62))),
	cf(1,eq(attr(var(15),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(60),'PERF'),'-')),
	cf(1,eq(attr(var(60),'SF'),'prop')),
	cf(1,eq(attr(var(60),'TENSE'),'past')),
	cf(1,eq(attr(var(60),'type'),'event')),
	cf(1,in_set(var(8),var(61))),
	cf(1,in_set(var(9),var(61))),
	cf(1,eq(attr(var(8),'ARG0'),var(60))),
	cf(1,eq(attr(var(8),'ARG1'),var(32))),
	cf(1,eq(attr(var(8),'ARG2'),var(50))),
	cf(1,eq(attr(var(8),'ARG3'),var(31))),
	cf(1,eq(attr(var(8),'LBL'),var(28))),
	cf(1,eq(attr(var(8),'_CAT'),'v')),
	cf(1,eq(attr(var(8),'relation'),semform('overlate',5,[],[]))),
	cf(1,eq(attr(var(9),'ARG0'),var(63))),
	cf(1,eq(attr(var(9),'ARG2'),var(50))),
	cf(1,eq(attr(var(9),'LBL'),var(28))),
	cf(1,eq(attr(var(9),'_CAT'),'p_sel')),
	cf(1,eq(attr(var(9),'relation'),semform('til',6,[],[]))),
	cf(1,eq(attr(var(63),'type'),'event')),
	cf(1,eq(attr(var(62),'ARG0'),var(60))),
	cf(1,eq(attr(var(62),'LBL'),var(29))),
	cf(1,eq(proj(var(64),'m::'),var(65))),
	cf(1,eq(proj(var(66),'m::'),var(65))),
	cf(1,eq(proj(var(67),'m::'),var(68))),
	cf(1,eq(proj(var(69),'m::'),var(68)))
	],
	% C-Structure:
	[
	cf(1,subtree(1061,'ROOT',1084,117)),
	cf(1,phi(1061,var(0))),
	cf(1,subtree(1084,'ROOT',-,1190)),
	cf(1,phi(1084,var(0))),
	cf(1,subtree(1190,'IP',2212,2884)),
	cf(1,phi(1190,var(0))),
	cf(1,subtree(2212,'IP',-,290)),
	cf(1,phi(2212,var(0))),
	cf(1,subtree(290,'PROPP',-,13)),
	cf(1,phi(290,var(10))),
	cf(1,subtree(13,'PROP',-,1)),
	cf(1,phi(13,var(10))),
	cf(1,terminal(1,'Abrams',[1])),
	cf(1,phi(1,var(70))),
	cf(1,subtree(2884,'I\'',2771,2778)),
	cf(1,phi(2884,var(0))),
	cf(1,subtree(2771,'I\'',-,2355)),
	cf(1,phi(2771,var(0))),
	cf(1,subtree(2355,'Vfin',2354,17)),
	cf(1,phi(2355,var(0))),
	cf(1,cproj(2355,var(64))),
	cf(1,subtree(2354,'Vfin',2353,19)),
	cf(1,phi(2354,var(0))),
	cf(1,cproj(2354,var(64))),
	cf(1,subtree(2353,'Vfin',-,21)),
	cf(1,phi(2353,var(0))),
	cf(1,cproj(2353,var(64))),
	cf(1,subtree(21,'V_BASE',-,22)),
	cf(1,phi(21,var(0))),
	cf(1,cproj(21,var(66))),
	cf(1,terminal(22,'overlate',[15])),
	cf(1,phi(22,var(71))),
	cf(1,cproj(22,var(66))),
	cf(1,subtree(19,'V_SUFF_BASE',-,20)),
	cf(1,phi(19,var(0))),
	cf(1,terminal(20,'+Verb',[15])),
	cf(1,phi(20,var(0))),
	cf(1,subtree(17,'V_SUFF_BASE',-,18)),
	cf(1,phi(17,var(0))),
	cf(1,terminal(18,'+Past',[15])),
	cf(1,phi(18,var(0))),
	cf(1,subtree(2778,'S',-,2786)),
	cf(1,phi(2778,var(0))),
	cf(1,subtree(2786,'VPmain',2798,1661)),
	cf(1,phi(2786,var(0))),
	cf(1,subtree(2798,'VPmain',-,1920)),
	cf(1,phi(2798,var(0))),
	cf(1,subtree(1920,'PPtil',1919,1782)),
	cf(1,phi(1920,var(11))),
	cf(1,subtree(1919,'PPtil',-,48)),
	cf(1,phi(1919,var(11))),
	cf(1,subtree(48,'Ptil',-,30)),
	cf(1,phi(48,var(11))),
	cf(1,terminal(30,'til',[30])),
	cf(1,phi(30,var(11))),
	cf(1,subtree(1782,'PROPP',-,1235)),
	cf(1,phi(1782,var(11))),
	cf(1,subtree(1235,'PROP',1234,52)),
	cf(1,phi(1235,var(11))),
	cf(1,subtree(1234,'PROP',1231,54)),
	cf(1,phi(1234,var(11))),
	cf(1,subtree(1231,'PROP',-,56)),
	cf(1,phi(1231,var(11))),
	cf(1,subtree(56,'PROP_BASE',-,57)),
	cf(1,phi(56,var(11))),
	cf(1,terminal(57,'Browne',[51])),
	cf(1,phi(57,var(72))),
	cf(1,subtree(54,'N_SUFF_BASE',-,55)),
	cf(1,phi(54,var(11))),
	cf(1,terminal(55,'+Prop',[51])),
	cf(1,phi(55,var(11))),
	cf(1,subtree(52,'N_SUFF_BASE',-,53)),
	cf(1,phi(52,var(11))),
	cf(1,terminal(53,'+Indef',[51])),
	cf(1,phi(53,var(11))),
	cf(1,subtree(1661,'VP\'',1660,1478)),
	cf(1,phi(1661,var(12))),
	cf(1,subtree(1660,'VP\'',-,93)),
	cf(1,phi(1660,var(12))),
	cf(1,subtree(93,'PARTinf',-,59)),
	cf(1,phi(93,var(12))),
	cf(1,terminal(59,'å',[59])),
	cf(1,phi(59,var(12))),
	cf(1,subtree(1478,'VP',-,1130)),
	cf(1,phi(1478,var(12))),
	cf(1,subtree(1130,'V',1129,97)),
	cf(1,phi(1130,var(12))),
	cf(1,cproj(1130,var(67))),
	cf(1,subtree(1129,'V',1128,99)),
	cf(1,phi(1129,var(12))),
	cf(1,cproj(1129,var(67))),
	cf(1,subtree(1128,'V',-,101)),
	cf(1,phi(1128,var(12))),
	cf(1,cproj(1128,var(67))),
	cf(1,subtree(101,'V_BASE',-,102)),
	cf(1,phi(101,var(12))),
	cf(1,cproj(101,var(69))),
	cf(1,terminal(102,'bjeffe',[96])),
	cf(1,phi(102,var(12))),
	cf(1,cproj(102,var(69))),
	cf(1,subtree(99,'V_SUFF_BASE',-,100)),
	cf(1,phi(99,var(12))),
	cf(1,terminal(100,'+Verb',[96])),
	cf(1,phi(100,var(12))),
	cf(1,subtree(97,'V_SUFF_BASE',-,98)),
	cf(1,phi(97,var(12))),
	cf(1,terminal(98,'+Infin',[96])),
	cf(1,phi(98,var(12))),
	cf(1,subtree(117,'PERIOD',-,110)),
	cf(1,phi(117,var(0))),
	cf(1,terminal(110,'.',[110])),
	cf(1,phi(110,var(0))),
	cf(1,semform_data(0,13,1,7)),
	cf(1,semform_data(1,13,1,7)),
	cf(1,semform_data(2,13,1,7)),
	cf(1,semform_data(4,21,8,15)),
	cf(1,semform_data(5,21,8,15)),
	cf(1,semform_data(6,21,8,15)),
	cf(1,semform_data(9,56,20,25)),
	cf(1,semform_data(10,56,20,25)),
	cf(1,semform_data(11,56,20,25)),
	cf(1,semform_data(12,101,29,34)),
	cf(1,semform_data(13,101,29,34)),
	cf(1,fspan(var(0),1,36)),
	cf(1,fspan(var(10),1,7)),
	cf(1,fspan(var(12),27,36)),
	cf(1,fspan(var(11),16,26)),
	cf(1,surfaceform(1,'Abrams',1,7)),
	cf(1,surfaceform(15,'overlot',8,15)),
	cf(1,surfaceform(30,'til',16,19)),
	cf(1,surfaceform(51,'Browne',20,26)),
	cf(1,surfaceform(59,'å',27,28)),
	cf(1,surfaceform(96,'bjeffe',29,36)),
	cf(1,surfaceform(110,'.',35,36))
	]).

