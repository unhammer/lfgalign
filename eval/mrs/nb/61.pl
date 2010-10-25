% -*- coding: iso-8859-1 -*-

fstructure('Brownes bjeffer.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.04 CPU seconds, 86 subtrees unified'),
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
	select(A2, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',12,[var(10)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(10))),
	cf(1,eq(attr(var(0),'TOPIC'),var(10))),
	cf(1,eq(attr(var(0),'CHECK'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(12))),
	cf(1,eq(proj(var(0),'m::'),var(13))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(10),'PRED'),var(14))),
	cf(1,eq(attr(var(10),'CHECK'),var(15))),
	cf(A1,eq(attr(var(10),'GEND'),var(16))),
	cf(A1,eq(attr(var(10),'NTYPE'),var(17))),
	cf(1,eq(attr(var(10),'NUM'),var(18))),
	cf(A1,eq(attr(var(10),'REF'),var(19))),
	cf(A2,eq(attr(var(10),'SPEC'),var(20))),
	cf(1,eq(proj(var(10),'m::'),var(21))),
	cf(1,eq(attr(var(10),'CASE'),'nom')),
	cf(1,eq(attr(var(10),'DEF'),'+')),
	cf(1,eq(attr(var(10),'PERS'),'3')),
	cf(A1,eq(var(14),semform('Brownes',0,[],[]))),
	cf(A2,eq(var(14),semform('pro',6,[],[]))),
	cf(A1,eq(attr(var(15),'_DEF-MORPH'),var(22))),
	cf(A2,eq(attr(var(15),'_PREDET'),var(23))),
	cf(A1,eq(var(22),'-')),
	cf(A2,eq(var(23),'+')),
	cf(A1,eq(attr(var(16),'FEM'),'-')),
	cf(A1,eq(attr(var(16),'MASC'),'+')),
	cf(A1,eq(attr(var(16),'NEUT'),'-')),
	cf(A1,eq(attr(var(17),'NSEM'),var(24))),
	cf(A1,eq(attr(var(17),'NSYN'),'proper')),
	cf(A1,eq(attr(var(24),'PROPER'),var(25))),
	cf(A1,eq(attr(var(25),'PROPER-TYPE'),'name')),
	cf(A1,eq(var(18),'sg')),
	cf(A1,eq(var(19),'+')),
	cf(A2,eq(attr(var(20),'POSS'),var(26))),
	cf(A2,eq(attr(var(26),'PRED'),semform('Browne',3,[],[]))),
	cf(A2,eq(attr(var(26),'CHECK'),var(27))),
	cf(A2,eq(attr(var(26),'NTYPE'),var(28))),
	cf(A2,eq(proj(var(26),'m::'),var(29))),
	cf(A2,eq(attr(var(26),'CASE'),'gen')),
	cf(A2,eq(attr(var(26),'DEF'),'+')),
	cf(A2,eq(attr(var(26),'NUM'),'sg')),
	cf(A2,eq(attr(var(26),'PERS'),'3')),
	cf(A2,eq(attr(var(26),'REF'),'+')),
	cf(A2,eq(attr(var(27),'_SEL'),var(30))),
	cf(A2,eq(attr(var(30),'_ABSTRACT'),'-')),
	cf(A2,eq(attr(var(30),'_ANIM'),'+')),
	cf(A2,eq(attr(var(30),'_HUMAN'),'+')),
	cf(A2,eq(attr(var(28),'NSEM'),var(31))),
	cf(A2,eq(attr(var(28),'NSYN'),'proper')),
	cf(A2,eq(attr(var(31),'PROPER'),var(32))),
	cf(A2,eq(attr(var(32),'PROPER-TYPE'),'name')),
	cf(A2,eq(attr(var(29),'H-CONS'),var(33))),
	cf(A2,eq(attr(var(29),'RELS'),var(34))),
	cf(A2,eq(attr(var(29),'RELS_EL'),var(6))),
	cf(A2,eq(attr(var(29),'TOP'),var(35))),
	cf(A2,eq(attr(var(29),'_QUANT'),var(36))),
	cf(1,in_set(var(1),var(33))),
	cf(A1,in_set(var(2),var(33))),
	cf(A2,in_set(var(3),var(33))),
	cf(A2,in_set(var(4),var(33))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(37))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(38))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(37),'type'),'handle')),
	cf(1,eq(attr(var(38),'type'),'handle')),
	cf(A1,eq(attr(var(2),'OUTSCPD'),var(39))),
	cf(A1,eq(attr(var(2),'SC_ARG'),var(40))),
	cf(A1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,eq(attr(var(40),'type'),'handle')),
	cf(A2,eq(attr(var(3),'OUTSCPD'),var(39))),
	cf(A2,eq(attr(var(3),'SC_ARG'),var(40))),
	cf(A2,eq(attr(var(3),'relation'),'qeq')),
	cf(A2,eq(attr(var(4),'OUTSCPD'),var(41))),
	cf(A2,eq(attr(var(4),'SC_ARG'),var(42))),
	cf(A2,eq(attr(var(4),'relation'),'qeq')),
	cf(A2,eq(attr(var(41),'type'),'handle')),
	cf(A2,eq(attr(var(42),'type'),'handle')),
	cf(A2,in_set(var(5),var(34))),
	cf(A2,in_set(var(6),var(34))),
	cf(1,in_set(var(7),var(34))),
	cf(A2,in_set(var(8),var(34))),
	cf(1,in_set(var(9),var(34))),
	cf(A2,eq(attr(var(5),'ARG1'),var(43))),
	cf(A2,eq(attr(var(5),'ARG2'),var(44))),
	cf(A2,eq(attr(var(5),'LBL'),var(39))),
	cf(A2,eq(attr(var(5),'relation'),semform('poss',8,[],[]))),
	cf(A2,eq(attr(var(43),'NATGEND'),'animate')),
	cf(A2,eq(attr(var(43),'NUM'),'sg')),
	cf(A2,eq(attr(var(43),'PERS'),'3')),
	cf(A2,eq(attr(var(43),'type'),'ref-ind')),
	cf(1,eq(attr(var(44),'NUM'),var(45))),
	cf(1,eq(attr(var(44),'PERS'),'3')),
	cf(1,eq(attr(var(44),'type'),'ref-ind')),
	cf(A1,eq(var(45),'sg')),
	cf(A2,eq(var(45),var(18))),
	cf(A2,eq(attr(var(6),'ARG0'),var(43))),
	cf(A2,eq(attr(var(6),'LBL'),var(41))),
	cf(A2,eq(attr(var(6),'CARG'),'Browne')),
	cf(A2,eq(attr(var(6),'relation'),semform('named',4,[],[]))),
	cf(1,eq(attr(var(7),'ARG0'),var(44))),
	cf(1,eq(attr(var(7),'BODY'),var(46))),
	cf(1,eq(attr(var(7),'LBL'),var(47))),
	cf(1,eq(attr(var(7),'RSTR'),var(40))),
	cf(1,eq(attr(var(7),'relation'),var(48))),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,eq(attr(var(47),'type'),'handle')),
	cf(A2,eq(var(48),semform('def_explicit_q',9,[],[]))),
	cf(A1,eq(var(48),semform('proper_q',2,[],[]))),
	cf(A2,eq(attr(var(8),'ARG0'),var(43))),
	cf(A2,eq(attr(var(8),'BODY'),var(49))),
	cf(A2,eq(attr(var(8),'LBL'),var(35))),
	cf(A2,eq(attr(var(8),'RSTR'),var(42))),
	cf(A2,eq(attr(var(8),'relation'),semform('proper_q',5,[],[]))),
	cf(A2,eq(attr(var(49),'type'),'handle')),
	cf(A2,eq(attr(var(35),'type'),'handle')),
	cf(1,eq(attr(var(9),'ARG0'),var(44))),
	cf(1,eq(attr(var(9),'LBL'),var(39))),
	cf(1,eq(attr(var(9),'relation'),var(50))),
	cf(A1,eq(attr(var(9),'CARG'),'Brownes')),
	cf(A1,eq(var(50),semform('named',1,[],[]))),
	cf(A2,eq(var(50),semform('generic-nom',7,[],[]))),
	cf(A2,eq(attr(var(36),'RELS_EL'),var(8))),
	cf(A2,eq(attr(var(36),'TOP'),var(51))),
	cf(A2,eq(attr(var(51),'type'),'handle')),
	cf(1,eq(attr(var(21),'H-CONS'),var(33))),
	cf(1,eq(attr(var(21),'RELS'),var(34))),
	cf(1,eq(attr(var(21),'RELS_EL'),var(9))),
	cf(1,eq(attr(var(21),'TOP'),var(47))),
	cf(1,eq(attr(var(21),'_QUANT'),var(52))),
	cf(1,eq(attr(var(52),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(52),'TOP'),var(53))),
	cf(1,eq(attr(var(53),'type'),'handle')),
	cf(1,eq(attr(var(11),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(12),'MOOD'),'indicative')),
	cf(1,eq(attr(var(12),'TENSE'),'pres')),
	cf(1,eq(attr(var(13),'H-CONS'),var(33))),
	cf(1,eq(attr(var(13),'INDEX'),var(54))),
	cf(1,eq(attr(var(13),'RELS'),var(55))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(56))),
	cf(1,eq(attr(var(13),'TOP'),var(38))),
	cf(1,eq(attr(var(13),'_MSG'),var(57))),
	cf(1,eq(attr(var(13),'_MSGQEQ'),var(1))),
	cf(1,eq(attr(var(54),'PERF'),'-')),
	cf(1,eq(attr(var(54),'SF'),'prop')),
	cf(1,eq(attr(var(54),'TENSE'),'pres')),
	cf(1,eq(attr(var(54),'type'),'event')),
	cf(1,in_set(var(56),var(55))),
	cf(1,eq(attr(var(56),'ARG0'),var(54))),
	cf(1,eq(attr(var(56),'ARG1'),var(44))),
	cf(1,eq(attr(var(56),'LBL'),var(37))),
	cf(1,eq(attr(var(56),'_CAT'),'v')),
	cf(1,eq(attr(var(56),'relation'),semform('bjeffe',13,[],[]))),
	cf(1,eq(attr(var(57),'ARG0'),var(54))),
	cf(1,eq(attr(var(57),'LBL'),var(38))),
	cf(1,eq(proj(var(58),'m::'),var(59))),
	cf(1,eq(proj(var(60),'m::'),var(59)))
	],
	% C-Structure:
	[
	cf(1,subtree(668,'ROOT',806,38)),
	cf(1,phi(668,var(0))),
	cf(1,subtree(806,'ROOT',-,905)),
	cf(1,phi(806,var(0))),
	cf(1,subtree(905,'IP',842,884)),
	cf(1,phi(905,var(0))),
	cf(A2,subtree(842,'IP',-,217)),
	cf(A2,phi(842,var(0))),
	cf(A2,subtree(217,'POSSP',-,212)),
	cf(A2,phi(217,var(10))),
	cf(A2,subtree(212,'PROPPgen',-,13)),
	cf(A2,phi(212,var(26))),
	cf(A2,subtree(13,'PROPgen',-,1)),
	cf(A2,phi(13,var(26))),
	cf(A2,terminal(1,'Brownes',[1])),
	cf(A2,phi(1,var(61))),
	cf(A1,subtree(842,'IP',-,977)),
	cf(A1,phi(842,var(0))),
	cf(A1,subtree(977,'PROPP',-,975)),
	cf(A1,phi(977,var(10))),
	cf(A1,subtree(975,'PROP',973,2)),
	cf(A1,phi(975,var(10))),
	cf(A1,subtree(973,'PROP',971,4)),
	cf(A1,phi(973,var(10))),
	cf(A1,subtree(971,'PROP',969,6)),
	cf(A1,phi(971,var(10))),
	cf(A1,subtree(969,'PROP',966,8)),
	cf(A1,phi(969,var(10))),
	cf(A1,subtree(966,'PROP',-,10)),
	cf(A1,phi(966,var(10))),
	cf(A1,subtree(10,'PROP_BASE',-,11)),
	cf(A1,phi(10,var(10))),
	cf(A1,terminal(11,'Brownes',[1])),
	cf(A1,phi(11,var(62))),
	cf(A1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(A1,phi(8,var(10))),
	cf(A1,terminal(9,'+Prop',[1])),
	cf(A1,phi(9,var(10))),
	cf(A1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(A1,phi(6,var(10))),
	cf(A1,terminal(7,'+Masc',[1])),
	cf(A1,phi(7,var(10))),
	cf(A1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(A1,phi(4,var(10))),
	cf(A1,terminal(5,'+Indef',[1])),
	cf(A1,phi(5,var(10))),
	cf(A1,subtree(2,'N_SUFF_BASE',-,3)),
	cf(A1,phi(2,var(10))),
	cf(A1,terminal(3,'+Sg',[1])),
	cf(A1,phi(3,var(10))),
	cf(1,subtree(884,'I\'',-,272)),
	cf(1,phi(884,var(0))),
	cf(1,subtree(272,'Vfin',271,18)),
	cf(1,phi(272,var(0))),
	cf(1,cproj(272,var(58))),
	cf(1,subtree(271,'Vfin',270,20)),
	cf(1,phi(271,var(0))),
	cf(1,cproj(271,var(58))),
	cf(1,subtree(270,'Vfin',-,22)),
	cf(1,phi(270,var(0))),
	cf(1,cproj(270,var(58))),
	cf(1,subtree(22,'V_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,cproj(22,var(60))),
	cf(1,terminal(23,'bjeffe',[17])),
	cf(1,phi(23,var(0))),
	cf(1,cproj(23,var(60))),
	cf(1,subtree(20,'V_SUFF_BASE',-,21)),
	cf(1,phi(20,var(0))),
	cf(1,terminal(21,'+Verb',[17])),
	cf(1,phi(21,var(0))),
	cf(1,subtree(18,'V_SUFF_BASE',-,19)),
	cf(1,phi(18,var(0))),
	cf(1,terminal(19,'+Pres',[17])),
	cf(1,phi(19,var(0))),
	cf(1,subtree(38,'PERIOD',-,31)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(31,'.',[31])),
	cf(1,phi(31,var(0))),
	cf(A1,semform_data(0,10,1,7)),
	cf(A1,semform_data(1,10,1,7)),
	cf(A1,semform_data(2,10,1,7)),
	cf(A2,semform_data(3,13,1,8)),
	cf(A2,semform_data(4,13,1,8)),
	cf(A2,semform_data(5,13,1,8)),
	cf(A2,semform_data(6,217,1,8)),
	cf(A2,semform_data(7,217,1,8)),
	cf(A2,semform_data(8,217,1,8)),
	cf(A2,semform_data(9,217,1,8)),
	cf(1,semform_data(12,22,9,14)),
	cf(1,semform_data(13,22,9,14)),
	cf(1,fspan(var(0),1,17)),
	cf(A2,fspan(var(10),1,8)),
	cf(A2,fspan(var(26),1,8)),
	cf(A1,fspan(var(10),1,8)),
	cf(1,surfaceform(1,'Brownes',1,8)),
	cf(1,surfaceform(17,'bjeffer',9,17)),
	cf(1,surfaceform(31,'.',16,17))
	]).
