% -*- coding: iso-8859-1 -*-

fstructure('Tjuetre hunder bjeffer.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.04 CPU seconds, 77 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',7,[var(5)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(5))),
	cf(1,eq(attr(var(0),'TOPIC'),var(5))),
	cf(1,eq(attr(var(0),'CHECK'),var(6))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(7))),
	cf(1,eq(proj(var(0),'m::'),var(8))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(5),'PRED'),semform('hund',5,[],[]))),
	cf(1,eq(attr(var(5),'CHECK'),var(9))),
	cf(1,eq(attr(var(5),'GEND'),var(10))),
	cf(1,eq(attr(var(5),'NTYPE'),var(11))),
	cf(1,eq(attr(var(5),'SPEC'),var(12))),
	cf(1,eq(proj(var(5),'m::'),var(13))),
	cf(1,eq(attr(var(5),'CASE'),'nom')),
	cf(1,eq(attr(var(5),'DEF'),'-')),
	cf(1,eq(attr(var(5),'NUM'),'pl')),
	cf(1,eq(attr(var(5),'PERS'),'3')),
	cf(1,eq(attr(var(5),'REF'),'+')),
	cf(1,eq(attr(var(9),'_SEL'),var(14))),
	cf(1,eq(attr(var(9),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(9),'_NOUN'),'+')),
	cf(1,eq(attr(var(9),'_PREDET'),'+')),
	cf(1,eq(attr(var(14),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(14),'_ANIM'),'+')),
	cf(1,eq(attr(var(14),'_HUMAN'),'-')),
	cf(1,eq(attr(var(10),'FEM'),'-')),
	cf(1,eq(attr(var(10),'MASC'),'+')),
	cf(1,eq(attr(var(10),'NEUT'),'-')),
	cf(1,eq(attr(var(11),'NSEM'),var(15))),
	cf(1,eq(attr(var(11),'NSYN'),'common')),
	cf(1,eq(attr(var(15),'COMMON'),'count')),
	cf(1,eq(attr(var(12),'NUMBER'),var(16))),
	cf(1,eq(attr(var(16),'PRED'),semform('tjuetre',0,[],[]))),
	cf(1,eq(attr(var(16),'AGRGEND'),var(10))),
	cf(1,eq(proj(var(16),'m::'),var(17))),
	cf(1,eq(attr(var(16),'AGRNUM'),'pl')),
	cf(1,eq(attr(var(16),'DIGVALUE'),'23')),
	cf(1,eq(attr(var(16),'HEADNUM'),'pl')),
	cf(1,eq(attr(var(17),'RELS'),var(18))),
	cf(1,eq(attr(var(17),'_CARD'),var(19))),
	cf(1,eq(attr(var(17),'_TOPHNDL'),var(20))),
	cf(1,in_set(var(19),var(18))),
	cf(1,eq(attr(var(19),'ARG0'),var(21))),
	cf(1,eq(attr(var(19),'ARG1'),var(22))),
	cf(1,eq(attr(var(19),'LBL'),var(20))),
	cf(1,eq(attr(var(19),'CARG'),'23')),
	cf(1,eq(attr(var(19),'relation'),semform('card',1,[],[]))),
	cf(1,eq(attr(var(21),'type'),'event')),
	cf(1,eq(attr(var(22),'DIV'),'+')),
	cf(1,eq(attr(var(22),'GRIND'),'-')),
	cf(1,eq(attr(var(22),'NATGEND'),'gender')),
	cf(1,eq(attr(var(22),'NUM'),'pl')),
	cf(1,eq(attr(var(22),'PERS'),'3')),
	cf(1,eq(attr(var(22),'type'),'ref-ind')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,eq(attr(var(13),'H-CONS'),var(23))),
	cf(1,eq(attr(var(13),'RELS'),var(24))),
	cf(1,eq(attr(var(13),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(13),'TOP'),var(25))),
	cf(1,eq(attr(var(13),'_QUANT'),var(26))),
	cf(1,in_set(var(1),var(23))),
	cf(1,in_set(var(2),var(23))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(20))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(27))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(28))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(29))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,in_set(var(3),var(24))),
	cf(1,in_set(var(4),var(24))),
	cf(1,eq(attr(var(3),'ARG0'),var(22))),
	cf(1,eq(attr(var(3),'LBL'),var(20))),
	cf(1,eq(attr(var(3),'_CAT'),'n')),
	cf(1,eq(attr(var(3),'relation'),semform('hund',6,[],[]))),
	cf(1,eq(attr(var(4),'ARG0'),var(22))),
	cf(1,eq(attr(var(4),'BODY'),var(30))),
	cf(1,eq(attr(var(4),'LBL'),var(25))),
	cf(1,eq(attr(var(4),'RSTR'),var(27))),
	cf(1,eq(attr(var(4),'relation'),semform('bare_div_q',2,[],[]))),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(26),'TOP'),var(31))),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(attr(var(6),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(7),'MOOD'),'indicative')),
	cf(1,eq(attr(var(7),'TENSE'),'pres')),
	cf(1,eq(attr(var(8),'H-CONS'),var(23))),
	cf(1,eq(attr(var(8),'INDEX'),var(32))),
	cf(1,eq(attr(var(8),'RELS'),var(33))),
	cf(1,eq(attr(var(8),'RELS_EL'),var(34))),
	cf(1,eq(attr(var(8),'TOP'),var(29))),
	cf(1,eq(attr(var(8),'_MSG'),var(35))),
	cf(1,eq(attr(var(8),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(32),'PERF'),'-')),
	cf(1,eq(attr(var(32),'SF'),'prop')),
	cf(1,eq(attr(var(32),'TENSE'),'pres')),
	cf(1,eq(attr(var(32),'type'),'event')),
	cf(1,in_set(var(34),var(33))),
	cf(1,eq(attr(var(34),'ARG0'),var(32))),
	cf(1,eq(attr(var(34),'ARG1'),var(22))),
	cf(1,eq(attr(var(34),'LBL'),var(28))),
	cf(1,eq(attr(var(34),'_CAT'),'v')),
	cf(1,eq(attr(var(34),'relation'),semform('bjeffe',8,[],[]))),
	cf(1,eq(attr(var(35),'ARG0'),var(32))),
	cf(1,eq(attr(var(35),'LBL'),var(29))),
	cf(1,eq(proj(var(36),'m::'),var(37))),
	cf(1,eq(proj(var(38),'m::'),var(37))),
	cf(1,eq(proj(var(39),'m::'),var(40))),
	cf(1,eq(proj(var(41),'m::'),var(40)))
	],
	% C-Structure:
	[
	cf(1,subtree(764,'ROOT',902,64)),
	cf(1,phi(764,var(0))),
	cf(1,subtree(902,'ROOT',-,1001)),
	cf(1,phi(902,var(0))),
	cf(1,subtree(1001,'IP',938,980)),
	cf(1,phi(1001,var(0))),
	cf(1,subtree(938,'IP',-,1242)),
	cf(1,phi(938,var(0))),
	cf(1,subtree(1242,'QuantP',1236,1120)),
	cf(1,phi(1242,var(5))),
	cf(1,subtree(1236,'QuantP',-,239)),
	cf(1,phi(1236,var(5))),
	cf(1,subtree(239,'NUMP',-,237)),
	cf(1,phi(239,var(16))),
	cf(1,subtree(237,'NUM21P',-,22)),
	cf(1,phi(237,var(16))),
	cf(1,subtree(22,'NUM21',-,3)),
	cf(1,phi(22,var(16))),
	cf(1,terminal(3,'tjuetre',[3])),
	cf(1,phi(3,var(16))),
	cf(1,subtree(1120,'NP',-,24)),
	cf(1,phi(1120,var(5))),
	cf(1,subtree(24,'N',1062,25)),
	cf(1,phi(24,var(5))),
	cf(1,cproj(24,var(36))),
	cf(1,subtree(1062,'N',1061,27)),
	cf(1,phi(1062,var(5))),
	cf(1,cproj(1062,var(36))),
	cf(1,subtree(1061,'N',1060,29)),
	cf(1,phi(1061,var(5))),
	cf(1,cproj(1061,var(36))),
	cf(1,subtree(1060,'N',1047,31)),
	cf(1,phi(1060,var(5))),
	cf(1,cproj(1060,var(36))),
	cf(1,subtree(1047,'N',-,33)),
	cf(1,phi(1047,var(5))),
	cf(1,cproj(1047,var(36))),
	cf(1,subtree(33,'N_BASE',-,34)),
	cf(1,phi(33,var(5))),
	cf(1,cproj(33,var(38))),
	cf(1,terminal(34,'hund',[23])),
	cf(1,phi(34,var(5))),
	cf(1,cproj(34,var(38))),
	cf(1,subtree(31,'N_SUFF_BASE',-,32)),
	cf(1,phi(31,var(5))),
	cf(1,terminal(32,'+Noun',[23])),
	cf(1,phi(32,var(5))),
	cf(1,subtree(29,'N_SUFF_BASE',-,30)),
	cf(1,phi(29,var(5))),
	cf(1,terminal(30,'+Masc',[23])),
	cf(1,phi(30,var(5))),
	cf(1,subtree(27,'N_SUFF_BASE',-,28)),
	cf(1,phi(27,var(5))),
	cf(1,terminal(28,'+Indef',[23])),
	cf(1,phi(28,var(5))),
	cf(1,subtree(25,'N_SUFF_BASE',-,26)),
	cf(1,phi(25,var(5))),
	cf(1,terminal(26,'+Pl',[23])),
	cf(1,phi(26,var(5))),
	cf(1,subtree(980,'I\'',-,368)),
	cf(1,phi(980,var(0))),
	cf(1,subtree(368,'Vfin',367,44)),
	cf(1,phi(368,var(0))),
	cf(1,cproj(368,var(39))),
	cf(1,subtree(367,'Vfin',366,46)),
	cf(1,phi(367,var(0))),
	cf(1,cproj(367,var(39))),
	cf(1,subtree(366,'Vfin',-,48)),
	cf(1,phi(366,var(0))),
	cf(1,cproj(366,var(39))),
	cf(1,subtree(48,'V_BASE',-,49)),
	cf(1,phi(48,var(0))),
	cf(1,cproj(48,var(41))),
	cf(1,terminal(49,'bjeffe',[43])),
	cf(1,phi(49,var(0))),
	cf(1,cproj(49,var(41))),
	cf(1,subtree(46,'V_SUFF_BASE',-,47)),
	cf(1,phi(46,var(0))),
	cf(1,terminal(47,'+Verb',[43])),
	cf(1,phi(47,var(0))),
	cf(1,subtree(44,'V_SUFF_BASE',-,45)),
	cf(1,phi(44,var(0))),
	cf(1,terminal(45,'+Pres',[43])),
	cf(1,phi(45,var(0))),
	cf(1,subtree(64,'PERIOD',-,57)),
	cf(1,phi(64,var(0))),
	cf(1,terminal(57,'.',[57])),
	cf(1,phi(57,var(0))),
	cf(1,semform_data(0,22,1,8)),
	cf(1,semform_data(1,22,1,8)),
	cf(1,semform_data(2,1236,1,8)),
	cf(1,semform_data(5,33,9,12)),
	cf(1,semform_data(6,33,9,12)),
	cf(1,semform_data(7,48,16,21)),
	cf(1,semform_data(8,48,16,21)),
	cf(1,fspan(var(0),1,24)),
	cf(1,fspan(var(5),1,15)),
	cf(1,fspan(var(16),1,8)),
	cf(1,surfaceform(3,'tjuetre',1,8)),
	cf(1,surfaceform(23,'hunder',9,15)),
	cf(1,surfaceform(43,'bjeffer',16,24)),
	cf(1,surfaceform(57,'.',23,24))
	]).

