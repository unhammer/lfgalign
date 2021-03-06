% -*- coding: iso-8859-1 -*-

fstructure('Tobakkshavehunden bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.04 CPU seconds, 78 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',4,[var(5)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(5))),
	cf(1,eq(attr(var(0),'TOPIC'),var(5))),
	cf(1,eq(attr(var(0),'CHECK'),var(6))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(7))),
	cf(1,eq(proj(var(0),'m::'),var(8))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(5),'PRED'),semform('Tobakkshavehunden',0,[],[]))),
	cf(1,eq(attr(var(5),'CHECK'),var(9))),
	cf(1,eq(attr(var(5),'GEND'),var(10))),
	cf(1,eq(attr(var(5),'NTYPE'),var(11))),
	cf(1,eq(proj(var(5),'m::'),var(12))),
	cf(1,eq(attr(var(5),'CASE'),'nom')),
	cf(1,eq(attr(var(5),'DEF'),'+')),
	cf(1,eq(attr(var(5),'NUM'),'sg')),
	cf(1,eq(attr(var(5),'PERS'),'3')),
	cf(1,eq(attr(var(5),'REF'),'+')),
	cf(1,eq(attr(var(9),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(10),'FEM'),'-')),
	cf(1,eq(attr(var(10),'MASC'),'+')),
	cf(1,eq(attr(var(10),'NEUT'),'-')),
	cf(1,eq(attr(var(11),'NSEM'),var(13))),
	cf(1,eq(attr(var(11),'NSYN'),'proper')),
	cf(1,eq(attr(var(13),'PROPER'),var(14))),
	cf(1,eq(attr(var(14),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(12),'H-CONS'),var(15))),
	cf(1,eq(attr(var(12),'RELS'),var(16))),
	cf(1,eq(attr(var(12),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(12),'TOP'),var(17))),
	cf(1,eq(attr(var(12),'_QUANT'),var(18))),
	cf(1,in_set(var(1),var(15))),
	cf(1,in_set(var(2),var(15))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(19))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(20))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(19),'type'),'handle')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(21))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(22))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(21),'type'),'handle')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,in_set(var(3),var(16))),
	cf(1,in_set(var(4),var(16))),
	cf(1,eq(attr(var(3),'ARG0'),var(23))),
	cf(1,eq(attr(var(3),'LBL'),var(19))),
	cf(1,eq(attr(var(3),'CARG'),'Tobakkshavehunden')),
	cf(1,eq(attr(var(3),'relation'),semform('named',1,[],[]))),
	cf(1,eq(attr(var(23),'NUM'),'sg')),
	cf(1,eq(attr(var(23),'PERS'),'3')),
	cf(1,eq(attr(var(23),'type'),'ref-ind')),
	cf(1,eq(attr(var(4),'ARG0'),var(23))),
	cf(1,eq(attr(var(4),'BODY'),var(24))),
	cf(1,eq(attr(var(4),'LBL'),var(17))),
	cf(1,eq(attr(var(4),'RSTR'),var(20))),
	cf(1,eq(attr(var(4),'relation'),semform('proper_q',2,[],[]))),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(17),'type'),'handle')),
	cf(1,eq(attr(var(18),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(18),'TOP'),var(25))),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(6),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(7),'MOOD'),'indicative')),
	cf(1,eq(attr(var(7),'TENSE'),'past')),
	cf(1,eq(attr(var(8),'H-CONS'),var(15))),
	cf(1,eq(attr(var(8),'INDEX'),var(26))),
	cf(1,eq(attr(var(8),'RELS'),var(27))),
	cf(1,eq(attr(var(8),'RELS_EL'),var(28))),
	cf(1,eq(attr(var(8),'TOP'),var(22))),
	cf(1,eq(attr(var(8),'_MSG'),var(29))),
	cf(1,eq(attr(var(8),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(26),'PERF'),'-')),
	cf(1,eq(attr(var(26),'SF'),'prop')),
	cf(1,eq(attr(var(26),'TENSE'),'past')),
	cf(1,eq(attr(var(26),'type'),'event')),
	cf(1,in_set(var(28),var(27))),
	cf(1,eq(attr(var(28),'ARG0'),var(26))),
	cf(1,eq(attr(var(28),'ARG1'),var(23))),
	cf(1,eq(attr(var(28),'LBL'),var(21))),
	cf(1,eq(attr(var(28),'_CAT'),'v')),
	cf(1,eq(attr(var(28),'relation'),semform('bjeffe',5,[],[]))),
	cf(1,eq(attr(var(29),'ARG0'),var(26))),
	cf(1,eq(attr(var(29),'LBL'),var(22))),
	cf(1,eq(proj(var(30),'m::'),var(31))),
	cf(1,eq(proj(var(32),'m::'),var(31)))
	],
	% C-Structure:
	[
	cf(1,subtree(1582,'ROOT',1605,123)),
	cf(1,phi(1582,var(0))),
	cf(1,subtree(1605,'ROOT',-,1717)),
	cf(1,phi(1605,var(0))),
	cf(1,subtree(1717,'IP',1635,1696)),
	cf(1,phi(1717,var(0))),
	cf(1,subtree(1635,'IP',-,2171)),
	cf(1,phi(1635,var(0))),
	cf(1,subtree(2171,'PROPP',-,2082)),
	cf(1,phi(2171,var(5))),
	cf(1,subtree(2082,'PROP',2080,2)),
	cf(1,phi(2082,var(5))),
	cf(1,subtree(2080,'PROP',2078,4)),
	cf(1,phi(2080,var(5))),
	cf(1,subtree(2078,'PROP',2076,6)),
	cf(1,phi(2078,var(5))),
	cf(1,subtree(2076,'PROP',2073,8)),
	cf(1,phi(2076,var(5))),
	cf(1,subtree(2073,'PROP',-,10)),
	cf(1,phi(2073,var(5))),
	cf(1,subtree(10,'PROP_BASE',-,11)),
	cf(1,phi(10,var(5))),
	cf(1,terminal(11,'Tobakkshavehunden',[1])),
	cf(1,phi(11,var(33))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(5))),
	cf(1,terminal(9,'+Prop',[1])),
	cf(1,phi(9,var(5))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(5))),
	cf(1,terminal(7,'+Masc',[1])),
	cf(1,phi(7,var(5))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(5))),
	cf(1,terminal(5,'+Indef',[1])),
	cf(1,phi(5,var(5))),
	cf(1,subtree(2,'N_SUFF_BASE',-,3)),
	cf(1,phi(2,var(5))),
	cf(1,terminal(3,'+Sg',[1])),
	cf(1,phi(3,var(5))),
	cf(1,subtree(1696,'I\'',-,1038)),
	cf(1,phi(1696,var(0))),
	cf(1,subtree(1038,'Vfin',1037,79)),
	cf(1,phi(1038,var(0))),
	cf(1,cproj(1038,var(30))),
	cf(1,subtree(1037,'Vfin',1036,81)),
	cf(1,phi(1037,var(0))),
	cf(1,cproj(1037,var(30))),
	cf(1,subtree(1036,'Vfin',-,83)),
	cf(1,phi(1036,var(0))),
	cf(1,cproj(1036,var(30))),
	cf(1,subtree(83,'V_BASE',-,84)),
	cf(1,phi(83,var(0))),
	cf(1,cproj(83,var(32))),
	cf(1,terminal(84,'bjeffe',[78])),
	cf(1,phi(84,var(0))),
	cf(1,cproj(84,var(32))),
	cf(1,subtree(81,'V_SUFF_BASE',-,82)),
	cf(1,phi(81,var(0))),
	cf(1,terminal(82,'+Verb',[78])),
	cf(1,phi(82,var(0))),
	cf(1,subtree(79,'V_SUFF_BASE',-,80)),
	cf(1,phi(79,var(0))),
	cf(1,terminal(80,'+Past',[78])),
	cf(1,phi(80,var(0))),
	cf(1,subtree(123,'PERIOD',-,116)),
	cf(1,phi(123,var(0))),
	cf(1,terminal(116,'.',[116])),
	cf(1,phi(116,var(0))),
	cf(1,semform_data(0,10,1,17)),
	cf(1,semform_data(1,10,1,17)),
	cf(1,semform_data(2,10,1,17)),
	cf(1,semform_data(4,83,19,26)),
	cf(1,semform_data(5,83,19,26)),
	cf(1,fspan(var(0),1,27)),
	cf(1,fspan(var(5),1,18)),
	cf(1,surfaceform(1,'Tobakkshavehunden',1,18)),
	cf(1,surfaceform(78,'bjeffet',19,27)),
	cf(1,surfaceform(116,'.',26,27))
	]).

