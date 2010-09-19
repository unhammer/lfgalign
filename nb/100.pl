% -*- coding: iso-8859-1 -*-

fstructure('Noen jaget Abrams.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.07 CPU seconds, 98 subtrees unified'),
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
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('jage',7,[var(7),var(8)],['NULL']))),
	cf(1,eq(attr(var(0),'SUBJ'),var(7))),
	cf(1,eq(attr(var(0),'OBJ'),var(8))),
	cf(1,eq(attr(var(0),'TOPIC'),var(7))),
	cf(1,eq(attr(var(0),'CHECK'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(proj(var(0),'m::'),var(11))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(7),'PRED'),semform('pro',2,[],[]))),
	cf(A2,eq(attr(var(7),'GEND'),var(12))),
	cf(1,eq(attr(var(7),'NUM'),var(13))),
	cf(1,eq(attr(var(7),'SPEC'),var(14))),
	cf(1,eq(proj(var(7),'m::'),var(15))),
	cf(1,eq(attr(var(7),'CASE'),'nom')),
	cf(1,eq(attr(var(7),'DEF'),'-')),
	cf(1,eq(attr(var(7),'PERS'),'3')),
	cf(1,eq(attr(var(7),'REF'),'+')),
	cf(A2,eq(attr(var(12),'NEUT'),'-')),
	cf(A1,eq(var(13),'pl')),
	cf(A2,eq(var(13),'sg')),
	cf(1,eq(attr(var(14),'QUANT'),var(16))),
	cf(1,eq(attr(var(16),'PRED'),semform('noen',0,[],[]))),
	cf(1,eq(attr(var(16),'QUANT-TYPE'),'existential')),
	cf(1,eq(attr(var(15),'H-CONS'),var(17))),
	cf(1,eq(attr(var(15),'RELS'),var(18))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(15),'TOP'),var(19))),
	cf(1,eq(attr(var(15),'_QUANT'),var(20))),
	cf(1,in_set(var(1),var(17))),
	cf(1,in_set(var(2),var(17))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(21))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(22))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(21),'type'),'handle')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(23))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(24))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(23),'type'),'handle')),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,in_set(var(3),var(18))),
	cf(1,in_set(var(4),var(18))),
	cf(1,eq(attr(var(3),'ARG0'),var(25))),
	cf(1,eq(attr(var(3),'BODY'),var(26))),
	cf(1,eq(attr(var(3),'LBL'),var(19))),
	cf(1,eq(attr(var(3),'RSTR'),var(22))),
	cf(1,eq(attr(var(3),'relation'),semform('noen_q',1,[],[]))),
	cf(1,eq(attr(var(25),'NUM'),var(13))),
	cf(1,eq(attr(var(25),'PERS'),'3')),
	cf(1,eq(attr(var(25),'type'),'ref-ind')),
	cf(1,eq(attr(var(26),'type'),'handle')),
	cf(1,eq(attr(var(19),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(25))),
	cf(1,eq(attr(var(4),'LBL'),var(21))),
	cf(1,eq(attr(var(4),'relation'),var(27))),
	cf(A2,eq(var(27),semform('person',3,[],[]))),
	cf(A1,eq(var(27),semform('generic-nom',5,[],[]))),
	cf(1,eq(attr(var(20),'RELS_EL'),var(3))),
	cf(1,eq(attr(var(20),'TOP'),var(28))),
	cf(1,eq(attr(var(28),'type'),'handle')),
	cf(1,eq(attr(var(8),'PRED'),semform('Abrams',9,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(29))),
	cf(1,eq(attr(var(8),'NTYPE'),var(30))),
	cf(1,eq(proj(var(8),'m::'),var(31))),
	cf(1,eq(attr(var(8),'CASE'),'obl')),
	cf(1,eq(attr(var(8),'DEF'),'+')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'PERS'),'3')),
	cf(1,eq(attr(var(8),'REF'),'+')),
	cf(1,eq(attr(var(29),'_SEL'),var(32))),
	cf(1,eq(attr(var(29),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(32),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(32),'_ANIM'),'+')),
	cf(1,eq(attr(var(32),'_HUMAN'),'+')),
	cf(1,eq(attr(var(30),'NSEM'),var(33))),
	cf(1,eq(attr(var(30),'NSYN'),'proper')),
	cf(1,eq(attr(var(33),'PROPER'),var(34))),
	cf(1,eq(attr(var(34),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(31),'H-CONS'),var(35))),
	cf(1,eq(attr(var(31),'RELS'),var(36))),
	cf(1,eq(attr(var(31),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(31),'TOP'),var(37))),
	cf(1,eq(attr(var(31),'_ANTECEDVAR'),var(25))),
	cf(1,eq(attr(var(31),'_QUANT'),var(38))),
	cf(1,in_set(var(39),var(35))),
	cf(1,eq(attr(var(39),'OUTSCPD'),var(40))),
	cf(1,eq(attr(var(39),'SC_ARG'),var(41))),
	cf(1,eq(attr(var(39),'relation'),'qeq')),
	cf(1,eq(attr(var(40),'type'),'handle')),
	cf(1,eq(attr(var(41),'type'),'handle')),
	cf(1,in_set(var(5),var(36))),
	cf(1,in_set(var(6),var(36))),
	cf(1,eq(attr(var(5),'ARG0'),var(42))),
	cf(1,eq(attr(var(5),'LBL'),var(40))),
	cf(1,eq(attr(var(5),'CARG'),'Abrams')),
	cf(1,eq(attr(var(5),'relation'),semform('named',10,[],[]))),
	cf(1,eq(attr(var(42),'NATGEND'),'animate')),
	cf(1,eq(attr(var(42),'NUM'),'sg')),
	cf(1,eq(attr(var(42),'PERS'),'3')),
	cf(1,eq(attr(var(42),'type'),'ref-ind')),
	cf(1,eq(attr(var(6),'ARG0'),var(42))),
	cf(1,eq(attr(var(6),'BODY'),var(43))),
	cf(1,eq(attr(var(6),'LBL'),var(37))),
	cf(1,eq(attr(var(6),'RSTR'),var(41))),
	cf(1,eq(attr(var(6),'relation'),semform('proper_q',11,[],[]))),
	cf(1,eq(attr(var(43),'type'),'handle')),
	cf(1,eq(attr(var(37),'type'),'handle')),
	cf(1,eq(attr(var(38),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(38),'TOP'),var(44))),
	cf(1,eq(attr(var(44),'type'),'handle')),
	cf(1,eq(attr(var(9),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'past')),
	cf(1,eq(attr(var(11),'H-CONS'),var(17))),
	cf(1,eq(attr(var(11),'INDEX'),var(45))),
	cf(1,eq(attr(var(11),'RELS'),var(46))),
	cf(1,eq(attr(var(11),'RELS_EL'),var(47))),
	cf(1,eq(attr(var(11),'TOP'),var(24))),
	cf(1,eq(attr(var(11),'_MSG'),var(48))),
	cf(1,eq(attr(var(11),'_MSGQEQ'),var(2))),
	cf(1,eq(attr(var(45),'PERF'),'-')),
	cf(1,eq(attr(var(45),'SF'),'prop')),
	cf(1,eq(attr(var(45),'TENSE'),'past')),
	cf(1,eq(attr(var(45),'type'),'event')),
	cf(1,in_set(var(47),var(46))),
	cf(1,eq(attr(var(47),'ARG0'),var(45))),
	cf(1,eq(attr(var(47),'ARG1'),var(25))),
	cf(1,eq(attr(var(47),'ARG2'),var(42))),
	cf(1,eq(attr(var(47),'LBL'),var(23))),
	cf(1,eq(attr(var(47),'_CAT'),'v')),
	cf(1,eq(attr(var(47),'relation'),semform('jage',8,[],[]))),
	cf(1,eq(attr(var(48),'ARG0'),var(45))),
	cf(1,eq(attr(var(48),'LBL'),var(24))),
	cf(1,eq(proj(var(49),'m::'),var(50))),
	cf(1,eq(proj(var(51),'m::'),var(50)))
	],
	% C-Structure:
	[
	cf(1,subtree(650,'ROOT',804,92)),
	cf(1,phi(650,var(0))),
	cf(1,subtree(804,'ROOT',-,1458)),
	cf(1,phi(804,var(0))),
	cf(1,subtree(1458,'IP',1329,1456)),
	cf(1,phi(1458,var(0))),
	cf(1,subtree(1329,'IP',-,269)),
	cf(1,phi(1329,var(0))),
	cf(1,subtree(269,'QuantP',-,267)),
	cf(1,phi(269,var(7))),
	cf(1,subtree(267,'QP',-,36)),
	cf(1,phi(267,var(7))),
	cf(1,subtree(36,'Q',-,3)),
	cf(1,phi(36,var(7))),
	cf(1,terminal(3,'noen',[3])),
	cf(1,phi(3,var(52))),
	cf(1,subtree(1456,'I\'',1423,1426)),
	cf(1,phi(1456,var(0))),
	cf(1,subtree(1423,'I\'',-,1049)),
	cf(1,phi(1423,var(0))),
	cf(1,subtree(1049,'Vfin',1048,39)),
	cf(1,phi(1049,var(0))),
	cf(1,cproj(1049,var(49))),
	cf(1,subtree(1048,'Vfin',1047,41)),
	cf(1,phi(1048,var(0))),
	cf(1,cproj(1048,var(49))),
	cf(1,subtree(1047,'Vfin',-,43)),
	cf(1,phi(1047,var(0))),
	cf(1,cproj(1047,var(49))),
	cf(1,subtree(43,'V_BASE',-,44)),
	cf(1,phi(43,var(0))),
	cf(1,cproj(43,var(51))),
	cf(1,terminal(44,'jage',[37])),
	cf(1,phi(44,var(0))),
	cf(1,cproj(44,var(51))),
	cf(1,subtree(41,'V_SUFF_BASE',-,42)),
	cf(1,phi(41,var(0))),
	cf(1,terminal(42,'+Verb',[37])),
	cf(1,phi(42,var(0))),
	cf(1,subtree(39,'V_SUFF_BASE',-,40)),
	cf(1,phi(39,var(0))),
	cf(1,terminal(40,'+Past',[37])),
	cf(1,phi(40,var(0))),
	cf(1,subtree(1426,'S',-,1433)),
	cf(1,phi(1426,var(0))),
	cf(1,subtree(1433,'VPmain',-,563)),
	cf(1,phi(1433,var(0))),
	cf(1,subtree(563,'PROPP',-,84)),
	cf(1,phi(563,var(8))),
	cf(1,subtree(84,'PROP',-,76)),
	cf(1,phi(84,var(8))),
	cf(1,terminal(76,'Abrams',[76])),
	cf(1,phi(76,var(8))),
	cf(1,subtree(92,'PERIOD',-,85)),
	cf(1,phi(92,var(0))),
	cf(1,terminal(85,'.',[85])),
	cf(1,phi(85,var(0))),
	cf(1,semform_data(0,36,1,5)),
	cf(1,semform_data(1,36,1,5)),
	cf(1,semform_data(2,269,1,5)),
	cf(A2,semform_data(3,269,1,5)),
	cf(A1,semform_data(5,269,1,5)),
	cf(1,semform_data(7,43,6,11)),
	cf(1,semform_data(8,43,6,11)),
	cf(1,semform_data(9,84,12,18)),
	cf(1,semform_data(10,84,12,18)),
	cf(1,semform_data(11,84,12,18)),
	cf(1,fspan(var(0),1,13)),
	cf(1,fspan(var(7),1,5)),
	cf(1,fspan(var(8),12,18)),
	cf(1,surfaceform(3,'noen',1,5)),
	cf(1,surfaceform(37,'jaget',6,11)),
	cf(1,surfaceform(76,'Abrams',12,18)),
	cf(1,surfaceform(85,'.',12,13))
	]).

