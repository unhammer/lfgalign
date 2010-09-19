% -*- coding: iso-8859-1 -*-

fstructure('Tre av hundene bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.22 CPU seconds, 314 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',10,[var(8)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(8))),
	cf(1,eq(attr(var(0),'TOPIC'),var(8))),
	cf(1,eq(attr(var(0),'CHECK'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(proj(var(0),'m::'),var(11))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(8),'PRED'),semform('tre',0,[var(12)],[]))),
	cf(1,eq(attr(var(8),'OBL'),var(12))),
	cf(1,eq(attr(var(8),'NTYPE'),var(13))),
	cf(1,eq(proj(var(8),'m::'),var(14))),
	cf(1,eq(attr(var(8),'CASE'),'nom')),
	cf(1,eq(attr(var(8),'DEF'),'-')),
	cf(1,eq(attr(var(8),'DIGVALUE'),'3')),
	cf(1,eq(attr(var(8),'HEADNUM'),'pl')),
	cf(1,eq(attr(var(8),'REF'),'+')),
	cf(1,eq(attr(var(12),'PRED'),semform('part',5,[var(15)],[]))),
	cf(1,eq(attr(var(12),'OBJ'),var(15))),
	cf(1,eq(attr(var(12),'CHECK'),var(16))),
	cf(1,eq(proj(var(12),'m::'),var(17))),
	cf(1,eq(attr(var(12),'PFORM'),'av')),
	cf(1,eq(attr(var(12),'PSEM'),'part')),
	cf(1,eq(attr(var(12),'PTYPE'),'sem')),
	cf(1,eq(attr(var(15),'PRED'),semform('hund',7,[],[]))),
	cf(1,eq(attr(var(15),'CHECK'),var(18))),
	cf(1,eq(attr(var(15),'GEND'),var(19))),
	cf(1,eq(attr(var(15),'NTYPE'),var(13))),
	cf(1,eq(proj(var(15),'m::'),var(20))),
	cf(1,eq(attr(var(15),'CASE'),'obl')),
	cf(1,eq(attr(var(15),'DEF'),'+')),
	cf(1,eq(attr(var(15),'NUM'),'pl')),
	cf(1,eq(attr(var(15),'PERS'),'3')),
	cf(1,eq(attr(var(18),'_SEL'),var(21))),
	cf(1,eq(attr(var(18),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(18),'_NOUN'),'+')),
	cf(1,eq(attr(var(21),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(21),'_ANIM'),'+')),
	cf(1,eq(attr(var(21),'_HUMAN'),'-')),
	cf(1,eq(attr(var(19),'FEM'),'-')),
	cf(1,eq(attr(var(19),'MASC'),'+')),
	cf(1,eq(attr(var(19),'NEUT'),'-')),
	cf(1,eq(attr(var(13),'NSEM'),var(22))),
	cf(1,eq(attr(var(13),'NSYN'),'common')),
	cf(1,eq(attr(var(22),'COMMON'),'count')),
	cf(1,eq(attr(var(20),'H-CONS'),var(23))),
	cf(1,eq(attr(var(20),'RELS'),var(24))),
	cf(1,eq(attr(var(20),'RELS_EL'),var(2))),
	cf(1,eq(attr(var(20),'TOP'),var(25))),
	cf(1,eq(attr(var(20),'_ANTECEDVAR'),var(26))),
	cf(1,eq(attr(var(20),'_QUANT'),var(27))),
	cf(1,in_set(var(28),var(23))),
	cf(1,eq(attr(var(28),'OUTSCPD'),var(29))),
	cf(1,eq(attr(var(28),'SC_ARG'),var(30))),
	cf(1,eq(attr(var(28),'relation'),'qeq')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,in_set(var(1),var(24))),
	cf(1,in_set(var(2),var(24))),
	cf(1,eq(attr(var(1),'ARG0'),var(31))),
	cf(1,eq(attr(var(1),'BODY'),var(32))),
	cf(1,eq(attr(var(1),'LBL'),var(25))),
	cf(1,eq(attr(var(1),'RSTR'),var(30))),
	cf(1,eq(attr(var(1),'relation'),semform('def_q',9,[],[]))),
	cf(1,eq(attr(var(31),'DIV'),'+')),
	cf(1,eq(attr(var(31),'GRIND'),'-')),
	cf(1,eq(attr(var(31),'NATGEND'),'gender')),
	cf(1,eq(attr(var(31),'NUM'),'pl')),
	cf(1,eq(attr(var(31),'PERS'),'3')),
	cf(1,eq(attr(var(31),'type'),'ref-ind')),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(2),'ARG0'),var(31))),
	cf(1,eq(attr(var(2),'LBL'),var(29))),
	cf(1,eq(attr(var(2),'_CAT'),'n')),
	cf(1,eq(attr(var(2),'relation'),semform('hund',8,[],[]))),
	cf(1,eq(attr(var(27),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(27),'TOP'),var(33))),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(16),'_ANTECED'),var(34))),
	cf(1,eq(attr(var(16),'_PARTITIVE'),'+')),
	cf(1,eq(proj(var(34),'m::'),var(35))),
	cf(1,eq(attr(var(35),'ARG0'),var(26))),
	cf(1,eq(attr(var(17),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(17),'_ANTECEDVAR'),var(26))),
	cf(1,eq(attr(var(5),'ARG0'),var(36))),
	cf(1,eq(attr(var(5),'ARG1'),var(31))),
	cf(1,eq(attr(var(5),'LBL'),var(37))),
	cf(1,eq(attr(var(5),'relation'),semform('part_of',3,[],[]))),
	cf(1,eq(attr(var(36),'type'),'ref-ind')),
	cf(1,eq(attr(var(37),'type'),'handle')),
	cf(1,eq(attr(var(14),'H-CONS'),var(38))),
	cf(1,eq(attr(var(14),'RELS'),var(39))),
	cf(1,eq(attr(var(14),'RELS_EL'),var(40))),
	cf(1,eq(attr(var(14),'TOP'),var(41))),
	cf(1,eq(attr(var(14),'_ANTECEDVAR'),var(26))),
	cf(1,eq(attr(var(14),'_CARD'),var(7))),
	cf(1,eq(attr(var(14),'_QUANT'),var(42))),
	cf(1,eq(attr(var(14),'_TOPHNDL'),var(37))),
	cf(1,in_set(var(3),var(38))),
	cf(1,in_set(var(4),var(38))),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(37))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(43))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(43),'type'),'handle')),
	cf(1,eq(attr(var(4),'OUTSCPD'),var(44))),
	cf(1,eq(attr(var(4),'SC_ARG'),var(45))),
	cf(1,eq(attr(var(4),'relation'),'qeq')),
	cf(1,eq(attr(var(44),'type'),'handle')),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,in_set(var(5),var(39))),
	cf(1,in_set(var(6),var(39))),
	cf(1,in_set(var(7),var(39))),
	cf(1,eq(attr(var(6),'ARG0'),var(36))),
	cf(1,eq(attr(var(6),'BODY'),var(46))),
	cf(1,eq(attr(var(6),'LBL'),var(41))),
	cf(1,eq(attr(var(6),'RSTR'),var(43))),
	cf(1,eq(attr(var(6),'relation'),semform('bare_div_q',2,[],[]))),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,eq(attr(var(41),'type'),'handle')),
	cf(1,eq(attr(var(7),'ARG0'),var(47))),
	cf(1,eq(attr(var(7),'ARG1'),var(36))),
	cf(1,eq(attr(var(7),'LBL'),var(37))),
	cf(1,eq(attr(var(7),'CARG'),'3')),
	cf(1,eq(attr(var(7),'relation'),semform('card',1,[],[]))),
	cf(1,eq(attr(var(47),'type'),'event')),
	cf(1,eq(attr(var(40),'ARG0'),var(36))),
	cf(1,eq(attr(var(40),'LBL'),var(37))),
	cf(1,eq(attr(var(42),'RELS_EL'),var(6))),
	cf(1,eq(attr(var(42),'TOP'),var(48))),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(9),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'past')),
	cf(1,eq(attr(var(11),'H-CONS'),var(38))),
	cf(1,eq(attr(var(11),'INDEX'),var(49))),
	cf(1,eq(attr(var(11),'RELS'),var(50))),
	cf(1,eq(attr(var(11),'RELS_EL'),var(51))),
	cf(1,eq(attr(var(11),'TOP'),var(45))),
	cf(1,eq(attr(var(11),'_MSG'),var(52))),
	cf(1,eq(attr(var(11),'_MSGQEQ'),var(4))),
	cf(1,eq(attr(var(49),'PERF'),'-')),
	cf(1,eq(attr(var(49),'SF'),'prop')),
	cf(1,eq(attr(var(49),'TENSE'),'past')),
	cf(1,eq(attr(var(49),'type'),'event')),
	cf(1,in_set(var(51),var(50))),
	cf(1,eq(attr(var(51),'ARG0'),var(49))),
	cf(1,eq(attr(var(51),'ARG1'),var(36))),
	cf(1,eq(attr(var(51),'LBL'),var(44))),
	cf(1,eq(attr(var(51),'_CAT'),'v')),
	cf(1,eq(attr(var(51),'relation'),semform('bjeffe',11,[],[]))),
	cf(1,eq(attr(var(52),'ARG0'),var(49))),
	cf(1,eq(attr(var(52),'LBL'),var(45))),
	cf(1,eq(proj(var(53),'o::'),var(54))),
	cf(1,in_set('Mark1',var(54))),
	cf(1,eq(proj(var(55),'m::'),var(56))),
	cf(1,eq(proj(var(57),'m::'),var(56))),
	cf(1,eq(proj(var(58),'m::'),var(59))),
	cf(1,eq(proj(var(60),'m::'),var(59)))
	],
	% C-Structure:
	[
	cf(1,subtree(377,'ROOT',374,122)),
	cf(1,phi(377,var(0))),
	cf(1,subtree(374,'ROOT',-,1211)),
	cf(1,phi(374,var(0))),
	cf(1,subtree(1211,'IP',1126,1190)),
	cf(1,phi(1211,var(0))),
	cf(1,subtree(1126,'IP',-,1789)),
	cf(1,phi(1126,var(0))),
	cf(1,subtree(1789,'QuantP',1759,1853)),
	cf(1,phi(1789,var(8))),
	cf(1,subtree(1759,'QuantP',-,298)),
	cf(1,phi(1759,var(8))),
	cf(1,subtree(298,'NUMP',-,296)),
	cf(1,phi(298,var(8))),
	cf(1,subtree(296,'NUM1P',-,37)),
	cf(1,phi(296,var(8))),
	cf(1,subtree(37,'NUM1',-,3)),
	cf(1,phi(37,var(8))),
	cf(1,terminal(3,'tre',[3])),
	cf(1,phi(3,var(8))),
	cf(1,subtree(1853,'PPpart',1849,1289)),
	cf(1,phi(1853,var(12))),
	cf(1,cproj(1853,var(53))),
	cf(1,subtree(1849,'PPpart',-,56)),
	cf(1,phi(1849,var(12))),
	cf(1,subtree(56,'Ppart',-,38)),
	cf(1,phi(56,var(12))),
	cf(1,terminal(38,'av',[38])),
	cf(1,phi(38,var(12))),
	cf(1,subtree(1289,'NP',-,58)),
	cf(1,phi(1289,var(15))),
	cf(1,subtree(58,'N',754,59)),
	cf(1,phi(58,var(15))),
	cf(1,cproj(58,var(55))),
	cf(1,subtree(754,'N',753,61)),
	cf(1,phi(754,var(15))),
	cf(1,cproj(754,var(55))),
	cf(1,subtree(753,'N',752,63)),
	cf(1,phi(753,var(15))),
	cf(1,cproj(753,var(55))),
	cf(1,subtree(752,'N',724,65)),
	cf(1,phi(752,var(15))),
	cf(1,cproj(752,var(55))),
	cf(1,subtree(724,'N',-,67)),
	cf(1,phi(724,var(15))),
	cf(1,cproj(724,var(55))),
	cf(1,subtree(67,'N_BASE',-,68)),
	cf(1,phi(67,var(15))),
	cf(1,cproj(67,var(57))),
	cf(1,terminal(68,'hund',[57])),
	cf(1,phi(68,var(15))),
	cf(1,cproj(68,var(57))),
	cf(1,subtree(65,'N_SUFF_BASE',-,66)),
	cf(1,phi(65,var(15))),
	cf(1,terminal(66,'+Noun',[57])),
	cf(1,phi(66,var(15))),
	cf(1,subtree(63,'N_SUFF_BASE',-,64)),
	cf(1,phi(63,var(15))),
	cf(1,terminal(64,'+Masc',[57])),
	cf(1,phi(64,var(15))),
	cf(1,subtree(61,'N_SUFF_BASE',-,62)),
	cf(1,phi(61,var(15))),
	cf(1,terminal(62,'+Def',[57])),
	cf(1,phi(62,var(61))),
	cf(1,subtree(59,'N_SUFF_BASE',-,60)),
	cf(1,phi(59,var(15))),
	cf(1,terminal(60,'+Pl',[57])),
	cf(1,phi(60,var(15))),
	cf(1,subtree(1190,'I\'',-,835)),
	cf(1,phi(1190,var(0))),
	cf(1,subtree(835,'Vfin',834,78)),
	cf(1,phi(835,var(0))),
	cf(1,cproj(835,var(58))),
	cf(1,subtree(834,'Vfin',833,80)),
	cf(1,phi(834,var(0))),
	cf(1,cproj(834,var(58))),
	cf(1,subtree(833,'Vfin',-,82)),
	cf(1,phi(833,var(0))),
	cf(1,cproj(833,var(58))),
	cf(1,subtree(82,'V_BASE',-,83)),
	cf(1,phi(82,var(0))),
	cf(1,cproj(82,var(60))),
	cf(1,terminal(83,'bjeffe',[77])),
	cf(1,phi(83,var(0))),
	cf(1,cproj(83,var(60))),
	cf(1,subtree(80,'V_SUFF_BASE',-,81)),
	cf(1,phi(80,var(0))),
	cf(1,terminal(81,'+Verb',[77])),
	cf(1,phi(81,var(0))),
	cf(1,subtree(78,'V_SUFF_BASE',-,79)),
	cf(1,phi(78,var(0))),
	cf(1,terminal(79,'+Past',[77])),
	cf(1,phi(79,var(0))),
	cf(1,subtree(122,'PERIOD',-,115)),
	cf(1,phi(122,var(0))),
	cf(1,terminal(115,'.',[115])),
	cf(1,phi(115,var(0))),
	cf(1,semform_data(0,37,1,4)),
	cf(1,semform_data(1,37,1,4)),
	cf(1,semform_data(2,1759,1,4)),
	cf(1,semform_data(3,1789,1,15)),
	cf(1,semform_data(5,56,5,7)),
	cf(1,semform_data(7,67,8,11)),
	cf(1,semform_data(8,67,8,11)),
	cf(1,semform_data(9,61,14,14)),
	cf(1,semform_data(10,82,16,23)),
	cf(1,semform_data(11,82,16,23)),
	cf(1,fspan(var(0),1,24)),
	cf(1,fspan(var(8),1,15)),
	cf(1,fspan(var(12),5,15)),
	cf(1,fspan(var(15),8,15)),
	cf(1,surfaceform(3,'tre',1,4)),
	cf(1,surfaceform(38,'av',5,7)),
	cf(1,surfaceform(57,'hundene',8,15)),
	cf(1,surfaceform(77,'bjeffet',16,24)),
	cf(1,surfaceform(115,'.',23,24))
	]).

