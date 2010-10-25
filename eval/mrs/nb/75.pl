% -*- coding: iso-8859-1 -*-

fstructure('Hunden ankom og Browne bjeffet.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.11 CPU seconds, 188 subtrees unified'),
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
	cf(1,in_set(var(1),var(0))),
	cf(1,in_set(var(2),var(0))),
	cf(1,eq(attr(var(0),'CHECK'),var(11))),
	cf(1,eq(proj(var(0),'m::'),var(12))),
	cf(1,eq(attr(var(0),'COORD-FORM'),'og')),
	cf(1,eq(attr(var(11),'_AUX-SELECT'),'be')),
	cf(1,eq(attr(var(11),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(12),'H-CONS'),var(13))),
	cf(1,eq(attr(var(12),'INDEX'),var(14))),
	cf(1,eq(attr(var(12),'RELS'),var(15))),
	cf(1,eq(attr(var(12),'RELS_EL'),var(16))),
	cf(1,eq(attr(var(12),'TOP'),var(17))),
	cf(1,eq(attr(var(12),'_CONJREL'),var(18))),
	cf(1,eq(attr(var(12),'_MSG'),var(19))),
	cf(1,in_set(var(3),var(13))),
	cf(1,in_set(var(4),var(13))),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(20))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(21))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,eq(attr(var(21),'type'),'handle')),
	cf(1,eq(attr(var(4),'OUTSCPD'),var(22))),
	cf(1,eq(attr(var(4),'SC_ARG'),var(23))),
	cf(1,eq(attr(var(4),'relation'),'qeq')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(23),'type'),'handle')),
	cf(1,eq(attr(var(14),'PERF'),'-')),
	cf(1,eq(attr(var(14),'SF'),'prop')),
	cf(1,eq(attr(var(14),'TENSE'),'past')),
	cf(1,eq(attr(var(14),'type'),'event')),
	cf(1,in_set(var(16),var(15))),
	cf(1,eq(attr(var(16),'ARG0'),var(14))),
	cf(1,eq(attr(var(16),'L-HNDL'),var(23))),
	cf(1,eq(attr(var(16),'L-INDEX'),var(24))),
	cf(1,eq(attr(var(16),'LBL'),var(17))),
	cf(1,eq(attr(var(16),'R-HNDL'),var(25))),
	cf(1,eq(attr(var(16),'R-INDEX'),var(26))),
	cf(1,eq(attr(var(16),'relation'),semform('and',6,[],[]))),
	cf(1,eq(attr(var(24),'PERF'),'-')),
	cf(1,eq(attr(var(24),'SF'),'prop')),
	cf(1,eq(attr(var(24),'TENSE'),'past')),
	cf(1,eq(attr(var(24),'type'),'event')),
	cf(1,eq(attr(var(17),'type'),'handle')),
	cf(1,eq(attr(var(25),'type'),'handle')),
	cf(1,eq(attr(var(26),'PERF'),'-')),
	cf(1,eq(attr(var(26),'SF'),'prop')),
	cf(1,eq(attr(var(26),'TENSE'),'past')),
	cf(1,eq(attr(var(26),'type'),'event')),
	cf(1,eq(attr(var(19),'ARG0'),var(14))),
	cf(1,eq(attr(var(19),'LBL'),var(17))),
	cf(1,eq(attr(var(1),'PRED'),semform('bjeffe',11,[var(27)],[]))),
	cf(1,eq(attr(var(1),'SUBJ'),var(27))),
	cf(1,eq(attr(var(1),'TOPIC'),var(27))),
	cf(1,eq(attr(var(1),'CHECK'),var(28))),
	cf(1,eq(attr(var(1),'TNS-ASP'),var(29))),
	cf(1,eq(proj(var(1),'m::'),var(30))),
	cf(1,eq(attr(var(1),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(1),'VFORM'),'fin')),
	cf(1,eq(attr(var(1),'VTYPE'),'main')),
	cf(1,eq(attr(var(27),'PRED'),semform('Browne',7,[],[]))),
	cf(1,eq(attr(var(27),'CHECK'),var(31))),
	cf(1,eq(attr(var(27),'NTYPE'),var(32))),
	cf(1,eq(proj(var(27),'m::'),var(33))),
	cf(1,eq(attr(var(27),'CASE'),'nom')),
	cf(1,eq(attr(var(27),'DEF'),'+')),
	cf(1,eq(attr(var(27),'NUM'),'sg')),
	cf(1,eq(attr(var(27),'PERS'),'3')),
	cf(1,eq(attr(var(27),'REF'),'+')),
	cf(1,eq(attr(var(31),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(32),'NSEM'),var(34))),
	cf(1,eq(attr(var(32),'NSYN'),'proper')),
	cf(1,eq(attr(var(34),'PROPER'),var(35))),
	cf(1,eq(attr(var(35),'PROPER-TYPE'),'name')),
	cf(1,eq(attr(var(33),'H-CONS'),var(36))),
	cf(1,eq(attr(var(33),'RELS'),var(37))),
	cf(1,eq(attr(var(33),'RELS_EL'),var(7))),
	cf(1,eq(attr(var(33),'TOP'),var(38))),
	cf(1,eq(attr(var(33),'_QUANT'),var(39))),
	cf(1,in_set(var(5),var(36))),
	cf(1,in_set(var(6),var(36))),
	cf(1,eq(attr(var(5),'OUTSCPD'),var(40))),
	cf(1,eq(attr(var(5),'SC_ARG'),var(41))),
	cf(1,eq(attr(var(5),'relation'),'qeq')),
	cf(1,eq(attr(var(40),'type'),'handle')),
	cf(1,eq(attr(var(41),'type'),'handle')),
	cf(1,eq(attr(var(6),'OUTSCPD'),var(42))),
	cf(1,eq(attr(var(6),'SC_ARG'),var(25))),
	cf(1,eq(attr(var(6),'relation'),'qeq')),
	cf(1,eq(attr(var(42),'type'),'handle')),
	cf(1,in_set(var(7),var(37))),
	cf(1,in_set(var(8),var(37))),
	cf(1,eq(attr(var(7),'ARG0'),var(43))),
	cf(1,eq(attr(var(7),'LBL'),var(40))),
	cf(1,eq(attr(var(7),'CARG'),'Browne')),
	cf(1,eq(attr(var(7),'relation'),semform('named',8,[],[]))),
	cf(1,eq(attr(var(43),'NUM'),'sg')),
	cf(1,eq(attr(var(43),'PERS'),'3')),
	cf(1,eq(attr(var(43),'type'),'ref-ind')),
	cf(1,eq(attr(var(8),'ARG0'),var(43))),
	cf(1,eq(attr(var(8),'BODY'),var(44))),
	cf(1,eq(attr(var(8),'LBL'),var(38))),
	cf(1,eq(attr(var(8),'RSTR'),var(41))),
	cf(1,eq(attr(var(8),'relation'),semform('proper_q',9,[],[]))),
	cf(1,eq(attr(var(44),'type'),'handle')),
	cf(1,eq(attr(var(38),'type'),'handle')),
	cf(1,eq(attr(var(39),'RELS_EL'),var(8))),
	cf(1,eq(attr(var(39),'TOP'),var(45))),
	cf(1,eq(attr(var(45),'type'),'handle')),
	cf(1,eq(attr(var(28),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(29),'MOOD'),'indicative')),
	cf(1,eq(attr(var(29),'TENSE'),'past')),
	cf(1,eq(attr(var(30),'H-CONS'),var(36))),
	cf(1,eq(attr(var(30),'INDEX'),var(26))),
	cf(1,eq(attr(var(30),'RELS'),var(46))),
	cf(1,eq(attr(var(30),'RELS_EL'),var(47))),
	cf(1,eq(attr(var(30),'TOP'),var(25))),
	cf(1,eq(attr(var(30),'_MSG'),var(48))),
	cf(1,eq(attr(var(30),'_MSGQEQ'),var(6))),
	cf(1,in_set(var(47),var(46))),
	cf(1,eq(attr(var(47),'ARG0'),var(26))),
	cf(1,eq(attr(var(47),'ARG1'),var(43))),
	cf(1,eq(attr(var(47),'LBL'),var(42))),
	cf(1,eq(attr(var(47),'_CAT'),'v')),
	cf(1,eq(attr(var(47),'relation'),semform('bjeffe',12,[],[]))),
	cf(1,eq(attr(var(48),'ARG0'),var(26))),
	cf(1,eq(attr(var(48),'LBL'),var(25))),
	cf(1,eq(attr(var(2),'PRED'),semform('ankomme',4,[var(49)],[]))),
	cf(1,eq(attr(var(2),'SUBJ'),var(49))),
	cf(1,eq(attr(var(2),'TOPIC'),var(49))),
	cf(1,eq(attr(var(2),'CHECK'),var(11))),
	cf(1,eq(attr(var(2),'TNS-ASP'),var(50))),
	cf(1,eq(proj(var(2),'m::'),var(51))),
	cf(1,eq(attr(var(2),'COORD-FORM'),'og')),
	cf(1,eq(attr(var(2),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(2),'VFORM'),'fin')),
	cf(1,eq(attr(var(2),'VTYPE'),'main')),
	cf(1,eq(attr(var(49),'PRED'),semform('hund',0,[],[]))),
	cf(1,eq(attr(var(49),'CHECK'),var(52))),
	cf(1,eq(attr(var(49),'GEND'),var(53))),
	cf(1,eq(attr(var(49),'NTYPE'),var(54))),
	cf(1,eq(proj(var(49),'m::'),var(55))),
	cf(1,eq(attr(var(49),'CASE'),'nom')),
	cf(1,eq(attr(var(49),'DEF'),'+')),
	cf(1,eq(attr(var(49),'NUM'),'sg')),
	cf(1,eq(attr(var(49),'PERS'),'3')),
	cf(1,eq(attr(var(52),'_SEL'),var(56))),
	cf(1,eq(attr(var(52),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(52),'_NOUN'),'+')),
	cf(1,eq(attr(var(56),'_ABSTRACT'),'-')),
	cf(1,eq(attr(var(56),'_ANIM'),'+')),
	cf(1,eq(attr(var(56),'_HUMAN'),'-')),
	cf(1,eq(attr(var(53),'FEM'),'-')),
	cf(1,eq(attr(var(53),'MASC'),'+')),
	cf(1,eq(attr(var(53),'NEUT'),'-')),
	cf(1,eq(attr(var(54),'NSEM'),var(57))),
	cf(1,eq(attr(var(54),'NSYN'),'common')),
	cf(1,eq(attr(var(57),'COMMON'),'count')),
	cf(1,eq(attr(var(55),'H-CONS'),var(13))),
	cf(1,eq(attr(var(55),'RELS'),var(58))),
	cf(1,eq(attr(var(55),'RELS_EL'),var(10))),
	cf(1,eq(attr(var(55),'TOP'),var(59))),
	cf(1,eq(attr(var(55),'_QUANT'),var(60))),
	cf(1,in_set(var(9),var(58))),
	cf(1,in_set(var(10),var(58))),
	cf(1,eq(attr(var(9),'ARG0'),var(61))),
	cf(1,eq(attr(var(9),'BODY'),var(62))),
	cf(1,eq(attr(var(9),'LBL'),var(59))),
	cf(1,eq(attr(var(9),'RSTR'),var(21))),
	cf(1,eq(attr(var(9),'relation'),semform('def_q',3,[],[]))),
	cf(1,eq(attr(var(61),'DIV'),'-')),
	cf(1,eq(attr(var(61),'GRIND'),'-')),
	cf(1,eq(attr(var(61),'NATGEND'),'gender')),
	cf(1,eq(attr(var(61),'NUM'),'sg')),
	cf(1,eq(attr(var(61),'PERS'),'3')),
	cf(1,eq(attr(var(61),'type'),'ref-ind')),
	cf(1,eq(attr(var(62),'type'),'handle')),
	cf(1,eq(attr(var(59),'type'),'handle')),
	cf(1,eq(attr(var(10),'ARG0'),var(61))),
	cf(1,eq(attr(var(10),'LBL'),var(20))),
	cf(1,eq(attr(var(10),'_CAT'),'n')),
	cf(1,eq(attr(var(10),'relation'),semform('hund',1,[],[]))),
	cf(1,eq(attr(var(60),'RELS_EL'),var(9))),
	cf(1,eq(attr(var(60),'TOP'),var(63))),
	cf(1,eq(attr(var(63),'type'),'handle')),
	cf(1,eq(attr(var(50),'MOOD'),'indicative')),
	cf(1,eq(attr(var(50),'TENSE'),'past')),
	cf(1,eq(attr(var(51),'H-CONS'),var(13))),
	cf(1,eq(attr(var(51),'INDEX'),var(24))),
	cf(1,eq(attr(var(51),'RELS'),var(64))),
	cf(1,eq(attr(var(51),'RELS_EL'),var(65))),
	cf(1,eq(attr(var(51),'TOP'),var(23))),
	cf(1,eq(attr(var(51),'_CONJREL'),var(18))),
	cf(1,eq(attr(var(51),'_MSG'),var(66))),
	cf(1,eq(attr(var(51),'_MSGQEQ'),var(4))),
	cf(1,in_set(var(65),var(64))),
	cf(1,eq(attr(var(65),'ARG0'),var(24))),
	cf(1,eq(attr(var(65),'ARG1'),var(61))),
	cf(1,eq(attr(var(65),'LBL'),var(22))),
	cf(1,eq(attr(var(65),'_CAT'),'v')),
	cf(1,eq(attr(var(65),'relation'),semform('ankomme',5,[],[]))),
	cf(1,eq(attr(var(66),'ARG0'),var(24))),
	cf(1,eq(attr(var(66),'LBL'),var(23))),
	cf(1,eq(proj(var(67),'m::'),var(68))),
	cf(1,eq(proj(var(69),'m::'),var(68))),
	cf(1,eq(proj(var(70),'m::'),var(71))),
	cf(1,eq(proj(var(72),'m::'),var(71))),
	cf(1,eq(proj(var(73),'m::'),var(74))),
	cf(1,eq(proj(var(75),'m::'),var(74)))
	],
	% C-Structure:
	[
	cf(1,subtree(1027,'ROOT',1066,113)),
	cf(1,phi(1027,var(0))),
	cf(1,subtree(1066,'ROOT',-,2504)),
	cf(1,phi(1066,var(0))),
	cf(1,subtree(2504,'IPcoord',2476,2495)),
	cf(1,phi(2504,var(0))),
	cf(1,subtree(2476,'IPcoord',2430,55)),
	cf(1,phi(2476,var(0))),
	cf(1,subtree(2430,'IPcoord',-,2428)),
	cf(1,phi(2430,var(0))),
	cf(1,subtree(2428,'IP',2226,2283)),
	cf(1,phi(2428,var(2))),
	cf(1,subtree(2226,'IP',-,2527)),
	cf(1,phi(2226,var(2))),
	cf(1,subtree(2527,'NP',-,2)),
	cf(1,phi(2527,var(49))),
	cf(1,subtree(2,'N',309,4)),
	cf(1,phi(2,var(49))),
	cf(1,cproj(2,var(67))),
	cf(1,subtree(309,'N',305,6)),
	cf(1,phi(309,var(49))),
	cf(1,cproj(309,var(67))),
	cf(1,subtree(305,'N',301,8)),
	cf(1,phi(305,var(49))),
	cf(1,cproj(305,var(67))),
	cf(1,subtree(301,'N',298,10)),
	cf(1,phi(301,var(49))),
	cf(1,cproj(301,var(67))),
	cf(1,subtree(298,'N',-,12)),
	cf(1,phi(298,var(49))),
	cf(1,cproj(298,var(67))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(49))),
	cf(1,cproj(12,var(69))),
	cf(1,terminal(13,'hund',[3])),
	cf(1,phi(13,var(49))),
	cf(1,cproj(13,var(69))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(49))),
	cf(1,terminal(11,'+Noun',[3])),
	cf(1,phi(11,var(49))),
	cf(1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(1,phi(8,var(49))),
	cf(1,terminal(9,'+Masc',[3])),
	cf(1,phi(9,var(49))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(49))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(76))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(49))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(49))),
	cf(1,subtree(2283,'I\'',-,377)),
	cf(1,phi(2283,var(2))),
	cf(1,subtree(377,'Vfin',376,21)),
	cf(1,phi(377,var(2))),
	cf(1,cproj(377,var(70))),
	cf(1,subtree(376,'Vfin',375,23)),
	cf(1,phi(376,var(2))),
	cf(1,cproj(376,var(70))),
	cf(1,subtree(375,'Vfin',-,25)),
	cf(1,phi(375,var(2))),
	cf(1,cproj(375,var(70))),
	cf(1,subtree(25,'V_BASE',-,26)),
	cf(1,phi(25,var(2))),
	cf(1,cproj(25,var(72))),
	cf(1,terminal(26,'ankomme',[20])),
	cf(1,phi(26,var(2))),
	cf(1,cproj(26,var(72))),
	cf(1,subtree(23,'V_SUFF_BASE',-,24)),
	cf(1,phi(23,var(2))),
	cf(1,terminal(24,'+Verb',[20])),
	cf(1,phi(24,var(2))),
	cf(1,subtree(21,'V_SUFF_BASE',-,22)),
	cf(1,phi(21,var(2))),
	cf(1,terminal(22,'+Past',[20])),
	cf(1,phi(22,var(2))),
	cf(1,subtree(55,'CONJev',-,36)),
	cf(1,phi(55,var(0))),
	cf(1,terminal(36,'og',[36])),
	cf(1,phi(36,var(0))),
	cf(1,subtree(2495,'IP',2491,1449)),
	cf(1,phi(2495,var(1))),
	cf(1,subtree(2491,'IP',-,1842)),
	cf(1,phi(2491,var(1))),
	cf(1,subtree(1842,'PROPP',-,1514)),
	cf(1,phi(1842,var(27))),
	cf(1,subtree(1514,'PROP',1513,59)),
	cf(1,phi(1514,var(27))),
	cf(1,subtree(1513,'PROP',1510,61)),
	cf(1,phi(1513,var(27))),
	cf(1,subtree(1510,'PROP',-,63)),
	cf(1,phi(1510,var(27))),
	cf(1,subtree(63,'PROP_BASE',-,64)),
	cf(1,phi(63,var(27))),
	cf(1,terminal(64,'Browne',[58])),
	cf(1,phi(64,var(77))),
	cf(1,subtree(61,'N_SUFF_BASE',-,62)),
	cf(1,phi(61,var(27))),
	cf(1,terminal(62,'+Prop',[58])),
	cf(1,phi(62,var(27))),
	cf(1,subtree(59,'N_SUFF_BASE',-,60)),
	cf(1,phi(59,var(27))),
	cf(1,terminal(60,'+Indef',[58])),
	cf(1,phi(60,var(27))),
	cf(1,subtree(1449,'I\'',-,625)),
	cf(1,phi(1449,var(1))),
	cf(1,subtree(625,'Vfin',624,69)),
	cf(1,phi(625,var(1))),
	cf(1,cproj(625,var(73))),
	cf(1,subtree(624,'Vfin',623,71)),
	cf(1,phi(624,var(1))),
	cf(1,cproj(624,var(73))),
	cf(1,subtree(623,'Vfin',-,73)),
	cf(1,phi(623,var(1))),
	cf(1,cproj(623,var(73))),
	cf(1,subtree(73,'V_BASE',-,74)),
	cf(1,phi(73,var(1))),
	cf(1,cproj(73,var(75))),
	cf(1,terminal(74,'bjeffe',[68])),
	cf(1,phi(74,var(1))),
	cf(1,cproj(74,var(75))),
	cf(1,subtree(71,'V_SUFF_BASE',-,72)),
	cf(1,phi(71,var(1))),
	cf(1,terminal(72,'+Verb',[68])),
	cf(1,phi(72,var(1))),
	cf(1,subtree(69,'V_SUFF_BASE',-,70)),
	cf(1,phi(69,var(1))),
	cf(1,terminal(70,'+Past',[68])),
	cf(1,phi(70,var(1))),
	cf(1,subtree(113,'PERIOD',-,106)),
	cf(1,phi(113,var(0))),
	cf(1,terminal(106,'.',[106])),
	cf(1,phi(106,var(0))),
	cf(1,semform_data(0,12,1,4)),
	cf(1,semform_data(1,12,1,4)),
	cf(1,semform_data(3,6,7,7)),
	cf(1,semform_data(4,25,8,13)),
	cf(1,semform_data(5,25,8,13)),
	cf(1,semform_data(6,55,14,16)),
	cf(1,semform_data(7,63,17,22)),
	cf(1,semform_data(8,63,17,22)),
	cf(1,semform_data(9,63,17,22)),
	cf(1,semform_data(11,73,24,31)),
	cf(1,semform_data(12,73,24,31)),
	cf(1,fspan(var(0),1,32)),
	cf(1,fspan(var(1),17,32)),
	cf(1,fspan(var(2),1,13)),
	cf(1,fspan(var(49),1,7)),
	cf(1,fspan(var(27),17,23)),
	cf(1,surfaceform(3,'hunden',1,7)),
	cf(1,surfaceform(20,'ankom',8,13)),
	cf(1,surfaceform(36,'og',14,16)),
	cf(1,surfaceform(58,'Browne',17,23)),
	cf(1,surfaceform(68,'bjeffet',24,32)),
	cf(1,surfaceform(106,'.',31,32))
	]).
