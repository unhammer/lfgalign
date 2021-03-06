% -*- coding: iso-8859-1 -*-

fstructure('Å jage katten er gammelt.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.17 CPU seconds, 225 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('være',10,[var(5),var(6)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(5))),
	cf(1,eq(attr(var(0),'PREDLINK'),var(6))),
	cf(1,eq(attr(var(0),'TOPIC'),var(5))),
	cf(1,eq(attr(var(0),'CHECK'),var(7))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(8))),
	cf(1,eq(proj(var(0),'m::'),var(9))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(5),'PRED'),semform('jage',1,[var(10),var(11)],['NULL']))),
	cf(1,eq(attr(var(5),'SUBJ'),var(10))),
	cf(1,eq(attr(var(5),'OBJ'),var(11))),
	cf(1,eq(attr(var(5),'CHECK'),var(12))),
	cf(1,eq(attr(var(5),'GEND'),var(13))),
	cf(1,eq(proj(var(5),'m::'),var(14))),
	cf(1,eq(attr(var(5),'COMP-FORM'),'å')),
	cf(1,eq(attr(var(5),'NUM'),'sg')),
	cf(1,eq(attr(var(5),'VFORM'),'inf')),
	cf(1,eq(attr(var(5),'VTYPE'),'main')),
	cf(1,eq(attr(var(10),'PRED'),semform('pro',0,[],[]))),
	cf(1,eq(proj(var(10),'m::'),var(15))),
	cf(1,eq(attr(var(10),'PRON-TYPE'),'null')),
	cf(1,eq(attr(var(15),'H-CONS'),var(16))),
	cf(1,eq(attr(var(15),'RELS_EL'),var(17))),
	cf(1,eq(attr(var(17),'ARG0'),var(18))),
	cf(1,eq(attr(var(18),'type'),'non_expl-ind')),
	cf(1,eq(attr(var(11),'PRED'),var(19))),
	cf(1,eq(attr(var(11),'CHECK'),var(20))),
	cf(1,eq(attr(var(11),'GEND'),var(21))),
	cf(1,eq(attr(var(11),'NTYPE'),var(22))),
	cf(1,eq(proj(var(11),'m::'),var(23))),
	cf(1,eq(attr(var(11),'CASE'),'obl')),
	cf(1,eq(attr(var(11),'DEF'),'+')),
	cf(1,eq(attr(var(11),'NUM'),'sg')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(A1,eq(var(19),semform('katte',5,[],[]))),
	cf(A2,eq(var(19),semform('katt',7,[],[]))),
	cf(1,eq(attr(var(20),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(20),'_NOUN'),'+')),
	cf(1,eq(attr(var(20),'_PREPEXISTS'),'-')),
	cf(1,eq(attr(var(21),'FEM'),'-')),
	cf(1,eq(attr(var(21),'MASC'),'+')),
	cf(1,eq(attr(var(21),'NEUT'),'-')),
	cf(1,eq(attr(var(22),'NSEM'),var(24))),
	cf(1,eq(attr(var(22),'NSYN'),'common')),
	cf(1,eq(attr(var(24),'COMMON'),'count')),
	cf(1,eq(attr(var(23),'H-CONS'),var(25))),
	cf(1,eq(attr(var(23),'RELS'),var(26))),
	cf(1,eq(attr(var(23),'RELS_EL'),var(2))),
	cf(1,eq(attr(var(23),'TOP'),var(27))),
	cf(1,eq(attr(var(23),'_ANTECEDVAR'),var(18))),
	cf(1,eq(attr(var(23),'_QUANT'),var(28))),
	cf(1,in_set(var(29),var(25))),
	cf(1,eq(attr(var(29),'OUTSCPD'),var(30))),
	cf(1,eq(attr(var(29),'SC_ARG'),var(31))),
	cf(1,eq(attr(var(29),'relation'),'qeq')),
	cf(1,eq(attr(var(30),'type'),'handle')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,in_set(var(1),var(26))),
	cf(1,in_set(var(2),var(26))),
	cf(1,eq(attr(var(1),'ARG0'),var(32))),
	cf(1,eq(attr(var(1),'BODY'),var(33))),
	cf(1,eq(attr(var(1),'LBL'),var(27))),
	cf(1,eq(attr(var(1),'RSTR'),var(31))),
	cf(1,eq(attr(var(1),'relation'),semform('def_q',9,[],[]))),
	cf(1,eq(attr(var(32),'DIV'),'-')),
	cf(1,eq(attr(var(32),'GRIND'),'-')),
	cf(1,eq(attr(var(32),'NATGEND'),'gender')),
	cf(1,eq(attr(var(32),'NUM'),'sg')),
	cf(1,eq(attr(var(32),'PERS'),'3')),
	cf(1,eq(attr(var(32),'type'),'ref-ind')),
	cf(1,eq(attr(var(33),'type'),'handle')),
	cf(1,eq(attr(var(27),'type'),'handle')),
	cf(1,eq(attr(var(2),'ARG0'),var(32))),
	cf(1,eq(attr(var(2),'LBL'),var(30))),
	cf(1,eq(attr(var(2),'relation'),var(34))),
	cf(1,eq(attr(var(2),'_CAT'),'n')),
	cf(A1,eq(var(34),semform('katte',6,[],[]))),
	cf(A2,eq(var(34),semform('katt',8,[],[]))),
	cf(1,eq(attr(var(28),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(28),'TOP'),var(35))),
	cf(1,eq(attr(var(35),'type'),'handle')),
	cf(1,eq(attr(var(12),'_OBJINSITU'),'+')),
	cf(1,eq(attr(var(12),'_TOPVP'),'+')),
	cf(1,eq(attr(var(13),'FEM'),'-')),
	cf(1,eq(attr(var(13),'MASC'),'-')),
	cf(1,eq(attr(var(13),'NEUT'),'+')),
	cf(1,eq(attr(var(14),'H-CONS'),var(16))),
	cf(1,eq(attr(var(14),'RELS'),var(36))),
	cf(1,eq(attr(var(14),'RELS_EL'),var(37))),
	cf(1,in_set(var(37),var(36))),
	cf(1,eq(attr(var(37),'ARG0'),var(38))),
	cf(1,eq(attr(var(37),'ARG1'),var(18))),
	cf(1,eq(attr(var(37),'ARG2'),var(32))),
	cf(1,eq(attr(var(37),'LBL'),var(39))),
	cf(1,eq(attr(var(37),'_CAT'),'v')),
	cf(1,eq(attr(var(37),'relation'),semform('jage',2,[],[]))),
	cf(1,eq(attr(var(38),'PERF'),'-')),
	cf(1,eq(attr(var(38),'TENSE'),'notense')),
	cf(1,eq(attr(var(38),'type'),'event')),
	cf(1,eq(attr(var(39),'type'),'handle')),
	cf(1,eq(attr(var(6),'PRED'),semform('gammel',13,[],[]))),
	cf(1,eq(attr(var(6),'CHECK'),var(40))),
	cf(1,eq(attr(var(6),'GEND'),var(13))),
	cf(1,eq(proj(var(6),'m::'),var(41))),
	cf(1,eq(attr(var(6),'ATYPE'),'predicative')),
	cf(1,eq(attr(var(6),'DEF'),'-')),
	cf(1,eq(attr(var(6),'MEASDIM'),'age')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(40),'_ADVERBIAL'),'-')),
	cf(1,eq(attr(var(40),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(40),'_DEG-MORPH'),'positive')),
	cf(1,eq(attr(var(41),'RELS'),var(42))),
	cf(1,eq(attr(var(41),'RELS_EL'),var(43))),
	cf(1,eq(attr(var(41),'_ANTECEDVAR'),var(38))),
	cf(1,eq(attr(var(41),'_MSG'),var(44))),
	cf(1,in_set(var(43),var(42))),
	cf(1,eq(attr(var(43),'ARG0'),var(45))),
	cf(1,eq(attr(var(43),'ARG1'),var(46))),
	cf(1,eq(attr(var(43),'LBL'),var(47))),
	cf(1,eq(attr(var(43),'_CAT'),'a')),
	cf(1,eq(attr(var(43),'relation'),semform('gammel',14,[],[]))),
	cf(1,eq(attr(var(45),'PERF'),'-')),
	cf(1,eq(attr(var(45),'SF'),'prop')),
	cf(1,eq(attr(var(45),'TENSE'),'pres')),
	cf(1,eq(attr(var(45),'type'),'event')),
	cf(1,eq(attr(var(46),'type'),'handle')),
	cf(1,eq(attr(var(47),'type'),'handle')),
	cf(1,eq(attr(var(44),'ARG0'),var(45))),
	cf(1,eq(attr(var(44),'LBL'),var(48))),
	cf(1,eq(attr(var(48),'type'),'handle')),
	cf(1,eq(attr(var(7),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(8),'MOOD'),'indicative')),
	cf(1,eq(attr(var(8),'TENSE'),'pres')),
	cf(1,eq(attr(var(9),'H-CONS'),var(49))),
	cf(1,eq(attr(var(9),'INDEX'),var(45))),
	cf(1,eq(attr(var(9),'RELS'),var(50))),
	cf(1,eq(attr(var(9),'RELS_EL'),var(43))),
	cf(1,eq(attr(var(9),'TOP'),var(48))),
	cf(1,eq(attr(var(9),'_MSG'),var(44))),
	cf(1,eq(attr(var(9),'_MSGQEQ'),var(3))),
	cf(1,in_set(var(3),var(49))),
	cf(1,in_set(var(4),var(49))),
	cf(1,eq(attr(var(3),'OUTSCPD'),var(47))),
	cf(1,eq(attr(var(3),'SC_ARG'),var(48))),
	cf(1,eq(attr(var(3),'relation'),'qeq')),
	cf(1,eq(attr(var(4),'OUTSCPD'),var(39))),
	cf(1,eq(attr(var(4),'SC_ARG'),var(46))),
	cf(1,eq(attr(var(4),'relation'),'qeq')),
	cf(1,in_set(var(51),var(50))),
	cf(1,eq(attr(var(51),'ARG1'),var(45))),
	cf(1,eq(attr(var(51),'LBL'),var(47))),
	cf(1,eq(attr(var(51),'relation'),semform('stative_asp',12,[],[]))),
	cf(1,eq(proj(var(52),'m::'),var(53))),
	cf(1,eq(proj(var(54),'m::'),var(53))),
	cf(A1,eq(var(55),var(56))),
	cf(A2,eq(var(55),var(57))),
	cf(A2,eq(proj(var(58),'m::'),var(57))),
	cf(1,eq(proj(var(59),'m::'),var(55))),
	cf(A1,eq(proj(var(60),'m::'),var(56)))
	],
	% C-Structure:
	[
	cf(1,subtree(1009,'ROOT',1113,125)),
	cf(1,phi(1009,var(0))),
	cf(1,subtree(1113,'ROOT',-,1825)),
	cf(1,phi(1113,var(0))),
	cf(1,subtree(1825,'IP',1723,1831)),
	cf(1,phi(1825,var(0))),
	cf(1,subtree(1723,'IP',-,1722)),
	cf(1,phi(1723,var(0))),
	cf(1,subtree(1722,'VP\'',298,1601)),
	cf(1,phi(1722,var(5))),
	cf(1,subtree(298,'VP\'',-,35)),
	cf(1,phi(298,var(5))),
	cf(1,subtree(35,'PARTinf',-,1)),
	cf(1,phi(35,var(5))),
	cf(1,terminal(1,'å',[1])),
	cf(1,phi(1,var(5))),
	cf(1,subtree(1601,'VP',313,1937)),
	cf(1,phi(1601,var(5))),
	cf(1,subtree(313,'VP',-,309)),
	cf(1,phi(313,var(5))),
	cf(1,subtree(309,'V',308,38)),
	cf(1,phi(309,var(5))),
	cf(1,cproj(309,var(52))),
	cf(1,subtree(308,'V',307,40)),
	cf(1,phi(308,var(5))),
	cf(1,cproj(308,var(52))),
	cf(1,subtree(307,'V',-,42)),
	cf(1,phi(307,var(5))),
	cf(1,cproj(307,var(52))),
	cf(1,subtree(42,'V_BASE',-,43)),
	cf(1,phi(42,var(5))),
	cf(1,cproj(42,var(54))),
	cf(1,terminal(43,'jage',[37])),
	cf(1,phi(43,var(5))),
	cf(1,cproj(43,var(54))),
	cf(1,subtree(40,'V_SUFF_BASE',-,41)),
	cf(1,phi(40,var(5))),
	cf(1,terminal(41,'+Verb',[37])),
	cf(1,phi(41,var(5))),
	cf(1,subtree(38,'V_SUFF_BASE',-,39)),
	cf(1,phi(38,var(5))),
	cf(1,terminal(39,'+Infin',[37])),
	cf(1,phi(39,var(5))),
	cf(1,subtree(1937,'NP',-,53)),
	cf(1,phi(1937,var(11))),
	cf(1,subtree(53,'N',457,54)),
	cf(1,phi(53,var(11))),
	cf(1,cproj(53,var(59))),
	cf(1,subtree(457,'N',456,56)),
	cf(1,phi(457,var(11))),
	cf(1,cproj(457,var(59))),
	cf(A2,subtree(456,'N',455,71)),
	cf(A2,phi(456,var(11))),
	cf(A2,cproj(456,var(59))),
	cf(A2,subtree(455,'N',418,73)),
	cf(A2,phi(455,var(11))),
	cf(A2,cproj(455,var(59))),
	cf(A2,subtree(418,'N',-,75)),
	cf(A2,phi(418,var(11))),
	cf(A2,cproj(418,var(59))),
	cf(A2,subtree(75,'N_BASE',-,76)),
	cf(A2,phi(75,var(11))),
	cf(A2,cproj(75,var(58))),
	cf(A2,terminal(76,'katt',[52])),
	cf(A2,phi(76,var(11))),
	cf(A2,cproj(76,var(58))),
	cf(A2,subtree(73,'N_SUFF_BASE',-,74)),
	cf(A2,phi(73,var(11))),
	cf(A2,terminal(74,'+Noun',[52])),
	cf(A2,phi(74,var(11))),
	cf(A2,subtree(71,'N_SUFF_BASE',-,72)),
	cf(A2,phi(71,var(11))),
	cf(A2,terminal(72,'+Masc',[52])),
	cf(A2,phi(72,var(11))),
	cf(A1,subtree(456,'N',458,58)),
	cf(A1,phi(456,var(11))),
	cf(A1,cproj(456,var(59))),
	cf(A1,subtree(458,'N',417,60)),
	cf(A1,phi(458,var(11))),
	cf(A1,cproj(458,var(59))),
	cf(A1,subtree(417,'N',-,64)),
	cf(A1,phi(417,var(11))),
	cf(A1,cproj(417,var(59))),
	cf(A1,subtree(64,'N_BASE',-,63)),
	cf(A1,phi(64,var(11))),
	cf(A1,cproj(64,var(60))),
	cf(A1,terminal(63,'katte',[52])),
	cf(A1,phi(63,var(11))),
	cf(A1,cproj(63,var(60))),
	cf(A1,subtree(60,'N_SUFF_BASE',-,61)),
	cf(A1,phi(60,var(11))),
	cf(A1,terminal(61,'+Noun',[52])),
	cf(A1,phi(61,var(11))),
	cf(A1,subtree(58,'N_SUFF_BASE',-,59)),
	cf(A1,phi(58,var(11))),
	cf(A1,terminal(59,'+Masc',[52])),
	cf(A1,phi(59,var(11))),
	cf(1,subtree(56,'N_SUFF_BASE',-,57)),
	cf(1,phi(56,var(11))),
	cf(1,terminal(57,'+Def',[52])),
	cf(1,phi(57,var(61))),
	cf(1,subtree(54,'N_SUFF_BASE',-,55)),
	cf(1,phi(54,var(11))),
	cf(1,terminal(55,'+Sg',[52])),
	cf(1,phi(55,var(11))),
	cf(1,subtree(1831,'I\'cop',1830,1815)),
	cf(1,phi(1831,var(0))),
	cf(1,subtree(1830,'I\'cop',-,88)),
	cf(1,phi(1830,var(0))),
	cf(1,subtree(88,'Vcopfin',-,83)),
	cf(1,phi(88,var(0))),
	cf(1,terminal(83,'er',[83])),
	cf(1,phi(83,var(62))),
	cf(1,subtree(1815,'Scop',-,1771)),
	cf(1,phi(1815,var(0))),
	cf(1,subtree(1771,'VPmain2',-,660)),
	cf(1,phi(1771,var(0))),
	cf(1,subtree(660,'AP',-,658)),
	cf(1,phi(660,var(6))),
	cf(1,subtree(658,'A',657,99)),
	cf(1,phi(658,var(6))),
	cf(1,subtree(657,'A',656,101)),
	cf(1,phi(657,var(6))),
	cf(1,subtree(656,'A',655,103)),
	cf(1,phi(656,var(6))),
	cf(1,subtree(655,'A',654,105)),
	cf(1,phi(655,var(6))),
	cf(1,subtree(654,'A',647,107)),
	cf(1,phi(654,var(6))),
	cf(1,subtree(647,'A',-,109)),
	cf(1,phi(647,var(6))),
	cf(1,subtree(109,'A_BASE',-,110)),
	cf(1,phi(109,var(6))),
	cf(1,terminal(110,'gammel',[98])),
	cf(1,phi(110,var(6))),
	cf(1,subtree(107,'A_SUFF_BASE',-,108)),
	cf(1,phi(107,var(6))),
	cf(1,terminal(108,'+Adj',[98])),
	cf(1,phi(108,var(6))),
	cf(1,subtree(105,'A_SUFF_BASE',-,106)),
	cf(1,phi(105,var(6))),
	cf(1,terminal(106,'+Pos',[98])),
	cf(1,phi(106,var(6))),
	cf(1,subtree(103,'N_SUFF_BASE',-,104)),
	cf(1,phi(103,var(6))),
	cf(1,terminal(104,'+Neut',[98])),
	cf(1,phi(104,var(6))),
	cf(1,subtree(101,'N_SUFF_BASE',-,102)),
	cf(1,phi(101,var(6))),
	cf(1,terminal(102,'+Indef',[98])),
	cf(1,phi(102,var(6))),
	cf(1,subtree(99,'N_SUFF_BASE',-,100)),
	cf(1,phi(99,var(6))),
	cf(1,terminal(100,'+Sg',[98])),
	cf(1,phi(100,var(6))),
	cf(1,subtree(125,'PERIOD',-,118)),
	cf(1,phi(125,var(0))),
	cf(1,terminal(118,'.',[118])),
	cf(1,phi(118,var(0))),
	cf(1,semform_data(0,1723,1,14)),
	cf(1,semform_data(1,42,3,6)),
	cf(1,semform_data(2,42,3,6)),
	cf(A1,semform_data(5,64,8,13)),
	cf(A1,semform_data(6,64,8,13)),
	cf(A2,semform_data(7,75,8,11)),
	cf(A2,semform_data(8,75,8,11)),
	cf(1,semform_data(9,56,14,14)),
	cf(1,semform_data(10,88,15,17)),
	cf(1,semform_data(12,88,15,17)),
	cf(1,semform_data(13,109,18,23)),
	cf(1,semform_data(14,109,18,23)),
	cf(1,fspan(var(0),1,26)),
	cf(1,fspan(var(5),1,14)),
	cf(1,fspan(var(11),8,14)),
	cf(1,fspan(var(6),18,26)),
	cf(1,surfaceform(1,'å',1,2)),
	cf(1,surfaceform(37,'jage',3,7)),
	cf(1,surfaceform(52,'katten',8,14)),
	cf(1,surfaceform(83,'er',15,17)),
	cf(1,surfaceform(98,'gammelt',18,26)),
	cf(1,surfaceform(118,'.',25,26))
	]).

