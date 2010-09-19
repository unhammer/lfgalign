% -*- coding: iso-8859-1 -*-

fstructure('Katten er gammel.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('2 solutions, 0.07 CPU seconds, 161 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('være',6,[var(3),var(4)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'PREDLINK'),var(4))),
	cf(1,eq(attr(var(0),'TOPIC'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(6))),
	cf(1,eq(proj(var(0),'m::'),var(7))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(3),'PRED'),var(8))),
	cf(1,eq(attr(var(3),'CHECK'),var(9))),
	cf(1,eq(attr(var(3),'GEND'),var(10))),
	cf(1,eq(attr(var(3),'NTYPE'),var(11))),
	cf(1,eq(proj(var(3),'m::'),var(12))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'DEF'),'+')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(A1,eq(var(8),semform('katte',0,[],[]))),
	cf(A2,eq(var(8),semform('katt',2,[],[]))),
	cf(1,eq(attr(var(9),'_DEF-MORPH'),'+')),
	cf(1,eq(attr(var(9),'_NOUN'),'+')),
	cf(1,eq(attr(var(10),'FEM'),'-')),
	cf(1,eq(attr(var(10),'MASC'),'+')),
	cf(1,eq(attr(var(10),'NEUT'),'-')),
	cf(1,eq(attr(var(11),'NSEM'),var(13))),
	cf(1,eq(attr(var(11),'NSYN'),'common')),
	cf(1,eq(attr(var(13),'COMMON'),'count')),
	cf(1,eq(attr(var(12),'H-CONS'),var(14))),
	cf(1,eq(attr(var(12),'RELS'),var(15))),
	cf(1,eq(attr(var(12),'RELS_EL'),var(2))),
	cf(1,eq(attr(var(12),'TOP'),var(16))),
	cf(1,eq(attr(var(12),'_QUANT'),var(17))),
	cf(1,in_set(var(18),var(14))),
	cf(1,eq(attr(var(18),'OUTSCPD'),var(19))),
	cf(1,eq(attr(var(18),'SC_ARG'),var(20))),
	cf(1,eq(attr(var(18),'relation'),'qeq')),
	cf(1,eq(attr(var(19),'type'),'handle')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,in_set(var(1),var(15))),
	cf(1,in_set(var(2),var(15))),
	cf(1,eq(attr(var(1),'ARG0'),var(21))),
	cf(1,eq(attr(var(1),'BODY'),var(22))),
	cf(1,eq(attr(var(1),'LBL'),var(16))),
	cf(1,eq(attr(var(1),'RSTR'),var(20))),
	cf(1,eq(attr(var(1),'relation'),semform('def_q',5,[],[]))),
	cf(1,eq(attr(var(21),'DIV'),'-')),
	cf(1,eq(attr(var(21),'GRIND'),'-')),
	cf(1,eq(attr(var(21),'NATGEND'),'gender')),
	cf(1,eq(attr(var(21),'NUM'),'sg')),
	cf(1,eq(attr(var(21),'PERS'),'3')),
	cf(1,eq(attr(var(21),'type'),'ref-ind')),
	cf(1,eq(attr(var(22),'type'),'handle')),
	cf(1,eq(attr(var(16),'type'),'handle')),
	cf(1,eq(attr(var(2),'ARG0'),var(21))),
	cf(1,eq(attr(var(2),'LBL'),var(19))),
	cf(1,eq(attr(var(2),'relation'),var(23))),
	cf(1,eq(attr(var(2),'_CAT'),'n')),
	cf(A1,eq(var(23),semform('katte',1,[],[]))),
	cf(A2,eq(var(23),semform('katt',3,[],[]))),
	cf(1,eq(attr(var(17),'RELS_EL'),var(1))),
	cf(1,eq(attr(var(17),'TOP'),var(24))),
	cf(1,eq(attr(var(24),'type'),'handle')),
	cf(1,eq(attr(var(4),'PRED'),semform('gammel',9,[],[]))),
	cf(1,eq(attr(var(4),'CHECK'),var(25))),
	cf(1,eq(attr(var(4),'GEND'),var(10))),
	cf(1,eq(proj(var(4),'m::'),var(26))),
	cf(1,eq(attr(var(4),'ATYPE'),'predicative')),
	cf(1,eq(attr(var(4),'DEF'),'-')),
	cf(1,eq(attr(var(4),'MEASDIM'),'age')),
	cf(1,eq(attr(var(4),'NUM'),'sg')),
	cf(1,eq(attr(var(25),'_ADVERBIAL'),'-')),
	cf(1,eq(attr(var(25),'_DEF-MORPH'),'-')),
	cf(1,eq(attr(var(25),'_DEG-MORPH'),'positive')),
	cf(1,eq(attr(var(26),'RELS'),var(27))),
	cf(1,eq(attr(var(26),'RELS_EL'),var(28))),
	cf(1,eq(attr(var(26),'_ANTECEDVAR'),var(21))),
	cf(1,eq(attr(var(26),'_MSG'),var(29))),
	cf(1,in_set(var(28),var(27))),
	cf(1,eq(attr(var(28),'ARG0'),var(30))),
	cf(1,eq(attr(var(28),'ARG1'),var(21))),
	cf(1,eq(attr(var(28),'LBL'),var(31))),
	cf(1,eq(attr(var(28),'_CAT'),'a')),
	cf(1,eq(attr(var(28),'relation'),semform('gammel',10,[],[]))),
	cf(1,eq(attr(var(30),'PERF'),'-')),
	cf(1,eq(attr(var(30),'SF'),'prop')),
	cf(1,eq(attr(var(30),'TENSE'),'pres')),
	cf(1,eq(attr(var(30),'type'),'event')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(attr(var(29),'ARG0'),var(30))),
	cf(1,eq(attr(var(29),'LBL'),var(32))),
	cf(1,eq(attr(var(32),'type'),'handle')),
	cf(1,eq(attr(var(5),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(6),'MOOD'),'indicative')),
	cf(1,eq(attr(var(6),'TENSE'),'pres')),
	cf(1,eq(attr(var(7),'H-CONS'),var(33))),
	cf(1,eq(attr(var(7),'INDEX'),var(30))),
	cf(1,eq(attr(var(7),'RELS'),var(34))),
	cf(1,eq(attr(var(7),'RELS_EL'),var(28))),
	cf(1,eq(attr(var(7),'TOP'),var(32))),
	cf(1,eq(attr(var(7),'_MSG'),var(29))),
	cf(1,eq(attr(var(7),'_MSGQEQ'),var(35))),
	cf(1,in_set(var(35),var(33))),
	cf(1,eq(attr(var(35),'OUTSCPD'),var(31))),
	cf(1,eq(attr(var(35),'SC_ARG'),var(32))),
	cf(1,eq(attr(var(35),'relation'),'qeq')),
	cf(1,in_set(var(36),var(34))),
	cf(1,eq(attr(var(36),'ARG1'),var(30))),
	cf(1,eq(attr(var(36),'LBL'),var(31))),
	cf(1,eq(attr(var(36),'relation'),semform('stative_asp',8,[],[]))),
	cf(A1,eq(var(37),var(38))),
	cf(A2,eq(var(37),var(39))),
	cf(A2,eq(proj(var(40),'m::'),var(39))),
	cf(1,eq(proj(var(41),'m::'),var(37))),
	cf(A1,eq(proj(var(42),'m::'),var(38)))
	],
	% C-Structure:
	[
	cf(1,subtree(821,'ROOT',902,75)),
	cf(1,phi(821,var(0))),
	cf(1,subtree(902,'ROOT',-,1404)),
	cf(1,phi(902,var(0))),
	cf(1,subtree(1404,'IP',1309,1410)),
	cf(1,phi(1404,var(0))),
	cf(1,subtree(1309,'IP',-,1473)),
	cf(1,phi(1309,var(0))),
	cf(1,subtree(1473,'NP',-,2)),
	cf(1,phi(1473,var(3))),
	cf(1,subtree(2,'N',293,4)),
	cf(1,phi(2,var(3))),
	cf(1,cproj(2,var(41))),
	cf(1,subtree(293,'N',267,6)),
	cf(1,phi(293,var(3))),
	cf(1,cproj(293,var(41))),
	cf(A2,subtree(267,'N',263,21)),
	cf(A2,phi(267,var(3))),
	cf(A2,cproj(267,var(41))),
	cf(A2,subtree(263,'N',260,23)),
	cf(A2,phi(263,var(3))),
	cf(A2,cproj(263,var(41))),
	cf(A2,subtree(260,'N',-,25)),
	cf(A2,phi(260,var(3))),
	cf(A2,cproj(260,var(41))),
	cf(A2,subtree(25,'N_BASE',-,26)),
	cf(A2,phi(25,var(3))),
	cf(A2,cproj(25,var(40))),
	cf(A2,terminal(26,'katt',[3])),
	cf(A2,phi(26,var(3))),
	cf(A2,cproj(26,var(40))),
	cf(A2,subtree(23,'N_SUFF_BASE',-,24)),
	cf(A2,phi(23,var(3))),
	cf(A2,terminal(24,'+Noun',[3])),
	cf(A2,phi(24,var(3))),
	cf(A2,subtree(21,'N_SUFF_BASE',-,22)),
	cf(A2,phi(21,var(3))),
	cf(A2,terminal(22,'+Masc',[3])),
	cf(A2,phi(22,var(3))),
	cf(A1,subtree(267,'N',289,8)),
	cf(A1,phi(267,var(3))),
	cf(A1,cproj(267,var(41))),
	cf(A1,subtree(289,'N',282,10)),
	cf(A1,phi(289,var(3))),
	cf(A1,cproj(289,var(41))),
	cf(A1,subtree(282,'N',-,14)),
	cf(A1,phi(282,var(3))),
	cf(A1,cproj(282,var(41))),
	cf(A1,subtree(14,'N_BASE',-,13)),
	cf(A1,phi(14,var(3))),
	cf(A1,cproj(14,var(42))),
	cf(A1,terminal(13,'katte',[3])),
	cf(A1,phi(13,var(3))),
	cf(A1,cproj(13,var(42))),
	cf(A1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(A1,phi(10,var(3))),
	cf(A1,terminal(11,'+Noun',[3])),
	cf(A1,phi(11,var(3))),
	cf(A1,subtree(8,'N_SUFF_BASE',-,9)),
	cf(A1,phi(8,var(3))),
	cf(A1,terminal(9,'+Masc',[3])),
	cf(A1,phi(9,var(3))),
	cf(1,subtree(6,'N_SUFF_BASE',-,7)),
	cf(1,phi(6,var(3))),
	cf(1,terminal(7,'+Def',[3])),
	cf(1,phi(7,var(43))),
	cf(1,subtree(4,'N_SUFF_BASE',-,5)),
	cf(1,phi(4,var(3))),
	cf(1,terminal(5,'+Sg',[3])),
	cf(1,phi(5,var(3))),
	cf(1,subtree(1410,'I\'cop',1409,1394)),
	cf(1,phi(1410,var(0))),
	cf(1,subtree(1409,'I\'cop',-,38)),
	cf(1,phi(1409,var(0))),
	cf(1,subtree(38,'Vcopfin',-,33)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(33,'er',[33])),
	cf(1,phi(33,var(44))),
	cf(1,subtree(1394,'Scop',-,1350)),
	cf(1,phi(1394,var(0))),
	cf(1,subtree(1350,'VPmain2',-,492)),
	cf(1,phi(1350,var(0))),
	cf(1,subtree(492,'AP',-,490)),
	cf(1,phi(492,var(4))),
	cf(1,subtree(490,'A',489,49)),
	cf(1,phi(490,var(4))),
	cf(1,subtree(489,'A',488,51)),
	cf(1,phi(489,var(4))),
	cf(1,subtree(488,'A',487,53)),
	cf(1,phi(488,var(4))),
	cf(1,subtree(487,'A',486,55)),
	cf(1,phi(487,var(4))),
	cf(1,subtree(486,'A',479,57)),
	cf(1,phi(486,var(4))),
	cf(1,subtree(479,'A',-,59)),
	cf(1,phi(479,var(4))),
	cf(1,subtree(59,'A_BASE',-,60)),
	cf(1,phi(59,var(4))),
	cf(1,terminal(60,'gammel',[48])),
	cf(1,phi(60,var(4))),
	cf(1,subtree(57,'A_SUFF_BASE',-,58)),
	cf(1,phi(57,var(4))),
	cf(1,terminal(58,'+Adj',[48])),
	cf(1,phi(58,var(4))),
	cf(1,subtree(55,'A_SUFF_BASE',-,56)),
	cf(1,phi(55,var(4))),
	cf(1,terminal(56,'+Pos',[48])),
	cf(1,phi(56,var(4))),
	cf(1,subtree(53,'N_SUFF_BASE',-,54)),
	cf(1,phi(53,var(4))),
	cf(1,terminal(54,'+MF',[48])),
	cf(1,phi(54,var(4))),
	cf(1,subtree(51,'N_SUFF_BASE',-,52)),
	cf(1,phi(51,var(4))),
	cf(1,terminal(52,'+Indef',[48])),
	cf(1,phi(52,var(4))),
	cf(1,subtree(49,'N_SUFF_BASE',-,50)),
	cf(1,phi(49,var(4))),
	cf(1,terminal(50,'+Sg',[48])),
	cf(1,phi(50,var(4))),
	cf(1,subtree(75,'PERIOD',-,68)),
	cf(1,phi(75,var(0))),
	cf(1,terminal(68,'.',[68])),
	cf(1,phi(68,var(0))),
	cf(A1,semform_data(0,14,1,6)),
	cf(A1,semform_data(1,14,1,6)),
	cf(A2,semform_data(2,25,1,4)),
	cf(A2,semform_data(3,25,1,4)),
	cf(1,semform_data(5,6,7,7)),
	cf(1,semform_data(6,38,8,10)),
	cf(1,semform_data(8,38,8,10)),
	cf(1,semform_data(9,59,11,16)),
	cf(1,semform_data(10,59,11,16)),
	cf(1,fspan(var(0),1,18)),
	cf(1,fspan(var(3),1,7)),
	cf(1,fspan(var(4),11,18)),
	cf(1,surfaceform(3,'katten',1,7)),
	cf(1,surfaceform(33,'er',8,10)),
	cf(1,surfaceform(48,'gammel',11,18)),
	cf(1,surfaceform(68,'.',17,18))
	]).

