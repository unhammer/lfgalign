% -*- coding: iso-8859-1 -*-

fstructure('Ikke bjeff!',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2008 10:36.'),
	'grammar'('/home/paul/lisp/projects/treebank/grammars/mrs/1.1//bokmal-mrs.lfg'),
	'grammar_date'('Apr 08, 2008 15:38'),
	'statistics'('1 solutions, 0.02 CPU seconds, 36 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('bjeffe',5,[var(8)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(8))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(9))),
	cf(1,eq(attr(var(0),'CHECK'),var(10))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(11))),
	cf(1,eq(proj(var(0),'m::'),var(12))),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'imp')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(8),'PRED'),semform('pro',2,[],[]))),
	cf(1,eq(attr(var(8),'GEND'),var(13))),
	cf(1,eq(proj(var(8),'m::'),var(14))),
	cf(1,eq(attr(var(8),'CASE'),'nom')),
	cf(1,eq(attr(var(8),'PERS'),'2')),
	cf(1,eq(attr(var(8),'REF'),'+')),
	cf(1,eq(attr(var(13),'NEUT'),'-')),
	cf(1,eq(attr(var(14),'H-CONS'),var(15))),
	cf(1,eq(attr(var(14),'RELS_EL'),var(4))),
	cf(1,eq(attr(var(14),'TOP'),var(16))),
	cf(1,in_set(var(1),var(15))),
	cf(1,in_set(var(2),var(15))),
	cf(1,eq(attr(var(1),'OUTSCPD'),var(17))),
	cf(1,eq(attr(var(1),'SC_ARG'),var(18))),
	cf(1,eq(attr(var(1),'relation'),'qeq')),
	cf(1,eq(attr(var(17),'type'),'handle')),
	cf(1,eq(attr(var(18),'type'),'handle')),
	cf(1,eq(attr(var(2),'OUTSCPD'),var(19))),
	cf(1,eq(attr(var(2),'SC_ARG'),var(20))),
	cf(1,eq(attr(var(2),'relation'),'qeq')),
	cf(1,eq(attr(var(19),'type'),'handle')),
	cf(1,eq(attr(var(20),'type'),'handle')),
	cf(1,eq(attr(var(4),'ARG0'),var(21))),
	cf(1,eq(attr(var(4),'LBL'),var(17))),
	cf(1,eq(attr(var(4),'relation'),semform('pron',4,[],[]))),
	cf(1,eq(attr(var(21),'PERS'),'2')),
	cf(1,eq(attr(var(21),'type'),'ref-ind')),
	cf(1,eq(attr(var(16),'type'),'handle')),
	cf(1,in_set(var(22),var(9))),
	cf(1,scopes(var(22),var(10))),
	cf(1,eq(attr(var(22),'PRED'),semform('ikke',0,[],[]))),
	cf(1,eq(proj(var(22),'m::'),var(23))),
	cf(1,eq(attr(var(22),'ADV-TYPE'),'neg')),
	cf(1,eq(attr(var(23),'H-CONS'),var(24))),
	cf(1,eq(attr(var(23),'H-CONS_EL'),var(25))),
	cf(1,eq(attr(var(23),'INDEX'),var(26))),
	cf(1,eq(attr(var(23),'RELS'),var(27))),
	cf(1,eq(attr(var(23),'RELS_EL'),var(6))),
	cf(1,in_set(var(25),var(24))),
	cf(1,eq(attr(var(25),'OUTSCPD'),var(28))),
	cf(1,eq(attr(var(25),'SC_ARG'),var(29))),
	cf(1,eq(attr(var(25),'relation'),'qeq')),
	cf(1,eq(attr(var(28),'handle'),'NXTLBL')),
	cf(1,eq(attr(var(29),'type'),'handle')),
	cf(1,eq(attr(var(26),'PERF'),'-')),
	cf(1,eq(attr(var(26),'SF'),'comm')),
	cf(1,eq(attr(var(26),'TENSE'),'pres')),
	cf(1,eq(attr(var(26),'type'),'event')),
	cf(1,in_set(var(6),var(27))),
	cf(1,eq(attr(var(6),'ARG0'),var(30))),
	cf(1,eq(attr(var(6),'ARG1'),var(29))),
	cf(1,eq(attr(var(6),'LBL'),var(31))),
	cf(1,eq(attr(var(6),'relation'),semform('neg',1,[],[]))),
	cf(1,eq(attr(var(30),'type'),'event')),
	cf(1,eq(attr(var(31),'type'),'handle')),
	cf(1,eq(proj(var(10),'m::'),var(5))),
	cf(1,eq(attr(var(10),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(10),'_MRSPTR'),'+')),
	cf(1,eq(attr(var(5),'ARG0'),var(26))),
	cf(1,eq(attr(var(5),'ARG1'),var(21))),
	cf(1,eq(attr(var(5),'LBL'),var(19))),
	cf(1,eq(attr(var(5),'_CAT'),'v')),
	cf(1,eq(attr(var(5),'relation'),semform('bjeffe',6,[],[]))),
	cf(1,eq(attr(var(11),'MOOD'),'imperative')),
	cf(1,eq(attr(var(12),'H-CONS'),var(15))),
	cf(1,eq(attr(var(12),'INDEX'),var(26))),
	cf(1,eq(attr(var(12),'RELS'),var(32))),
	cf(1,eq(attr(var(12),'RELS_EL'),var(5))),
	cf(1,eq(attr(var(12),'TOP'),var(20))),
	cf(1,eq(attr(var(12),'_MSG'),var(33))),
	cf(1,eq(attr(var(12),'_MSGQEQ'),var(2))),
	cf(1,in_set(var(3),var(32))),
	cf(1,in_set(var(4),var(32))),
	cf(1,in_set(var(5),var(32))),
	cf(1,in_set(var(6),var(32))),
	cf(1,in_set(var(7),var(32))),
	cf(1,eq(attr(var(3),'ARG0'),var(21))),
	cf(1,eq(attr(var(3),'BODY'),var(34))),
	cf(1,eq(attr(var(3),'LBL'),var(16))),
	cf(1,eq(attr(var(3),'RSTR'),var(18))),
	cf(1,eq(attr(var(3),'relation'),semform('pronoun_q',3,[],[]))),
	cf(1,eq(attr(var(34),'type'),'handle')),
	cf(1,eq(attr(var(7),'ARG1'),var(26))),
	cf(1,eq(attr(var(7),'ARG2'),var(30))),
	cf(1,eq(attr(var(7),'LBL'),var(19))),
	cf(1,eq(attr(var(7),'relation'),'topic_d')),
	cf(1,eq(attr(var(33),'ARG0'),var(26))),
	cf(1,eq(attr(var(33),'LBL'),var(20))),
	cf(1,eq(proj(var(35),'m::'),var(36))),
	cf(1,eq(proj(var(37),'m::'),var(36)))
	],
	% C-Structure:
	[
	cf(1,subtree(650,'ROOT',686,693)),
	cf(1,phi(650,var(0))),
	cf(1,subtree(686,'ROOT',-,950)),
	cf(1,phi(686,var(0))),
	cf(1,subtree(950,'IP',228,929)),
	cf(1,phi(950,var(0))),
	cf(1,subtree(228,'IP',-,16)),
	cf(1,phi(228,var(0))),
	cf(1,subtree(16,'ADVneg',-,3)),
	cf(1,phi(16,var(22))),
	cf(1,terminal(3,'ikke',[3])),
	cf(1,phi(3,var(22))),
	cf(1,subtree(929,'I\'',-,252)),
	cf(1,phi(929,var(0))),
	cf(1,subtree(252,'Vfin',251,34)),
	cf(1,phi(252,var(0))),
	cf(1,cproj(252,var(35))),
	cf(1,subtree(251,'Vfin',250,36)),
	cf(1,phi(251,var(0))),
	cf(1,cproj(251,var(35))),
	cf(1,subtree(250,'Vfin',-,38)),
	cf(1,phi(250,var(0))),
	cf(1,cproj(250,var(35))),
	cf(1,subtree(38,'V_BASE',-,39)),
	cf(1,phi(38,var(0))),
	cf(1,cproj(38,var(37))),
	cf(1,terminal(39,'bjeffe',[17])),
	cf(1,phi(39,var(0))),
	cf(1,cproj(39,var(37))),
	cf(1,subtree(36,'V_SUFF_BASE',-,37)),
	cf(1,phi(36,var(0))),
	cf(1,terminal(37,'+Verb',[17])),
	cf(1,phi(37,var(0))),
	cf(1,subtree(34,'V_SUFF_BASE',-,35)),
	cf(1,phi(34,var(0))),
	cf(1,terminal(35,'+Impv',[17])),
	cf(1,phi(35,var(0))),
	cf(1,subtree(693,'EXCL-POINT',692,49)),
	cf(1,phi(693,var(0))),
	cf(1,subtree(692,'EXCL-POINT',691,51)),
	cf(1,phi(692,var(0))),
	cf(1,subtree(691,'EXCL-POINT',-,53)),
	cf(1,phi(691,var(0))),
	cf(1,subtree(53,'EXCL-POINT_BASE',-,54)),
	cf(1,phi(53,var(0))),
	cf(1,terminal(54,'!',[48])),
	cf(1,phi(54,var(0))),
	cf(1,subtree(51,'Punct_SUFF_BASE',-,52)),
	cf(1,phi(51,var(0))),
	cf(1,terminal(52,'+Punct',[48])),
	cf(1,phi(52,var(0))),
	cf(1,subtree(49,'SENT_SUFF_BASE',-,50)),
	cf(1,phi(49,var(0))),
	cf(1,terminal(50,'+Sent',[48])),
	cf(1,phi(50,var(0))),
	cf(1,semform_data(0,16,1,5)),
	cf(1,semform_data(1,16,1,5)),
	cf(1,semform_data(2,950,1,11)),
	cf(1,semform_data(3,950,1,11)),
	cf(1,semform_data(4,950,1,11)),
	cf(1,semform_data(5,38,6,11)),
	cf(1,semform_data(6,38,6,11)),
	cf(1,fspan(var(0),1,12)),
	cf(1,fspan(var(22),1,5)),
	cf(1,surfaceform(3,'ikke',1,5)),
	cf(1,surfaceform(17,'bjeff',6,11)),
	cf(1,surfaceform(48,'!',11,12))
	]).

