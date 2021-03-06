% -*- coding: iso-8859-1 -*-

fstructure('jaGli qePs.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('2'),
	'statistics'('2+2 solutions, 0.03 CPU seconds, 53 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
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
	cf(1,eq(attr(var(0),'PRED'),var(14))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(A2,eq(attr(var(0),'OBJth'),var(12))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(15))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(14),semform('*-qePa',2,[var(3)],[]))),
	cf(A2,eq(var(14),semform('*-qePa',2,[var(3),var(12)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('jaGli',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(A2,eq(attr(var(12),'PRED'),semform('pro',6,[],[]))),
	cf(A2,eq(attr(var(12),'CHECK'),var(13))),
	cf(A2,eq(attr(var(12),'CASE'),'dat')),
	cf(A2,eq(attr(var(12),'PERS'),'3')),
	cf(A2,eq(attr(var(13),'_PRODROP'),'+')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(7))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'pres')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(7),'_AGR'),var(8))),
	cf(1,eq(attr(var(7),'_CLASS'),var(10))),
	cf(1,eq(attr(var(7),'_LEXID'),var(11))),
	cf(1,eq(attr(var(7),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(7),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(8),'_OBJ'),var(9))),
	cf(A2,eq(var(9),var(12))),
	cf(A1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(A1,eq(var(10),'MV')),
	cf(A2,eq(var(10),'RM1')),
	cf(A1,eq(var(11),'V2746-3-7')),
	cf(A2,eq(var(11),'V2746-10-11')),
	cf(1,eq(attr(var(15),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(15),'MOOD'),'indicative')),
	cf(1,eq(attr(var(15),'TENSE'),'pres')),
	cf(A2,eq(proj(var(30),'o::'),var(31))),
	cf(A2,in_set('PV',var(31))),
	cf(A1,eq(proj(var(32),'o::'),var(33))),
	cf(A1,in_set('PV',var(33)))
	],
	% C-Structure:
	[
	cf(1,subtree(641,'ROOT',638,47)),
	cf(1,phi(641,var(0))),
	cf(1,subtree(638,'ROOT',-,585)),
	cf(1,phi(638,var(0))),
	cf(1,subtree(585,'IPfoc[main,-]',321,582)),
	cf(1,phi(585,var(0))),
	cf(1,subtree(582,'Ibar[main,-]',-,477)),
	cf(1,phi(582,var(0))),
	cf(1,subtree(477,'I[main,-]',-,472)),
	cf(1,phi(477,var(0))),
	cf(1,subtree(472,'V',471,22)),
	cf(1,phi(472,var(0))),
	cf(1,subtree(471,'V',470,24)),
	cf(1,phi(471,var(0))),
	cf(1,subtree(470,'V',469,26)),
	cf(1,phi(470,var(0))),
	cf(1,subtree(469,'V',468,28)),
	cf(1,phi(469,var(0))),
	cf(1,subtree(468,'V',467,30)),
	cf(1,phi(468,var(0))),
	cf(1,subtree(467,'V',465,32)),
	cf(1,phi(467,var(0))),
	cf(1,subtree(465,'V',-,36)),
	cf(1,phi(465,var(0))),
	cf(1,subtree(321,'IPfoc[main,-]',-,311)),
	cf(1,phi(321,var(0))),
	cf(1,subtree(311,'NP',-,308)),
	cf(1,phi(311,var(3))),
	cf(1,subtree(308,'N',307,5)),
	cf(1,phi(308,var(3))),
	cf(1,subtree(307,'N',157,10)),
	cf(1,phi(307,var(3))),
	cf(1,subtree(157,'N',155,13)),
	cf(1,phi(157,var(3))),
	cf(1,subtree(155,'N',153,14)),
	cf(1,phi(155,var(3))),
	cf(1,subtree(153,'N',-,16)),
	cf(1,phi(153,var(3))),
	cf(1,subtree(47,'PERIOD',-,41)),
	cf(1,phi(47,var(0))),
	cf(1,terminal(41,'.',[41])),
	cf(1,phi(41,var(0))),
	cf(A1,terminal(40,'0-*-qePa-2746-3-7',[21])),
	cf(A1,phi(40,var(0))),
	cf(A1,cproj(40,var(32))),
	cf(A1,subtree(36,'V_BASE',-,40)),
	cf(A1,phi(36,var(0))),
	cf(A2,subtree(36,'V_BASE',-,35)),
	cf(A2,phi(36,var(0))),
	cf(A2,terminal(35,'0-*-qePa-2746-10-11',[21])),
	cf(A2,phi(35,var(0))),
	cf(A2,cproj(35,var(30))),
	cf(1,terminal(33,'+V',[21])),
	cf(1,phi(33,var(0))),
	cf(1,subtree(32,'V_SUFF_BASE',-,33)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'+Unerg',[21])),
	cf(1,phi(31,var(0))),
	cf(1,subtree(30,'V_SUFF_BASE',-,31)),
	cf(1,phi(30,var(0))),
	cf(1,terminal(29,'+Base',[21])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(28,'V_SUFF_BASE',-,29)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(27,'+Pres',[21])),
	cf(1,phi(27,var(0))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,terminal(25,'+Subj3Sg',[21])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(23,'+Obj3',[21])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(17,'jaGli',[1])),
	cf(1,phi(17,var(3))),
	cf(1,subtree(16,'N_BASE',-,17)),
	cf(1,phi(16,var(3))),
	cf(1,terminal(15,'+N',[1])),
	cf(1,phi(15,var(3))),
	cf(1,subtree(14,'N_SUFF_BASE',-,15)),
	cf(1,phi(14,var(3))),
	cf(1,subtree(13,'N_SUFF_BASE',-,12)),
	cf(1,phi(13,var(3))),
	cf(1,terminal(12,'+Nom',[1])),
	cf(1,phi(12,var(3))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(3))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(3))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(3))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(3))),
	cf(1,semform_data(0,16,1,5)),
	cf(A1,semform_data(2,36,7,10)),
	cf(A2,semform_data(2,36,7,10)),
	cf(A2,semform_data(6,36,7,10)),
	cf(1,fspan(var(0),1,12)),
	cf(1,fspan(var(3),1,6)),
	cf(1,surfaceform(1,'jaGli',1,6)),
	cf(1,surfaceform(21,'qePs',7,11)),
	cf(1,surfaceform(41,'.',11,12))
	]).

