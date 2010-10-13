% -*- coding: iso-8859-1 -*-

fstructure('soPim klevervegenisken SeuHvia.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('1+1 solutions, 0.19 CPU seconds, 68 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('Se-Hveva',4,[var(10),var(17),var(15)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(10))),
	cf(1,eq(attr(var(0),'OBJ'),var(17))),
	cf(1,eq(attr(var(0),'OBJben'),var(15))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(8))),
	cf(1,eq(attr(var(0),'POLARITY'),var(4))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(19))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(10),'PRED'),semform('soPi',0,[],[]))),
	cf(1,eq(attr(var(10),'CHECK'),var(11))),
	cf(1,eq(attr(var(10),'NTYPE'),var(12))),
	cf(1,eq(attr(var(10),'ANIM'),'+')),
	cf(1,eq(attr(var(10),'CASE'),'erg')),
	cf(1,eq(attr(var(10),'GEND'),'fem')),
	cf(1,eq(attr(var(10),'NUM'),'sg')),
	cf(1,eq(attr(var(10),'PERS'),'3')),
	cf(1,eq(attr(var(11),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(12),'NSYN'),'proper')),
	cf(1,eq(attr(var(17),'PRED'),semform('pro',6,[],[]))),
	cf(1,eq(attr(var(17),'CHECK'),var(18))),
	cf(1,eq(attr(var(17),'CASE'),'nom')),
	cf(1,eq(attr(var(18),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(18),'_PRODROP'),'+')),
	cf(1,eq(attr(var(15),'PRED'),semform('pro',7,[],[]))),
	cf(1,eq(attr(var(15),'CHECK'),var(16))),
	cf(1,eq(attr(var(15),'CASE'),'dat')),
	cf(1,eq(attr(var(15),'PERS'),'3')),
	cf(1,eq(attr(var(16),'_PRODROP'),'+')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('-ken',3,[var(5)],[]))),
	cf(1,eq(attr(var(2),'OBJ'),var(5))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(5),'PRED'),semform('klevervegeni',2,[],[]))),
	cf(1,eq(attr(var(5),'CHECK'),var(6))),
	cf(1,eq(attr(var(5),'NTYPE'),var(7))),
	cf(1,eq(attr(var(5),'CASE'),'gen')),
	cf(1,eq(attr(var(5),'NUM'),'sg')),
	cf(1,eq(attr(var(5),'PERS'),'3')),
	cf(1,eq(attr(var(6),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'_POSTP'),'-ken')),
	cf(1,eq(attr(var(7),'NSYN'),'common')),
	cf(1,eq(attr(var(3),'_POLARITY'),var(4))),
	cf(1,eq(attr(var(3),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(3),'_POSTP'),'-ken')),
	cf(1,eq(attr(var(8),'_IN-SITU'),var(9))),
	cf(1,eq(attr(var(8),'_MORPH-SYNT'),var(13))),
	cf(1,eq(attr(var(8),'_AGR'),'both')),
	cf(1,eq(attr(var(8),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(8),'_PERIOD'),'+')),
	cf(1,eq(attr(var(8),'_TENSE'),'aor')),
	cf(1,eq(attr(var(8),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(10),var(9))),
	cf(1,eq(attr(var(13),'_AGR'),var(14))),
	cf(1,eq(attr(var(13),'_CLASS'),'T3')),
	cf(1,eq(attr(var(13),'_LEXID'),'V3614-35')),
	cf(1,eq(attr(var(13),'_PERF-PV'),'Se')),
	cf(1,eq(attr(var(13),'_SYNTAX'),'trans')),
	cf(1,eq(attr(var(14),'_OBJ'),var(15))),
	cf(1,eq(attr(var(19),'ASPECT'),'perf')),
	cf(1,eq(attr(var(19),'MOOD'),'indicative')),
	cf(1,eq(attr(var(19),'TENSE'),'past')),
	cf(1,eq(proj(var(32),'o::'),var(33))),
	cf(1,in_set('OBJ-TH',var(33))),
	cf(1,in_set('PV',var(33)))
	],
	% C-Structure:
	[
	cf(1,subtree(889,'ROOT',886,53)),
	cf(1,phi(889,var(0))),
	cf(1,subtree(886,'ROOT',-,811)),
	cf(1,phi(886,var(0))),
	cf(1,subtree(811,'IP[main,-]',186,751)),
	cf(1,phi(811,var(0))),
	cf(1,subtree(751,'IPfoc[main,-]',488,749)),
	cf(1,phi(751,var(0))),
	cf(1,subtree(749,'Ibar[main,-]',-,646)),
	cf(1,phi(749,var(0))),
	cf(1,subtree(646,'I[main,-]',-,643)),
	cf(1,phi(646,var(0))),
	cf(1,subtree(643,'V',642,29)),
	cf(1,phi(643,var(0))),
	cf(1,subtree(642,'V',641,31)),
	cf(1,phi(642,var(0))),
	cf(1,subtree(641,'V',640,33)),
	cf(1,phi(641,var(0))),
	cf(1,subtree(640,'V',639,35)),
	cf(1,phi(640,var(0))),
	cf(1,subtree(639,'V',638,37)),
	cf(1,phi(639,var(0))),
	cf(1,subtree(638,'V',636,39)),
	cf(1,phi(638,var(0))),
	cf(1,subtree(636,'V',-,43)),
	cf(1,phi(636,var(0))),
	cf(1,subtree(614,'PP',-,483)),
	cf(1,phi(614,var(2))),
	cf(1,subtree(488,'IPfoc[main,-]',-,614)),
	cf(1,phi(488,var(0))),
	cf(1,subtree(483,'NP',-,478)),
	cf(1,phi(483,var(5))),
	cf(1,subtree(478,'N',477,6)),
	cf(1,phi(478,var(5))),
	cf(1,subtree(477,'N',476,10)),
	cf(1,phi(477,var(5))),
	cf(1,subtree(476,'N',326,15)),
	cf(1,phi(476,var(5))),
	cf(1,subtree(326,'N',324,19)),
	cf(1,phi(326,var(5))),
	cf(1,subtree(324,'N',322,20)),
	cf(1,phi(324,var(5))),
	cf(1,subtree(322,'N',-,22)),
	cf(1,phi(322,var(5))),
	cf(1,subtree(186,'IP[main,-]',-,162)),
	cf(1,phi(186,var(0))),
	cf(1,subtree(162,'PROPP',-,2)),
	cf(1,phi(162,var(10))),
	cf(1,subtree(53,'PERIOD',-,47)),
	cf(1,phi(53,var(0))),
	cf(1,terminal(47,'.',[47])),
	cf(1,phi(47,var(0))),
	cf(1,subtree(43,'V_BASE',-,42)),
	cf(1,phi(43,var(0))),
	cf(1,terminal(42,'Se-Hveva-3614-35',[27])),
	cf(1,phi(42,var(34))),
	cf(1,cproj(42,var(32))),
	cf(1,terminal(40,'+V',[27])),
	cf(1,phi(40,var(0))),
	cf(1,subtree(39,'V_SUFF_BASE',-,40)),
	cf(1,phi(39,var(0))),
	cf(1,terminal(38,'+Trans',[27])),
	cf(1,phi(38,var(0))),
	cf(1,subtree(37,'V_SUFF_BASE',-,38)),
	cf(1,phi(37,var(0))),
	cf(1,terminal(36,'+Base',[27])),
	cf(1,phi(36,var(0))),
	cf(1,subtree(35,'V_SUFF_BASE',-,36)),
	cf(1,phi(35,var(0))),
	cf(1,terminal(34,'+Aor',[27])),
	cf(1,phi(34,var(0))),
	cf(1,subtree(33,'V_SUFF_BASE',-,34)),
	cf(1,phi(33,var(0))),
	cf(1,terminal(32,'+Subj3Sg',[27])),
	cf(1,phi(32,var(0))),
	cf(1,subtree(31,'V_SUFF_BASE',-,32)),
	cf(1,phi(31,var(0))),
	cf(1,terminal(30,'+Obj3',[27])),
	cf(1,phi(30,var(0))),
	cf(1,subtree(29,'V_SUFF_BASE',-,30)),
	cf(1,phi(29,var(0))),
	cf(1,terminal(23,'klevervegeni',[3])),
	cf(1,phi(23,var(5))),
	cf(1,subtree(22,'N_BASE',-,23)),
	cf(1,phi(22,var(5))),
	cf(1,terminal(21,'+N',[3])),
	cf(1,phi(21,var(5))),
	cf(1,subtree(20,'N_SUFF_BASE',-,21)),
	cf(1,phi(20,var(5))),
	cf(1,subtree(19,'N_SUFF_BASE',-,17)),
	cf(1,phi(19,var(5))),
	cf(1,terminal(17,'+Gen',[3])),
	cf(1,phi(17,var(5))),
	cf(1,subtree(15,'N_SUFF_BASE',-,12)),
	cf(1,phi(15,var(5))),
	cf(1,terminal(12,'+Sg',[3])),
	cf(1,phi(12,var(5))),
	cf(1,subtree(10,'N_SUFF_BASE',-,8)),
	cf(1,phi(10,var(5))),
	cf(1,terminal(8,'+Full',[3])),
	cf(1,phi(8,var(5))),
	cf(1,subtree(6,'N_SUFF_BASE',-,5)),
	cf(1,phi(6,var(5))),
	cf(1,terminal(5,'+Ken',[3])),
	cf(1,phi(5,var(5))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(10))),
	cf(1,terminal(1,'soPim',[1])),
	cf(1,phi(1,var(10))),
	cf(1,semform_data(0,2,1,6)),
	cf(1,semform_data(2,22,7,18)),
	cf(1,semform_data(3,614,7,23)),
	cf(1,semform_data(4,43,24,30)),
	cf(1,semform_data(6,43,24,30)),
	cf(1,semform_data(7,43,24,30)),
	cf(1,fspan(var(0),1,32)),
	cf(1,fspan(var(10),1,6)),
	cf(1,fspan(var(2),7,23)),
	cf(1,fspan(var(5),7,23)),
	cf(1,surfaceform(1,'soPim',1,6)),
	cf(1,surfaceform(3,'klevervegenisken',7,23)),
	cf(1,surfaceform(27,'SeuHvia',24,31)),
	cf(1,surfaceform(47,'.',31,32))
	]).

