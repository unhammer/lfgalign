% -*- coding: iso-8859-1 -*-

fstructure('jaGls qePa unda.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('2+2 solutions, 0.05 CPU seconds, 107 subtrees unified'),
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
	select(A2, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('ndoma',9,[var(3),var(6)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'OBJ'),var(6))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(9))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(14))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(3),'PRED'),semform('jaGli',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(5))),
	cf(1,eq(attr(var(3),'CASE'),'dat')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(5),'NSYN'),'common')),
	cf(1,eq(attr(var(6),'PRED'),var(11))),
	cf(A2,eq(attr(var(6),'SUBJ'),var(3))),
	cf(1,eq(attr(var(6),'CHECK'),var(7))),
	cf(A1,eq(attr(var(6),'NTYPE'),var(10))),
	cf(1,eq(attr(var(6),'CASE'),'nom')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(6),'PERS'),'3')),
	cf(A2,eq(attr(var(6),'VFORM'),'masdar')),
	cf(A1,eq(var(11),semform('qePa',7,[],[]))),
	cf(A2,eq(var(11),semform('*-qePa',2,[var(3)],[]))),
	cf(A2,eq(attr(var(7),'_MORPH-SYNT'),var(8))),
	cf(1,eq(attr(var(7),'_POLARITY'),var(9))),
	cf(1,eq(attr(var(7),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(7),'_CASE-TYPE'),'full')),
	cf(A2,eq(attr(var(7),'_TENSE'),'-')),
	cf(A2,eq(attr(var(8),'PARTICIPLE'),'masdar')),
	cf(A2,eq(attr(var(8),'_CLASS'),'MV')),
	cf(A2,eq(attr(var(8),'_LEXID'),'V2746-3-7')),
	cf(A2,eq(attr(var(8),'_PERF-PV'),'*')),
	cf(A1,eq(attr(var(10),'NSYN'),'common')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(12))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'pres')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(A1,in_set(var(6),var(2))),
	cf(1,eq(attr(var(12),'_AGR'),var(13))),
	cf(1,eq(attr(var(12),'_CLASS'),'IV2')),
	cf(1,eq(attr(var(12),'_LEXID'),'V1634-3')),
	cf(1,eq(attr(var(12),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(12),'_SYNTAX'),'inv')),
	cf(1,eq(attr(var(13),'_OBJ'),var(6))),
	cf(1,eq(attr(var(14),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(14),'MOOD'),'indicative')),
	cf(1,eq(attr(var(14),'TENSE'),'pres')),
	cf(1,eq(proj(var(21),'o::'),var(22))),
	cf(1,in_set('NO-PV',var(22)))
	],
	% C-Structure:
	[
	cf(1,subtree(1091,'ROOT',1088,91)),
	cf(1,phi(1091,var(0))),
	cf(1,subtree(1088,'ROOT',-,1014)),
	cf(1,phi(1088,var(0))),
	cf(1,subtree(1014,'IP[main,-]',366,775)),
	cf(1,phi(1014,var(0))),
	cf(1,subtree(990,'V',989,69)),
	cf(1,phi(990,var(0))),
	cf(1,subtree(989,'V',988,71)),
	cf(1,phi(989,var(0))),
	cf(1,subtree(988,'V',987,73)),
	cf(1,phi(988,var(0))),
	cf(1,subtree(987,'V',986,75)),
	cf(1,phi(987,var(0))),
	cf(1,subtree(986,'V',984,77)),
	cf(1,phi(986,var(0))),
	cf(1,subtree(984,'V',-,79)),
	cf(1,phi(984,var(0))),
	cf(A1,subtree(953,'NP',-,947)),
	cf(A1,phi(953,var(6))),
	cf(A1,subtree(947,'N',795,54)),
	cf(A1,phi(947,var(6))),
	cf(A1,subtree(795,'N',793,57)),
	cf(A1,phi(795,var(6))),
	cf(A1,subtree(793,'N',791,58)),
	cf(A1,phi(793,var(6))),
	cf(A1,subtree(791,'N',-,60)),
	cf(A1,phi(791,var(6))),
	cf(1,subtree(775,'IPfoc[main,-]',521,769)),
	cf(1,phi(775,var(0))),
	cf(1,subtree(769,'Ibar[main,-]',-,766)),
	cf(1,phi(769,var(0))),
	cf(1,subtree(766,'I[main,-]',-,66)),
	cf(1,phi(766,var(0))),
	cf(A1,subtree(521,'IPfoc[main,-]',-,953)),
	cf(A1,phi(521,var(0))),
	cf(A2,subtree(521,'IPfoc[main,-]',-,516)),
	cf(A2,phi(521,var(0))),
	cf(A2,subtree(516,'VPmasdar',-,512)),
	cf(A2,phi(516,var(6))),
	cf(A2,subtree(512,'Vmasdar',511,26)),
	cf(A2,phi(512,var(6))),
	cf(A2,subtree(511,'Vmasdar',510,31)),
	cf(A2,phi(511,var(6))),
	cf(A2,subtree(510,'Vmasdar',509,34)),
	cf(A2,phi(510,var(6))),
	cf(A2,subtree(509,'Vmasdar',508,35)),
	cf(A2,phi(509,var(6))),
	cf(A2,subtree(508,'Vmasdar',506,37)),
	cf(A2,phi(508,var(6))),
	cf(A2,subtree(506,'Vmasdar',-,39)),
	cf(A2,phi(506,var(6))),
	cf(1,subtree(366,'IP[main,-]',-,355)),
	cf(1,phi(366,var(0))),
	cf(1,subtree(355,'NP',-,352)),
	cf(1,phi(355,var(3))),
	cf(1,subtree(352,'N',351,5)),
	cf(1,phi(352,var(3))),
	cf(1,subtree(351,'N',201,10)),
	cf(1,phi(351,var(3))),
	cf(1,subtree(201,'N',199,14)),
	cf(1,phi(201,var(3))),
	cf(1,subtree(199,'N',197,15)),
	cf(1,phi(199,var(3))),
	cf(1,subtree(197,'N',-,17)),
	cf(1,phi(197,var(3))),
	cf(1,subtree(91,'PERIOD',-,85)),
	cf(1,phi(91,var(0))),
	cf(1,terminal(85,'.',[85])),
	cf(1,phi(85,var(0))),
	cf(1,terminal(80,'ndoma-1634-3',[65])),
	cf(1,phi(80,var(23))),
	cf(1,cproj(80,var(21))),
	cf(1,subtree(79,'V_BASE',-,80)),
	cf(1,phi(79,var(0))),
	cf(1,terminal(78,'+V',[65])),
	cf(1,phi(78,var(0))),
	cf(1,subtree(77,'V_SUFF_BASE',-,78)),
	cf(1,phi(77,var(0))),
	cf(1,terminal(76,'+Inv',[65])),
	cf(1,phi(76,var(0))),
	cf(1,subtree(75,'V_SUFF_BASE',-,76)),
	cf(1,phi(75,var(0))),
	cf(1,terminal(74,'+Base',[65])),
	cf(1,phi(74,var(0))),
	cf(1,subtree(73,'V_SUFF_BASE',-,74)),
	cf(1,phi(73,var(0))),
	cf(1,terminal(72,'+Pres',[65])),
	cf(1,phi(72,var(0))),
	cf(1,subtree(71,'V_SUFF_BASE',-,72)),
	cf(1,phi(71,var(0))),
	cf(1,terminal(70,'+Obj3',[65])),
	cf(1,phi(70,var(0))),
	cf(1,subtree(69,'V_SUFF_BASE',-,70)),
	cf(1,phi(69,var(0))),
	cf(1,terminal(68,'+Subj3Sg',[65])),
	cf(1,phi(68,var(0))),
	cf(1,subtree(67,'V_SUFF_BASE',-,68)),
	cf(1,phi(67,var(0))),
	cf(1,subtree(66,'V',990,67)),
	cf(1,phi(66,var(0))),
	cf(A1,terminal(61,'qePa',[22])),
	cf(A1,phi(61,var(6))),
	cf(A1,subtree(60,'N_BASE',-,61)),
	cf(A1,phi(60,var(6))),
	cf(A1,terminal(59,'+N',[22])),
	cf(A1,phi(59,var(6))),
	cf(A1,subtree(58,'N_SUFF_BASE',-,59)),
	cf(A1,phi(58,var(6))),
	cf(A1,subtree(57,'N_SUFF_BASE',-,56)),
	cf(A1,phi(57,var(6))),
	cf(A1,terminal(56,'+Nom',[22])),
	cf(A1,phi(56,var(6))),
	cf(A1,subtree(54,'N_SUFF_BASE',-,51)),
	cf(A1,phi(54,var(6))),
	cf(A1,terminal(51,'+Sg',[22])),
	cf(A1,phi(51,var(6))),
	cf(A2,terminal(49,'0-*-qePa-2746-3-7',[22])),
	cf(A2,phi(49,var(6))),
	cf(A2,subtree(39,'Vpart_BASE',-,49)),
	cf(A2,phi(39,var(6))),
	cf(A2,terminal(38,'+VPart',[22])),
	cf(A2,phi(38,var(6))),
	cf(A2,subtree(37,'Vpart_SUFF_BASE',-,38)),
	cf(A2,phi(37,var(6))),
	cf(A2,terminal(36,'+Masdar',[22])),
	cf(A2,phi(36,var(6))),
	cf(A2,subtree(35,'Vmasdar_SUFF_BASE',-,36)),
	cf(A2,phi(35,var(6))),
	cf(A2,subtree(34,'N_SUFF_BASE',-,33)),
	cf(A2,phi(34,var(6))),
	cf(A2,terminal(33,'+Nom',[22])),
	cf(A2,phi(33,var(6))),
	cf(A2,subtree(31,'N_SUFF_BASE',-,28)),
	cf(A2,phi(31,var(6))),
	cf(A2,terminal(28,'+Sg',[22])),
	cf(A2,phi(28,var(6))),
	cf(A2,subtree(26,'N_SUFF_BASE',-,24)),
	cf(A2,phi(26,var(6))),
	cf(A2,terminal(24,'+Full',[22])),
	cf(A2,phi(24,var(6))),
	cf(1,terminal(18,'jaGli',[1])),
	cf(1,phi(18,var(3))),
	cf(1,subtree(17,'N_BASE',-,18)),
	cf(1,phi(17,var(3))),
	cf(1,terminal(16,'+N',[1])),
	cf(1,phi(16,var(3))),
	cf(1,subtree(15,'N_SUFF_BASE',-,16)),
	cf(1,phi(15,var(3))),
	cf(1,subtree(14,'N_SUFF_BASE',-,12)),
	cf(1,phi(14,var(3))),
	cf(1,terminal(12,'+Dat',[1])),
	cf(1,phi(12,var(3))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(3))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(3))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(3))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(3))),
	cf(1,semform_data(0,17,1,5)),
	cf(A2,semform_data(2,39,7,10)),
	cf(A1,semform_data(7,60,7,10)),
	cf(1,semform_data(9,79,12,15)),
	cf(1,fspan(var(0),1,17)),
	cf(1,fspan(var(3),1,6)),
	cf(A2,fspan(var(6),7,11)),
	cf(A1,fspan(var(6),7,11)),
	cf(1,surfaceform(1,'jaGls',1,6)),
	cf(1,surfaceform(22,'qePa',7,11)),
	cf(1,surfaceform(65,'unda',12,16)),
	cf(1,surfaceform(85,'.',16,17))
	]).
