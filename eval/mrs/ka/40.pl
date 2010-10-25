% -*- coding: iso-8859-1 -*-

fstructure('jaGli ver qePda.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('2+2 solutions, 0.03 CPU seconds, 59 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),var(16))),
	cf(1,eq(attr(var(0),'SUBJ'),var(6))),
	cf(A2,eq(attr(var(0),'OBJth'),var(14))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(4))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(17))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'NEG'),'+')),
	cf(1,eq(attr(var(0),'POLARITY'),'pot-neg')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(16),semform('*-qePa',3,[var(6)],[]))),
	cf(A2,eq(var(16),semform('*-qePa',3,[var(6),var(14)],[]))),
	cf(1,eq(attr(var(6),'PRED'),semform('jaGli',0,[],[]))),
	cf(1,eq(attr(var(6),'CHECK'),var(7))),
	cf(1,eq(attr(var(6),'NTYPE'),var(8))),
	cf(1,eq(attr(var(6),'CASE'),'nom')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(6),'PERS'),'3')),
	cf(1,eq(attr(var(7),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(7),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(7),'_POLARITY'),'pot-neg')),
	cf(1,eq(attr(var(8),'NSYN'),'common')),
	cf(A2,eq(attr(var(14),'PRED'),semform('pro',7,[],[]))),
	cf(A2,eq(attr(var(14),'CHECK'),var(15))),
	cf(A2,eq(attr(var(14),'CASE'),'dat')),
	cf(A2,eq(attr(var(14),'PERS'),'3')),
	cf(A2,eq(attr(var(15),'_PRODROP'),'+')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('ver',2,[],[]))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(2),'ADV-TYPE'),'pot-neg')),
	cf(1,eq(attr(var(3),'_PURE-NEG'),'+')),
	cf(1,eq(attr(var(4),'_IN-SITU'),var(5))),
	cf(1,eq(attr(var(4),'_MORPH-SYNT'),var(9))),
	cf(1,eq(attr(var(4),'_AGR'),'both')),
	cf(1,eq(attr(var(4),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(4),'_PERIOD'),'+')),
	cf(1,eq(attr(var(4),'_TENSE'),'impf')),
	cf(1,eq(attr(var(4),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(6),var(5))),
	cf(1,eq(attr(var(9),'_AGR'),var(10))),
	cf(1,eq(attr(var(9),'_CLASS'),var(12))),
	cf(1,eq(attr(var(9),'_LEXID'),var(13))),
	cf(1,eq(attr(var(9),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(9),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(10),'_OBJ'),var(11))),
	cf(A2,eq(var(11),var(14))),
	cf(A1,eq(attr(var(11),'NUM'),'sg')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(A1,eq(var(12),'MV')),
	cf(A2,eq(var(12),'RM1')),
	cf(A1,eq(var(13),'V2746-3-7')),
	cf(A2,eq(var(13),'V2746-10-11')),
	cf(1,eq(attr(var(17),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(17),'MOOD'),'indicative')),
	cf(1,eq(attr(var(17),'TENSE'),'past')),
	cf(A2,eq(proj(var(34),'o::'),var(35))),
	cf(A2,in_set('PV',var(35))),
	cf(A1,eq(proj(var(36),'o::'),var(37))),
	cf(A1,in_set('PV',var(37)))
	],
	% C-Structure:
	[
	cf(1,subtree(808,'ROOT',805,55)),
	cf(1,phi(808,var(0))),
	cf(1,subtree(805,'ROOT',-,786)),
	cf(1,phi(805,var(0))),
	cf(1,subtree(786,'IPfoc[main,-]',329,784)),
	cf(1,phi(786,var(0))),
	cf(1,subtree(784,'Ibar[main,-]',-,776)),
	cf(1,phi(784,var(0))),
	cf(1,subtree(776,'I[main,-]',-,604)),
	cf(1,phi(776,var(0))),
	cf(1,subtree(604,'Vneg',414,602)),
	cf(1,phi(604,var(0))),
	cf(1,subtree(602,'V',601,30)),
	cf(1,phi(602,var(0))),
	cf(1,subtree(601,'V',600,32)),
	cf(1,phi(601,var(0))),
	cf(1,subtree(600,'V',599,34)),
	cf(1,phi(600,var(0))),
	cf(1,subtree(599,'V',598,36)),
	cf(1,phi(599,var(0))),
	cf(1,subtree(598,'V',597,38)),
	cf(1,phi(598,var(0))),
	cf(1,subtree(597,'V',595,40)),
	cf(1,phi(597,var(0))),
	cf(1,subtree(595,'V',-,44)),
	cf(1,phi(595,var(0))),
	cf(1,subtree(414,'Vneg',-,28)),
	cf(1,phi(414,var(0))),
	cf(1,subtree(329,'IPfoc[main,-]',-,319)),
	cf(1,phi(329,var(0))),
	cf(1,subtree(319,'NP',-,316)),
	cf(1,phi(319,var(6))),
	cf(1,subtree(316,'N',315,5)),
	cf(1,phi(316,var(6))),
	cf(1,subtree(315,'N',165,10)),
	cf(1,phi(315,var(6))),
	cf(1,subtree(165,'N',163,13)),
	cf(1,phi(165,var(6))),
	cf(1,subtree(163,'N',161,14)),
	cf(1,phi(163,var(6))),
	cf(1,subtree(161,'N',-,16)),
	cf(1,phi(161,var(6))),
	cf(1,subtree(55,'PERIOD',-,49)),
	cf(1,phi(55,var(0))),
	cf(1,terminal(49,'.',[49])),
	cf(1,phi(49,var(0))),
	cf(A1,terminal(48,'0-*-qePa-2746-3-7',[29])),
	cf(A1,phi(48,var(0))),
	cf(A1,cproj(48,var(36))),
	cf(A1,subtree(44,'V_BASE',-,48)),
	cf(A1,phi(44,var(0))),
	cf(A2,subtree(44,'V_BASE',-,43)),
	cf(A2,phi(44,var(0))),
	cf(A2,terminal(43,'0-*-qePa-2746-10-11',[29])),
	cf(A2,phi(43,var(0))),
	cf(A2,cproj(43,var(34))),
	cf(1,terminal(41,'+V',[29])),
	cf(1,phi(41,var(0))),
	cf(1,subtree(40,'V_SUFF_BASE',-,41)),
	cf(1,phi(40,var(0))),
	cf(1,terminal(39,'+Unerg',[29])),
	cf(1,phi(39,var(0))),
	cf(1,subtree(38,'V_SUFF_BASE',-,39)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(37,'+Base',[29])),
	cf(1,phi(37,var(0))),
	cf(1,subtree(36,'V_SUFF_BASE',-,37)),
	cf(1,phi(36,var(0))),
	cf(1,terminal(35,'+Impf',[29])),
	cf(1,phi(35,var(0))),
	cf(1,subtree(34,'V_SUFF_BASE',-,35)),
	cf(1,phi(34,var(0))),
	cf(1,terminal(33,'+Subj3Sg',[29])),
	cf(1,phi(33,var(0))),
	cf(1,subtree(32,'V_SUFF_BASE',-,33)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'+Obj3',[29])),
	cf(1,phi(31,var(0))),
	cf(1,subtree(30,'V_SUFF_BASE',-,31)),
	cf(1,phi(30,var(0))),
	cf(1,subtree(28,'ADVneg',-,21)),
	cf(1,phi(28,var(2))),
	cf(1,terminal(21,'ver',[21])),
	cf(1,phi(21,var(2))),
	cf(1,terminal(17,'jaGli',[1])),
	cf(1,phi(17,var(6))),
	cf(1,subtree(16,'N_BASE',-,17)),
	cf(1,phi(16,var(6))),
	cf(1,terminal(15,'+N',[1])),
	cf(1,phi(15,var(6))),
	cf(1,subtree(14,'N_SUFF_BASE',-,15)),
	cf(1,phi(14,var(6))),
	cf(1,subtree(13,'N_SUFF_BASE',-,12)),
	cf(1,phi(13,var(6))),
	cf(1,terminal(12,'+Nom',[1])),
	cf(1,phi(12,var(6))),
	cf(1,subtree(10,'N_SUFF_BASE',-,7)),
	cf(1,phi(10,var(6))),
	cf(1,terminal(7,'+Sg',[1])),
	cf(1,phi(7,var(6))),
	cf(1,subtree(5,'N_SUFF_BASE',-,3)),
	cf(1,phi(5,var(6))),
	cf(1,terminal(3,'+Full',[1])),
	cf(1,phi(3,var(6))),
	cf(1,semform_data(0,16,1,5)),
	cf(1,semform_data(2,28,7,10)),
	cf(A1,semform_data(3,44,11,15)),
	cf(A2,semform_data(3,44,11,15)),
	cf(A2,semform_data(7,44,11,15)),
	cf(1,fspan(var(0),1,17)),
	cf(1,fspan(var(6),1,6)),
	cf(1,fspan(var(2),7,10)),
	cf(1,surfaceform(1,'jaGli',1,6)),
	cf(1,surfaceform(21,'ver',7,10)),
	cf(1,surfaceform(29,'qePda',11,16)),
	cf(1,surfaceform(49,'.',16,17))
	]).
