% -*- coding: iso-8859-1 -*-

fstructure('abramsma brouns sigareti miacoda.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('4'),
	'statistics'('2+2 solutions, 0.52 CPU seconds, 83 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),var(16))),
	cf(1,eq(attr(var(0),'SUBJ'),var(6))),
	cf(1,eq(attr(var(0),'OBJth'),var(3))),
	cf(1,eq(attr(var(0),'OBJ'),var(9))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(11))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(17))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(16),semform('mi-cvda',6,[var(6),var(9),var(3)],[]))),
	cf(A2,eq(var(16),semform('mi-codeba',10,[var(6),var(9),var(3)],[]))),
	cf(1,eq(attr(var(6),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(6),'CHECK'),var(7))),
	cf(1,eq(attr(var(6),'NTYPE'),var(8))),
	cf(1,eq(attr(var(6),'ANIM'),'+')),
	cf(1,eq(attr(var(6),'CASE'),'erg')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(6),'PERS'),'3')),
	cf(1,eq(attr(var(7),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(8),'NSYN'),'proper')),
	cf(1,eq(attr(var(3),'PRED'),semform('Browne',2,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(5))),
	cf(1,eq(attr(var(3),'ANIM'),'+')),
	cf(1,eq(attr(var(3),'CASE'),'dat')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(5),'NSYN'),'proper')),
	cf(1,eq(attr(var(9),'PRED'),semform('sigareti',4,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(10))),
	cf(1,eq(attr(var(9),'NTYPE'),var(12))),
	cf(1,eq(attr(var(9),'CASE'),'nom')),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'_POLARITY'),var(11))),
	cf(1,eq(attr(var(10),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(10),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(12),'NSYN'),'common')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(13))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'aor')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(2))),
	cf(1,in_set(var(6),var(2))),
	cf(1,in_set(var(9),var(2))),
	cf(1,eq(attr(var(13),'_AGR'),var(14))),
	cf(1,eq(attr(var(13),'_LEXID'),var(15))),
	cf(1,eq(attr(var(13),'_CLASS'),'T5')),
	cf(1,eq(attr(var(13),'_PERF-PV'),'mi')),
	cf(1,eq(attr(var(13),'_SYNTAX'),'trans')),
	cf(1,eq(attr(var(14),'_OBJ'),var(3))),
	cf(A1,eq(var(15),'V3268-53')),
	cf(A2,eq(var(15),'V3268-69')),
	cf(1,eq(attr(var(17),'ASPECT'),'perf')),
	cf(1,eq(attr(var(17),'MOOD'),'indicative')),
	cf(1,eq(attr(var(17),'TENSE'),'past')),
	cf(A2,eq(proj(var(40),'o::'),var(41))),
	cf(A2,in_set('OBJ-TH',var(41))),
	cf(A2,in_set('PV',var(41))),
	cf(A1,eq(proj(var(42),'o::'),var(43))),
	cf(A1,in_set('OBJ-TH',var(43))),
	cf(A1,in_set('PV',var(43)))
	],
	% C-Structure:
	[
	cf(1,subtree(1117,'ROOT',1114,51)),
	cf(1,phi(1117,var(0))),
	cf(1,subtree(1114,'ROOT',-,1020)),
	cf(1,phi(1114,var(0))),
	cf(1,subtree(1020,'IP[main,-]',166,919)),
	cf(1,phi(1020,var(0))),
	cf(1,subtree(919,'IP[main,-]',424,913)),
	cf(1,phi(919,var(0))),
	cf(1,subtree(913,'IPfoc[main,-]',610,910)),
	cf(1,phi(913,var(0))),
	cf(1,subtree(910,'Ibar[main,-]',-,776)),
	cf(1,phi(910,var(0))),
	cf(1,subtree(776,'I[main,-]',-,767)),
	cf(1,phi(776,var(0))),
	cf(1,subtree(767,'V',766,26)),
	cf(1,phi(767,var(0))),
	cf(1,subtree(766,'V',765,28)),
	cf(1,phi(766,var(0))),
	cf(1,subtree(765,'V',764,30)),
	cf(1,phi(765,var(0))),
	cf(1,subtree(764,'V',763,32)),
	cf(1,phi(764,var(0))),
	cf(1,subtree(763,'V',762,34)),
	cf(1,phi(763,var(0))),
	cf(1,subtree(762,'V',760,36)),
	cf(1,phi(762,var(0))),
	cf(1,subtree(760,'V',-,40)),
	cf(1,phi(760,var(0))),
	cf(1,subtree(610,'IPfoc[main,-]',-,607)),
	cf(1,phi(610,var(0))),
	cf(1,subtree(607,'NP',-,602)),
	cf(1,phi(607,var(9))),
	cf(1,subtree(602,'N',601,9)),
	cf(1,phi(602,var(9))),
	cf(1,subtree(601,'N',451,14)),
	cf(1,phi(601,var(9))),
	cf(1,subtree(451,'N',449,17)),
	cf(1,phi(451,var(9))),
	cf(1,subtree(449,'N',447,18)),
	cf(1,phi(449,var(9))),
	cf(1,subtree(447,'N',-,20)),
	cf(1,phi(447,var(9))),
	cf(1,subtree(424,'IP[main,-]',-,207)),
	cf(1,phi(424,var(0))),
	cf(1,subtree(207,'PROPP',-,4)),
	cf(1,phi(207,var(3))),
	cf(1,subtree(166,'IP[main,-]',-,141)),
	cf(1,phi(166,var(0))),
	cf(1,subtree(141,'PROPP',-,2)),
	cf(1,phi(141,var(6))),
	cf(1,subtree(51,'PERIOD',-,45)),
	cf(1,phi(51,var(0))),
	cf(1,terminal(45,'.',[45])),
	cf(1,phi(45,var(0))),
	cf(A1,terminal(44,'mi-cvda-3268-53',[25])),
	cf(A1,phi(44,var(0))),
	cf(A1,cproj(44,var(42))),
	cf(A1,subtree(40,'V_BASE',-,44)),
	cf(A1,phi(40,var(0))),
	cf(A2,subtree(40,'V_BASE',-,39)),
	cf(A2,phi(40,var(0))),
	cf(A2,terminal(39,'mi-codeba-3268-69',[25])),
	cf(A2,phi(39,var(0))),
	cf(A2,cproj(39,var(40))),
	cf(1,terminal(37,'+V',[25])),
	cf(1,phi(37,var(0))),
	cf(1,subtree(36,'V_SUFF_BASE',-,37)),
	cf(1,phi(36,var(0))),
	cf(1,terminal(35,'+Trans',[25])),
	cf(1,phi(35,var(0))),
	cf(1,subtree(34,'V_SUFF_BASE',-,35)),
	cf(1,phi(34,var(0))),
	cf(1,terminal(33,'+Base',[25])),
	cf(1,phi(33,var(0))),
	cf(1,subtree(32,'V_SUFF_BASE',-,33)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'+Aor',[25])),
	cf(1,phi(31,var(0))),
	cf(1,subtree(30,'V_SUFF_BASE',-,31)),
	cf(1,phi(30,var(0))),
	cf(1,terminal(29,'+Subj3Sg',[25])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(28,'V_SUFF_BASE',-,29)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(27,'+Obj3',[25])),
	cf(1,phi(27,var(0))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,terminal(21,'sigareti',[5])),
	cf(1,phi(21,var(9))),
	cf(1,subtree(20,'N_BASE',-,21)),
	cf(1,phi(20,var(9))),
	cf(1,terminal(19,'+N',[5])),
	cf(1,phi(19,var(9))),
	cf(1,subtree(18,'N_SUFF_BASE',-,19)),
	cf(1,phi(18,var(9))),
	cf(1,subtree(17,'N_SUFF_BASE',-,16)),
	cf(1,phi(17,var(9))),
	cf(1,terminal(16,'+Nom',[5])),
	cf(1,phi(16,var(9))),
	cf(1,subtree(14,'N_SUFF_BASE',-,11)),
	cf(1,phi(14,var(9))),
	cf(1,terminal(11,'+Sg',[5])),
	cf(1,phi(11,var(9))),
	cf(1,subtree(9,'N_SUFF_BASE',-,7)),
	cf(1,phi(9,var(9))),
	cf(1,terminal(7,'+Full',[5])),
	cf(1,phi(7,var(9))),
	cf(1,subtree(4,'PROP',-,3)),
	cf(1,phi(4,var(3))),
	cf(1,terminal(3,'brouns',[3])),
	cf(1,phi(3,var(3))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(6))),
	cf(1,terminal(1,'abramsma',[1])),
	cf(1,phi(1,var(6))),
	cf(1,semform_data(0,2,1,9)),
	cf(1,semform_data(2,4,10,16)),
	cf(1,semform_data(4,20,17,24)),
	cf(A1,semform_data(6,40,26,32)),
	cf(A2,semform_data(10,40,26,32)),
	cf(1,fspan(var(0),1,34)),
	cf(1,fspan(var(6),1,9)),
	cf(1,fspan(var(3),10,16)),
	cf(1,fspan(var(9),17,25)),
	cf(1,surfaceform(1,'abramsma',1,9)),
	cf(1,surfaceform(3,'brouns',10,16)),
	cf(1,surfaceform(5,'sigareti',17,25)),
	cf(1,surfaceform(25,'miacoda',26,33)),
	cf(1,surfaceform(45,'.',33,34))
	]).

