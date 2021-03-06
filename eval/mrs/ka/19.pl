% -*- coding: iso-8859-1 -*-

fstructure('TambaKo movida.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('2'),
	'statistics'('1+1 solutions, 0.11 CPU seconds, 49 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('mo-svla',2,[var(3)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(10))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(3),'PRED'),semform('TambaKo',0,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(7))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'aor')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(7),'_AGR'),var(8))),
	cf(1,eq(attr(var(7),'_CLASS'),'MV')),
	cf(1,eq(attr(var(7),'_LEXID'),'V2032-3')),
	cf(1,eq(attr(var(7),'_PERF-PV'),'mo')),
	cf(1,eq(attr(var(7),'_SYNTAX'),'unacc')),
	cf(1,eq(attr(var(8),'_OBJ'),var(9))),
	cf(1,eq(attr(var(9),'NUM'),'sg')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'ASPECT'),'perf')),
	cf(1,eq(attr(var(10),'MOOD'),'indicative')),
	cf(1,eq(attr(var(10),'TENSE'),'past')),
	cf(1,eq(proj(var(21),'o::'),var(22))),
	cf(1,in_set('PV',var(22)))
	],
	% C-Structure:
	[
	cf(1,subtree(635,'ROOT',632,42)),
	cf(1,phi(635,var(0))),
	cf(1,subtree(632,'ROOT',-,579)),
	cf(1,phi(632,var(0))),
	cf(1,subtree(579,'IPfoc[main,-]',315,576)),
	cf(1,phi(579,var(0))),
	cf(1,subtree(576,'Ibar[main,-]',-,471)),
	cf(1,phi(576,var(0))),
	cf(1,subtree(471,'I[main,-]',-,466)),
	cf(1,phi(471,var(0))),
	cf(1,subtree(466,'V',465,18)),
	cf(1,phi(466,var(0))),
	cf(1,subtree(465,'V',464,20)),
	cf(1,phi(465,var(0))),
	cf(1,subtree(464,'V',463,22)),
	cf(1,phi(464,var(0))),
	cf(1,subtree(463,'V',462,24)),
	cf(1,phi(463,var(0))),
	cf(1,subtree(462,'V',461,26)),
	cf(1,phi(462,var(0))),
	cf(1,subtree(461,'V',459,28)),
	cf(1,phi(461,var(0))),
	cf(1,subtree(459,'V',-,32)),
	cf(1,phi(459,var(0))),
	cf(1,subtree(315,'IPfoc[main,-]',-,305)),
	cf(1,phi(315,var(0))),
	cf(1,subtree(305,'NP',-,302)),
	cf(1,phi(305,var(3))),
	cf(1,subtree(302,'N',152,6)),
	cf(1,phi(302,var(3))),
	cf(1,subtree(152,'N',150,9)),
	cf(1,phi(152,var(3))),
	cf(1,subtree(150,'N',148,10)),
	cf(1,phi(150,var(3))),
	cf(1,subtree(148,'N',-,12)),
	cf(1,phi(148,var(3))),
	cf(1,subtree(42,'PERIOD',-,36)),
	cf(1,phi(42,var(0))),
	cf(1,terminal(36,'.',[36])),
	cf(1,phi(36,var(0))),
	cf(1,subtree(32,'V_BASE',-,31)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'mo-svla-2032-3',[17])),
	cf(1,phi(31,var(23))),
	cf(1,cproj(31,var(21))),
	cf(1,terminal(29,'+V',[17])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(28,'V_SUFF_BASE',-,29)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(27,'+Unacc',[17])),
	cf(1,phi(27,var(0))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,terminal(25,'+Base',[17])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(23,'+Aor',[17])),
	cf(1,phi(23,var(0))),
	cf(1,subtree(22,'V_SUFF_BASE',-,23)),
	cf(1,phi(22,var(0))),
	cf(1,terminal(21,'+Subj3Sg',[17])),
	cf(1,phi(21,var(0))),
	cf(1,subtree(20,'V_SUFF_BASE',-,21)),
	cf(1,phi(20,var(0))),
	cf(1,terminal(19,'+Obj3',[17])),
	cf(1,phi(19,var(0))),
	cf(1,subtree(18,'V_SUFF_BASE',-,19)),
	cf(1,phi(18,var(0))),
	cf(1,terminal(13,'TambaKo',[1])),
	cf(1,phi(13,var(3))),
	cf(1,subtree(12,'N_BASE',-,13)),
	cf(1,phi(12,var(3))),
	cf(1,terminal(11,'+N',[1])),
	cf(1,phi(11,var(3))),
	cf(1,subtree(10,'N_SUFF_BASE',-,11)),
	cf(1,phi(10,var(3))),
	cf(1,subtree(9,'N_SUFF_BASE',-,8)),
	cf(1,phi(9,var(3))),
	cf(1,terminal(8,'+Nom',[1])),
	cf(1,phi(8,var(3))),
	cf(1,subtree(6,'N_SUFF_BASE',-,3)),
	cf(1,phi(6,var(3))),
	cf(1,terminal(3,'+Sg',[1])),
	cf(1,phi(3,var(3))),
	cf(1,semform_data(0,12,1,7)),
	cf(1,semform_data(2,32,9,14)),
	cf(1,fspan(var(0),1,16)),
	cf(1,fspan(var(3),1,8)),
	cf(1,surfaceform(1,'TambaKo',1,8)),
	cf(1,surfaceform(17,'movida',9,15)),
	cf(1,surfaceform(36,'.',15,16))
	]).

