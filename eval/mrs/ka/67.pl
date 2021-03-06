% -*- coding: iso-8859-1 -*-

fstructure('abramsis suraTi movida.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('1+1 solutions, 0.02 CPU seconds, 91 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('mo-svla',5,[var(3)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(14))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(3),'PRED'),semform('suraTi',3,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'SPEC'),var(7))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(1,eq(attr(var(7),'POSS'),var(8))),
	cf(1,eq(attr(var(8),'PRED'),semform('Abrams',0,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(9))),
	cf(1,eq(attr(var(8),'NTYPE'),var(10))),
	cf(1,eq(attr(var(8),'ANIM'),'+')),
	cf(1,eq(attr(var(8),'CASE'),'gen')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'PERS'),'3')),
	cf(1,eq(attr(var(9),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(10),'NSYN'),'proper')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(11))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'aor')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(11),'_AGR'),var(12))),
	cf(1,eq(attr(var(11),'_CLASS'),'MV')),
	cf(1,eq(attr(var(11),'_LEXID'),'V2032-3')),
	cf(1,eq(attr(var(11),'_PERF-PV'),'mo')),
	cf(1,eq(attr(var(11),'_SYNTAX'),'unacc')),
	cf(1,eq(attr(var(12),'_OBJ'),var(13))),
	cf(1,eq(attr(var(13),'NUM'),'sg')),
	cf(1,eq(attr(var(13),'PERS'),'3')),
	cf(1,eq(attr(var(14),'ASPECT'),'perf')),
	cf(1,eq(attr(var(14),'MOOD'),'indicative')),
	cf(1,eq(attr(var(14),'TENSE'),'past')),
	cf(1,eq(proj(var(25),'o::'),var(26))),
	cf(1,in_set('PV',var(26)))
	],
	% C-Structure:
	[
	cf(1,subtree(864,'ROOT',861,48)),
	cf(1,phi(864,var(0))),
	cf(1,subtree(885,'POSSP',158,458)),
	cf(1,phi(885,var(3))),
	cf(1,subtree(861,'ROOT',-,811)),
	cf(1,phi(861,var(0))),
	cf(1,subtree(811,'IPfoc[main,-]',791,724)),
	cf(1,phi(811,var(0))),
	cf(1,subtree(791,'IPfoc[main,-]',-,885)),
	cf(1,phi(791,var(0))),
	cf(1,subtree(724,'Ibar[main,-]',-,621)),
	cf(1,phi(724,var(0))),
	cf(1,subtree(621,'I[main,-]',-,618)),
	cf(1,phi(621,var(0))),
	cf(1,subtree(618,'V',617,24)),
	cf(1,phi(618,var(0))),
	cf(1,subtree(617,'V',616,26)),
	cf(1,phi(617,var(0))),
	cf(1,subtree(616,'V',615,28)),
	cf(1,phi(616,var(0))),
	cf(1,subtree(615,'V',614,30)),
	cf(1,phi(615,var(0))),
	cf(1,subtree(614,'V',613,32)),
	cf(1,phi(614,var(0))),
	cf(1,subtree(613,'V',611,34)),
	cf(1,phi(613,var(0))),
	cf(1,subtree(611,'V',-,38)),
	cf(1,phi(611,var(0))),
	cf(1,subtree(458,'NP',-,453)),
	cf(1,phi(458,var(3))),
	cf(1,subtree(453,'N',452,7)),
	cf(1,phi(453,var(3))),
	cf(1,subtree(452,'N',302,12)),
	cf(1,phi(452,var(3))),
	cf(1,subtree(302,'N',300,15)),
	cf(1,phi(302,var(3))),
	cf(1,subtree(300,'N',298,16)),
	cf(1,phi(300,var(3))),
	cf(1,subtree(298,'N',-,18)),
	cf(1,phi(298,var(3))),
	cf(1,subtree(158,'POSSP',-,138)),
	cf(1,phi(158,var(3))),
	cf(1,subtree(138,'PROPP',-,2)),
	cf(1,phi(138,var(8))),
	cf(1,subtree(48,'PERIOD',-,42)),
	cf(1,phi(48,var(0))),
	cf(1,terminal(42,'.',[42])),
	cf(1,phi(42,var(0))),
	cf(1,subtree(38,'V_BASE',-,37)),
	cf(1,phi(38,var(0))),
	cf(1,terminal(37,'mo-svla-2032-3',[23])),
	cf(1,phi(37,var(27))),
	cf(1,cproj(37,var(25))),
	cf(1,terminal(35,'+V',[23])),
	cf(1,phi(35,var(0))),
	cf(1,subtree(34,'V_SUFF_BASE',-,35)),
	cf(1,phi(34,var(0))),
	cf(1,terminal(33,'+Unacc',[23])),
	cf(1,phi(33,var(0))),
	cf(1,subtree(32,'V_SUFF_BASE',-,33)),
	cf(1,phi(32,var(0))),
	cf(1,terminal(31,'+Base',[23])),
	cf(1,phi(31,var(0))),
	cf(1,subtree(30,'V_SUFF_BASE',-,31)),
	cf(1,phi(30,var(0))),
	cf(1,terminal(29,'+Aor',[23])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(28,'V_SUFF_BASE',-,29)),
	cf(1,phi(28,var(0))),
	cf(1,terminal(27,'+Subj3Sg',[23])),
	cf(1,phi(27,var(0))),
	cf(1,subtree(26,'V_SUFF_BASE',-,27)),
	cf(1,phi(26,var(0))),
	cf(1,terminal(25,'+Obj3',[23])),
	cf(1,phi(25,var(0))),
	cf(1,subtree(24,'V_SUFF_BASE',-,25)),
	cf(1,phi(24,var(0))),
	cf(1,terminal(19,'suraTi',[3])),
	cf(1,phi(19,var(3))),
	cf(1,subtree(18,'N_BASE',-,19)),
	cf(1,phi(18,var(3))),
	cf(1,terminal(17,'+N',[3])),
	cf(1,phi(17,var(3))),
	cf(1,subtree(16,'N_SUFF_BASE',-,17)),
	cf(1,phi(16,var(3))),
	cf(1,subtree(15,'N_SUFF_BASE',-,14)),
	cf(1,phi(15,var(3))),
	cf(1,terminal(14,'+Nom',[3])),
	cf(1,phi(14,var(3))),
	cf(1,subtree(12,'N_SUFF_BASE',-,9)),
	cf(1,phi(12,var(3))),
	cf(1,terminal(9,'+Sg',[3])),
	cf(1,phi(9,var(3))),
	cf(1,subtree(7,'N_SUFF_BASE',-,5)),
	cf(1,phi(7,var(3))),
	cf(1,terminal(5,'+Full',[3])),
	cf(1,phi(5,var(3))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(8))),
	cf(1,terminal(1,'abramsis',[1])),
	cf(1,phi(1,var(8))),
	cf(1,semform_data(0,2,1,9)),
	cf(1,semform_data(3,18,10,15)),
	cf(1,semform_data(5,38,17,22)),
	cf(1,fspan(var(0),1,24)),
	cf(1,fspan(var(3),1,16)),
	cf(1,fspan(var(8),1,9)),
	cf(1,surfaceform(1,'abramsis',1,9)),
	cf(1,surfaceform(3,'suraTi',10,16)),
	cf(1,surfaceform(23,'movida',17,23)),
	cf(1,surfaceform(42,'.',23,24))
	]).

