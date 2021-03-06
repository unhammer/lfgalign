% -*- coding: iso-8859-1 -*-

fstructure('qoveli kata qePs.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('2+2 solutions, 0.07 CPU seconds, 113 subtrees unified'),
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
	select(A1, 1),
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),var(17))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(A2,eq(attr(var(0),'OBJth'),var(15))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(18))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(A1,eq(var(17),semform('*-qePa',6,[var(3)],[]))),
	cf(A2,eq(var(17),semform('*-qePa',6,[var(3),var(15)],[]))),
	cf(1,eq(attr(var(3),'PRED'),semform('kata',4,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'NTYPE'),var(6))),
	cf(1,eq(attr(var(3),'SPEC'),var(7))),
	cf(1,eq(attr(var(3),'CASE'),'nom')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(3),'REF'),'+')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(4),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(6),'NSYN'),'common')),
	cf(1,eq(attr(var(7),'QUANT'),var(8))),
	cf(1,eq(attr(var(8),'PRED'),semform('qoveli',0,[],[]))),
	cf(1,eq(attr(var(8),'CHECK'),var(9))),
	cf(1,eq(attr(var(8),'CASE'),'nom')),
	cf(1,eq(attr(var(8),'QUANT-TYPE'),'universal')),
	cf(1,eq(attr(var(9),'_CASE-TYPE'),'reduced')),
	cf(A2,eq(attr(var(15),'PRED'),semform('pro',10,[],[]))),
	cf(A2,eq(attr(var(15),'CHECK'),var(16))),
	cf(A2,eq(attr(var(15),'CASE'),'dat')),
	cf(A2,eq(attr(var(15),'PERS'),'3')),
	cf(A2,eq(attr(var(16),'_PRODROP'),'+')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(10))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'pres')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(10),'_AGR'),var(11))),
	cf(1,eq(attr(var(10),'_CLASS'),var(13))),
	cf(1,eq(attr(var(10),'_LEXID'),var(14))),
	cf(1,eq(attr(var(10),'_PERF-PV'),'*')),
	cf(1,eq(attr(var(10),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(11),'_OBJ'),var(12))),
	cf(A2,eq(var(12),var(15))),
	cf(A1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(A1,eq(var(13),'MV')),
	cf(A2,eq(var(13),'RM1')),
	cf(A1,eq(var(14),'V2746-3-7')),
	cf(A2,eq(var(14),'V2746-10-11')),
	cf(1,eq(attr(var(18),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(18),'MOOD'),'indicative')),
	cf(1,eq(attr(var(18),'TENSE'),'pres')),
	cf(A2,eq(proj(var(33),'o::'),var(34))),
	cf(A2,in_set('PV',var(34))),
	cf(A1,eq(proj(var(35),'o::'),var(36))),
	cf(A1,in_set('PV',var(36)))
	],
	% C-Structure:
	[
	cf(1,subtree(1049,'ROOT',1046,73)),
	cf(1,phi(1049,var(0))),
	cf(1,subtree(1046,'ROOT',-,994)),
	cf(1,phi(1046,var(0))),
	cf(1,subtree(994,'IPfoc[main,-]',974,907)),
	cf(1,phi(994,var(0))),
	cf(1,subtree(974,'IPfoc[main,-]',-,761)),
	cf(1,phi(974,var(0))),
	cf(1,subtree(907,'Ibar[main,-]',-,804)),
	cf(1,phi(907,var(0))),
	cf(1,subtree(804,'I[main,-]',-,801)),
	cf(1,phi(804,var(0))),
	cf(1,subtree(801,'V',800,48)),
	cf(1,phi(801,var(0))),
	cf(1,subtree(800,'V',799,50)),
	cf(1,phi(800,var(0))),
	cf(1,subtree(799,'V',798,52)),
	cf(1,phi(799,var(0))),
	cf(1,subtree(798,'V',797,54)),
	cf(1,phi(798,var(0))),
	cf(1,subtree(797,'V',796,56)),
	cf(1,phi(797,var(0))),
	cf(1,subtree(796,'V',794,58)),
	cf(1,phi(796,var(0))),
	cf(1,subtree(794,'V',-,62)),
	cf(1,phi(794,var(0))),
	cf(1,subtree(761,'QuantP',185,639)),
	cf(1,phi(761,var(3))),
	cf(1,subtree(639,'NP',-,634)),
	cf(1,phi(639,var(3))),
	cf(1,subtree(634,'N',482,36)),
	cf(1,phi(634,var(3))),
	cf(1,subtree(482,'N',480,39)),
	cf(1,phi(482,var(3))),
	cf(1,subtree(480,'N',478,40)),
	cf(1,phi(480,var(3))),
	cf(1,subtree(478,'N',-,42)),
	cf(1,phi(478,var(3))),
	cf(1,subtree(330,'ALLQ',179,25)),
	cf(1,phi(330,var(8))),
	cf(1,subtree(185,'QuantP',-,183)),
	cf(1,phi(185,var(3))),
	cf(1,subtree(183,'ALLQP',-,181)),
	cf(1,phi(183,var(8))),
	cf(1,subtree(181,'ALLQ',330,29)),
	cf(1,phi(181,var(8))),
	cf(1,subtree(179,'ALLQ',178,10)),
	cf(1,phi(179,var(8))),
	cf(1,subtree(178,'ALLQ',-,12)),
	cf(1,phi(178,var(8))),
	cf(1,subtree(73,'PERIOD',-,67)),
	cf(1,phi(73,var(0))),
	cf(1,terminal(67,'.',[67])),
	cf(1,phi(67,var(0))),
	cf(A1,terminal(66,'0-*-qePa-2746-3-7',[47])),
	cf(A1,phi(66,var(0))),
	cf(A1,cproj(66,var(35))),
	cf(A1,subtree(62,'V_BASE',-,66)),
	cf(A1,phi(62,var(0))),
	cf(A2,subtree(62,'V_BASE',-,61)),
	cf(A2,phi(62,var(0))),
	cf(A2,terminal(61,'0-*-qePa-2746-10-11',[47])),
	cf(A2,phi(61,var(0))),
	cf(A2,cproj(61,var(33))),
	cf(1,terminal(59,'+V',[47])),
	cf(1,phi(59,var(0))),
	cf(1,subtree(58,'V_SUFF_BASE',-,59)),
	cf(1,phi(58,var(0))),
	cf(1,terminal(57,'+Unerg',[47])),
	cf(1,phi(57,var(0))),
	cf(1,subtree(56,'V_SUFF_BASE',-,57)),
	cf(1,phi(56,var(0))),
	cf(1,terminal(55,'+Base',[47])),
	cf(1,phi(55,var(0))),
	cf(1,subtree(54,'V_SUFF_BASE',-,55)),
	cf(1,phi(54,var(0))),
	cf(1,terminal(53,'+Pres',[47])),
	cf(1,phi(53,var(0))),
	cf(1,subtree(52,'V_SUFF_BASE',-,53)),
	cf(1,phi(52,var(0))),
	cf(1,terminal(51,'+Subj3Sg',[47])),
	cf(1,phi(51,var(0))),
	cf(1,subtree(50,'V_SUFF_BASE',-,51)),
	cf(1,phi(50,var(0))),
	cf(1,terminal(49,'+Obj3',[47])),
	cf(1,phi(49,var(0))),
	cf(1,subtree(48,'V_SUFF_BASE',-,49)),
	cf(1,phi(48,var(0))),
	cf(1,terminal(43,'kata',[31])),
	cf(1,phi(43,var(3))),
	cf(1,subtree(42,'N_BASE',-,43)),
	cf(1,phi(42,var(3))),
	cf(1,terminal(41,'+N',[31])),
	cf(1,phi(41,var(3))),
	cf(1,subtree(40,'N_SUFF_BASE',-,41)),
	cf(1,phi(40,var(3))),
	cf(1,subtree(39,'N_SUFF_BASE',-,38)),
	cf(1,phi(39,var(3))),
	cf(1,terminal(38,'+Nom',[31])),
	cf(1,phi(38,var(3))),
	cf(1,subtree(36,'N_SUFF_BASE',-,33)),
	cf(1,phi(36,var(3))),
	cf(1,terminal(33,'+Sg',[31])),
	cf(1,phi(33,var(3))),
	cf(1,subtree(29,'N_SUFF_BASE',-,27)),
	cf(1,phi(29,var(8))),
	cf(1,terminal(27,'+Reduced',[1])),
	cf(1,phi(27,var(8))),
	cf(1,subtree(25,'N_SUFF_BASE',-,24)),
	cf(1,phi(25,var(8))),
	cf(1,terminal(24,'+Nom',[1])),
	cf(1,phi(24,var(8))),
	cf(1,terminal(13,'qoveli',[1])),
	cf(1,phi(13,var(8))),
	cf(1,subtree(12,'ALLQ_BASE',-,13)),
	cf(1,phi(12,var(8))),
	cf(1,terminal(11,'+ALLQ',[1])),
	cf(1,phi(11,var(8))),
	cf(1,subtree(10,'ALLQ_SUFF_BASE',-,11)),
	cf(1,phi(10,var(8))),
	cf(1,semform_data(0,12,1,6)),
	cf(1,semform_data(4,42,8,11)),
	cf(A1,semform_data(6,62,13,16)),
	cf(A2,semform_data(6,62,13,16)),
	cf(A2,semform_data(10,62,13,16)),
	cf(1,fspan(var(0),1,18)),
	cf(1,fspan(var(3),1,12)),
	cf(1,fspan(var(8),1,7)),
	cf(1,surfaceform(1,'qoveli',1,7)),
	cf(1,surfaceform(31,'kata',8,12)),
	cf(1,surfaceform(47,'qePs',13,17)),
	cf(1,surfaceform(67,'.',17,18))
	]).

